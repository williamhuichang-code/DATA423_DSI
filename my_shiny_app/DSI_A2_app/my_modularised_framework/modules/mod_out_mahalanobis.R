# =================================================================================
# mod_out_mahalanobis.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

out_mahalanobis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd;
               min-height: 100vh; padding-left: 20px;",
      div(
        style = "font-size:13px; color:#343a40; background:white; padding:10px;
                 border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;
                 box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Mahalanobis Distance</b><br><br>
              Multivariate outlier detection. Flags observations far from the
              centroid of the distribution using D² vs a χ² threshold.<br><br>
              Internally applies: NZV removal, linear combination removal,
              Yeo-Johnson transform, then computes D².")
      ),
      hr(),
      selectInput(ns("id_col"), "ID / label column:", choices = NULL),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns (numeric only):",
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = "Defaults to Predictor roles")),
      hr(),
      sliderInput(ns("threshold_p"), "Chi-squared threshold (p):",
                  min = 0.90, max = 0.999, value = 0.999, step = 0.001, width = "100%"),
      helpText("Higher p = stricter, fewer outliers flagged.")
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("plot"), height = "60vh"),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

# ── SERVER ───────────────────────────────────────────────────────────────────

out_mahalanobis_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate inputs ───────────────────────────────────────────────────────
    observe({
      df      <- get_data(); req(df)
      r       <- roles()
      id_col  <- names(r)[r == "obs_id"]
      pred_cols <- names(r)[r == "predictor"]
      num_pred  <- pred_cols[sapply(pred_cols, function(v)
        v %in% names(df) && is.numeric(df[[v]]) && !grepl("_shadow$", v))]
      
      updateSelectInput(session, "id_col",
                        choices  = names(df),
                        selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices  = names(df),
                           selected = num_pred, server = TRUE)
    })
    
    # ── Core computation ──────────────────────────────────────────────────────
    result <- reactive({
      req(input$id_col, input$pred_cols)
      df       <- get_data()
      num_cols <- input$pred_cols[sapply(input$pred_cols, function(v)
        v %in% names(df) && is.numeric(df[[v]]))]
      if (length(num_cols) < 2) return(NULL)
      
      df_num    <- df[, num_cols, drop = FALSE]
      orig_rows <- which(complete.cases(df_num))
      df_num    <- df_num[orig_rows, , drop = FALSE]
      if (nrow(df_num) < 3) return(NULL)
      
      processed <- tryCatch({
        library(recipes)
        recipe(~ ., data = df_num) |>
          step_nzv(all_predictors()) |>
          step_lincomb(all_numeric_predictors()) |>
          step_YeoJohnson(all_numeric_predictors()) |>
          prep(training = df_num) |>
          bake(new_data = NULL)
      }, error = function(e) NULL)
      
      if (is.null(processed) || ncol(processed) < 2) return(NULL)
      
      id_labels <- as.character(df[[input$id_col]])[orig_rows]
      Covar     <- var(processed)
      Means     <- colMeans(processed)
      md2       <- mahalanobis(x = processed, center = Means, cov = Covar)
      threshold <- qchisq(p = input$threshold_p, df = ncol(processed))
      is_out    <- md2 > threshold
      
      list(md2 = md2, threshold = threshold, is_out = is_out,
           id_labels = id_labels, orig_rows = orig_rows,
           n_processed = ncol(processed), n_rows = nrow(df_num))
    })
    
    # ── Plot ──────────────────────────────────────────────────────────────────
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Need ≥ 2 numeric predictors with complete cases.",
                            colour="#C41E3A", size=5) + ggplot2::theme_void()
        return()
      }
      md2       <- res$md2
      threshold <- res$threshold
      n_out     <- sum(res$is_out)
      id        <- seq_along(md2) / length(md2)
      label     <- ifelse(res$is_out, res$id_labels, NA_character_)
      Obs       <- ifelse(res$is_out, "outlier", "non-outlier")
      
      ggplot2::ggplot(data.frame(md2, id, label, Obs),
                      ggplot2::aes(y=md2, x=id)) +
        ggplot2::geom_point(ggplot2::aes(colour=Obs), size=2) +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 size=3.2, na.rm=TRUE) +
        ggplot2::scale_colour_manual(
          values = c("non-outlier"="#4a80d4", "outlier"="#C41E3A")) +
        ggplot2::geom_hline(yintercept=threshold, linetype="dashed") +
        ggplot2::scale_x_continuous(breaks=c(0,.5,1), labels=c("0%","50%","100%")) +
        ggplot2::annotate("text", x=0.01, y=threshold*1.05,
                          label=sprintf("χ²=%.1f  (%d outliers)", threshold, n_out),
                          hjust=0, size=3.5) +
        ggplot2::labs(
          title = paste0("Mahalanobis D² | p=", input$threshold_p,
                         " | χ²(", res$n_processed, ")=", round(threshold,1),
                         " | n=", res$n_rows, " complete rows"),
          y="Mahalanobis D²", x="Complete observations") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=12, face="bold", hjust=0.5),
                       legend.title=ggplot2::element_text(face="bold"))
    })
    
    # ── Outlier table ─────────────────────────────────────────────────────────
    output$table <- DT::renderDataTable({
      res <- result(); req(res)
      df      <- get_data()
      raw_df  <- get_raw()
      out_idx <- res$orig_rows[res$is_out]
      if (length(out_idx) == 0) return(data.frame(Message="No outliers flagged."))
      raw_df[out_idx, , drop=FALSE] |>
        DT::datatable(options=list(pageLength=10, dom="tip", scrollX=TRUE),
                      rownames=FALSE)
    })
    
    # ── Return flagged row indices ────────────────────────────────────────────
    return(list(
      flagged = reactive({
        res <- result()
        if (is.null(res)) return(integer(0))
        res$orig_rows[res$is_out]
      })
    ))
  })
}