# =================================================================================
# mod_out_lof.R
# =================================================================================

out_lof_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width=3,
      style="background-color:#e8f0fe; border-left:2px solid #a8c0fd;
             min-height:100vh; padding-left:20px;",
      div(
        style="font-size:13px; color:#343a40; background:white; padding:10px;
               border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;",
        icon("info-circle", style="color:#0d6efd;"),
        HTML("&nbsp; <b>Local Outlier Factor (LOF)</b><br><br>
              Scores each observation by density of its neighbourhood.
              Isolated points far from any local cluster get high scores.<br><br>
              lof ≤ 1: not an outlier | lof >> 1: probable outlier<br><br>
              Uses <code>dbscan::lof()</code>.")
      ),
      hr(),
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns (numeric only):",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      hr(),
      numericInput(ns("min_pts"), "minPts (nearest neighbours):",
                   value=4, min=2, max=50, step=1),
      helpText("Range 3–6 is usually optimal."),
      hr(),
      sliderInput(ns("threshold"), "LOF threshold:",
                  min=1.0, max=5.0, value=1.5, step=0.1, width="100%"),
      helpText("Observations with LOF > threshold are flagged."),
      hr(),
      checkboxInput(ns("scale"), "Scale predictors (divide by SD)", value=TRUE)
    ),
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="60vh"),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_lof_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      preds  <- names(r)[r == "predictor"]
      num_pred <- preds[sapply(preds, function(v)
        v %in% names(df) && is.numeric(df[[v]]) && !grepl("_shadow$", v))]
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=num_pred, server=TRUE)
    })
    
    result <- reactive({
      req(input$id_col, input$pred_cols)
      df       <- get_data()
      num_cols <- input$pred_cols[sapply(input$pred_cols, function(v)
        v %in% names(df) && is.numeric(df[[v]]))]
      if (length(num_cols) < 2) return(NULL)
      
      df_sub <- df[, num_cols, drop=FALSE]
      mask   <- complete.cases(df_sub)
      orig   <- which(mask)
      df_comp <- df_sub[mask,,drop=FALSE]
      if (nrow(df_comp) < 5) return(NULL)
      
      tryCatch({
        library(recipes)
        rec <- recipe(~., data=df_comp) |>
          step_nzv(all_predictors()) |>
          step_lincomb(all_numeric_predictors())
        if (isTRUE(input$scale))
          rec <- rec |> step_scale(all_numeric_predictors())
        baked <- rec |> prep(training=df_comp) |> bake(new_data=NULL)
        if (ncol(baked) < 1) return(NULL)
        mat   <- as.matrix(baked)
        scores <- dbscan::lof(mat, minPts=input$min_pts)
        is_out <- scores > input$threshold
        list(scores=scores, is_out=is_out, orig_rows=orig, n_rows=nrow(df_comp))
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Could not compute LOF.\nEnsure dbscan is installed.",
                            colour="#C41E3A", size=5) + ggplot2::theme_void()
        return()
      }
      df        <- get_data()
      scores    <- res$scores
      threshold <- input$threshold
      n_out     <- sum(res$is_out)
      id        <- seq_along(scores)/length(scores)
      id_labels <- as.character(df[[input$id_col]])[res$orig_rows]
      label     <- ifelse(res$is_out, id_labels, NA_character_)
      Obs       <- ifelse(res$is_out, "outlier", "non-outlier")
      
      ggplot2::ggplot(data.frame(scores, id, label, Obs), ggplot2::aes(y=scores, x=id)) +
        ggplot2::geom_point(ggplot2::aes(colour=Obs), size=2) +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 size=3.2, na.rm=TRUE) +
        ggplot2::scale_colour_manual(
          values=c("non-outlier"="#4a80d4","outlier"="#C41E3A")) +
        ggplot2::geom_hline(yintercept=threshold, linetype="dashed") +
        ggplot2::scale_x_continuous(breaks=c(0,.5,1), labels=c("0%","50%","100%")) +
        ggplot2::scale_y_continuous(limits=c(0,NA)) +
        ggplot2::annotate("text", x=0.01, y=threshold*1.05,
                          label=sprintf("threshold=%.1f  (%d outliers)", threshold, n_out),
                          hjust=0, size=3.5) +
        ggplot2::labs(
          title=paste0("LOF | minPts=", input$min_pts, " | threshold=", threshold,
                       if(isTRUE(input$scale)) " | scaled" else " | unscaled",
                       " | n=", res$n_rows, " complete rows"),
          y="LOF", x="Complete observations") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=12, face="bold", hjust=0.5))
    })
    
    output$table <- DT::renderDataTable({
      res <- result(); req(res)
      raw_df  <- get_raw()
      out_idx <- res$orig_rows[res$is_out]
      if (length(out_idx)==0) return(data.frame(Message="No outliers flagged."))
      raw_df[out_idx,,drop=FALSE] |>
        DT::datatable(options=list(pageLength=10, dom="tip", scrollX=TRUE), rownames=FALSE)
    })
    
    return(list(
      flagged = reactive({
        res <- result()
        if (is.null(res)) return(integer(0))
        res$orig_rows[res$is_out]
      })
    ))
  })
}