# =================================================================================
# mod_out_cooks.R
# =================================================================================

out_cooks_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
               min-height:100vh; padding-left:20px;",
      div(
        style = "font-size:13px; color:#343a40; background:white; padding:10px;
                 border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;",
        icon("info-circle", style="color:#0d6efd;"),
        HTML("&nbsp; <b>Cook's Distance</b><br><br>
              Measures influence of each observation on a linear regression fit.
              Flags points with both high leverage and large residuals.<br><br>
              Internally applies: NZV, linear combination, and correlation
              removal, then fits a gaussian GLM.")
      ),
      hr(),
      selectInput(ns("id_col"),       "ID / label column:",     choices=NULL),
      selectInput(ns("response_col"), "Response variable (y):", choices=NULL),
      helpText("⚠ Do not select ID, split, or non-numeric columns as response or predictors — 
               the plot will not render."),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns:",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      hr(),
      sliderInput(ns("threshold_mult"), "Threshold multiplier (k × mean D_c):",
                  min=1, max=10, value=4, step=0.5, width="100%"),
      helpText("Standard rule: 4 × mean. Higher = fewer outliers flagged.")
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("plot"), height="60vh"),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_cooks_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df      <- get_data(); req(df)
      r       <- roles()
      id_col  <- names(r)[r == "obs_id"]
      resp    <- names(r)[r == "outcome"]
      preds   <- names(r)[r == "predictor"]
      num_pred <- preds[sapply(preds, function(v)
        v %in% names(df) && is.numeric(df[[v]]) && !grepl("_shadow$", v))]
      
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectInput(session, "response_col",
                        choices=names(df),
                        selected=if(length(resp)>0) resp[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=num_pred, server=TRUE)
    })
    
    processed <- reactive({
      req(input$response_col, input$pred_cols)
      df       <- get_data()
      response <- input$response_col
      preds    <- setdiff(input$pred_cols, response)
      if (length(preds) < 1) return(NULL)
      
      cols     <- intersect(union(preds, response), names(df))
      df_sub   <- df[, cols, drop=FALSE]
      mask     <- complete.cases(df_sub)
      orig     <- which(mask)
      df_comp  <- df_sub[mask, , drop=FALSE]
      if (nrow(df_comp) < 3) return(NULL)
      
      tryCatch({
        library(recipes)
        rec <- recipe(as.formula(paste(response, "~ .")), data=df_comp) |>
          step_nzv(all_predictors()) |>
          step_lincomb(all_numeric_predictors()) |>
          step_corr(all_numeric_predictors(), threshold=0.9) |>
          prep(training=df_comp)
        baked <- bake(rec, new_data=NULL)
        list(baked=baked, orig_rows=orig)
      }, error=function(e) NULL)
    })
    
    result <- reactive({
      res <- processed(); req(res)
      baked    <- res$baked
      response <- input$response_col
      if (ncol(baked) < 2 || nrow(baked) < 3) return(NULL)
      tryCatch({
        lmod      <- glm(as.formula(paste(response,"~ .")), data=baked, family=gaussian)
        dc        <- cooks.distance(lmod)
        threshold <- input$threshold_mult * mean(dc, na.rm=TRUE)
        is_out    <- dc > threshold
        list(dc=dc, threshold=threshold, is_out=is_out,
             orig_rows=res$orig_rows, n_rows=nrow(baked))
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Could not fit model. Check selections.",
                            colour="#C41E3A", size=5) + ggplot2::theme_void()
        return()
      }
      df        <- get_data()
      dc        <- res$dc
      threshold <- res$threshold
      n_out     <- sum(res$is_out)
      id        <- seq_along(dc)/length(dc)
      id_labels <- as.character(df[[input$id_col]])[res$orig_rows]
      label     <- ifelse(res$is_out, id_labels, NA_character_)
      Obs       <- ifelse(res$is_out, "outlier", "non-outlier")
      
      ggplot2::ggplot(data.frame(dc, id, label, Obs), ggplot2::aes(y=dc, x=id)) +
        ggplot2::geom_point(ggplot2::aes(colour=Obs), size=2) +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 size=3.2, na.rm=TRUE) +
        ggplot2::scale_colour_manual(
          values=c("non-outlier"="#4a80d4","outlier"="#C41E3A")) +
        ggplot2::geom_hline(yintercept=threshold, linetype="dashed") +
        ggplot2::scale_x_continuous(breaks=c(0,.5,1), labels=c("0%","50%","100%")) +
        ggplot2::annotate("text", x=0.01, y=threshold*1.05,
                          label=sprintf("threshold=%.4f  (%d outliers)", threshold, n_out),
                          hjust=0, size=3.5) +
        ggplot2::labs(
          title=paste0("Cook's Distance | threshold=", input$threshold_mult,
                       " × mean(D_c)=", round(threshold,4),
                       " | n=", res$n_rows, " complete rows"),
          y="Cook's distance", x="Complete observations") +
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