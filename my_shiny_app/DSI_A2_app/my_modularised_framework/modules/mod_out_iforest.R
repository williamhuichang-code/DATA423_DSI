# =================================================================================
# mod_out_iforest.R
# =================================================================================

out_iforest_ui <- function(id) {
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
        HTML("&nbsp; <b>Isolation Forest</b><br><br>
              Tree-based outlier detection. Scores between 0 and 1 —
              higher scores = more outlier-like.<br><br>
              Tolerates missing values. Works on numeric and nominal columns.<br><br>
              Uses <code>isotree::isolation.forest()</code>.")
      ),
      hr(),
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns:",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      hr(),
      sliderInput(ns("threshold"), "Outlier score threshold:",
                  min=0.50, max=0.95, value=0.55, step=0.01, width="100%"),
      helpText("Scores above threshold are flagged as outliers.")
    ),
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="60vh"),
      hr(),
      verbatimTextOutput(ns("summary")),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_iforest_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      preds  <- names(r)[r == "predictor"]
      iforest_pred <- preds[!grepl("_shadow$", preds) & preds %in% names(df)]
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=iforest_pred, server=TRUE)
    })
    
    result <- reactive({
      req(input$id_col, input$pred_cols)
      df    <- get_data()
      preds <- intersect(input$pred_cols, names(df))
      if (length(preds) < 1) return(NULL)
      
      df_sub <- df[, preds, drop=FALSE]
      if (nrow(df_sub) < 5) return(NULL)
      
      tryCatch({
        itree  <- isotree::isolation.forest(df_sub)
        scores <- predict(itree, newdata=df_sub)
        names(scores) <- as.character(df[[input$id_col]])
        is_out <- scores > input$threshold
        list(scores=scores, is_out=is_out,
             orig_rows=seq_len(nrow(df)), n=nrow(df))
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Could not fit Isolation Forest.\nEnsure isotree is installed.",
                            colour="#C41E3A", size=5) + ggplot2::theme_void()
        return()
      }
      scores    <- res$scores
      threshold <- input$threshold
      n_out     <- sum(res$is_out)
      n         <- res$n
      id        <- seq_len(n)/n
      label     <- ifelse(res$is_out, names(scores), NA_character_)
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
        ggplot2::annotate("text", x=0.01, y=threshold*1.02,
                          label=sprintf("threshold=%.2f  (%d outliers)", threshold, n_out),
                          hjust=0, size=3.5) +
        ggplot2::labs(
          title=paste0("Isolation Forest | threshold=", threshold,
                       " | n=", n, " observations"),
          y="Iso Score", x="Observations") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=12, face="bold", hjust=0.5))
    })
    
    output$summary <- renderPrint({
      res <- result()
      if (is.null(res)) { cat("No result.\n"); return(invisible(NULL)) }
      scores    <- res$scores
      threshold <- input$threshold
      is_out    <- res$is_out
      n_out     <- sum(is_out)
      cat(sprintf("Isolation Forest | threshold=%.2f\n", threshold))
      cat(sprintf("Observations : %d\n", res$n))
      cat(sprintf("Outliers     : %d  (%.1f%%)\n", n_out, 100*n_out/res$n))
      cat("──────────────────────────────\n")
      if (n_out==0) cat("No outliers flagged.\n")
      else { cat("Flagged IDs:\n"); cat(paste(names(scores)[is_out], collapse="  "), "\n") }
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