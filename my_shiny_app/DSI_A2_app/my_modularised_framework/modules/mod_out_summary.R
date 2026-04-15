# =================================================================================
# mod_out_summary.R
# =================================================================================

out_summary_ui <- function(id) {
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
        HTML("&nbsp; <b>Outlier Summary</b><br><br>
              Aggregates flags from all active detection methods into a
              cumulative novelty score per observation.<br><br>
              Taller bars = flagged by more methods = more likely a true outlier.")
      ),
      hr(),
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      numericInput(ns("min_count"), "Min flag count to display:",
                   value=2, min=1, step=1),
      helpText("Only show observations flagged by at least this many methods.")
    ),
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="70vh"),
      hr(),
      h5("Observations Flagged by Multiple Methods"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_summary_server <- function(id, get_data, get_raw, roles,
                               flagged_mah, flagged_cooks, flagged_lof,
                               flagged_svm, flagged_rf, flagged_iforest) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
    })
    
    # ── Aggregate flags ───────────────────────────────────────────────────────
    long_df <- reactive({
      req(input$id_col)
      df <- get_data()
      id_labels <- as.character(df[[input$id_col]])
      
      method_flags <- list(
        mahalanobis = flagged_mah(),
        cooks       = flagged_cooks(),
        lof         = flagged_lof(),
        svm         = flagged_svm(),
        rf          = flagged_rf(),
        iforest     = flagged_iforest()
      )
      
      # build long-format data frame
      rows <- lapply(names(method_flags), function(method) {
        idx <- method_flags[[method]]
        if (length(idx) == 0) return(NULL)
        data.frame(id=id_labels[idx], Type=method, stringsAsFactors=FALSE)
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(NULL)
      do.call(rbind, rows)
    })
    
    method_colors <- c(
      mahalanobis = "#2ab7ca",
      cooks       = "#fe4a49",
      lof         = "#27ae60",
      svm         = "#e056fd",
      rf          = "#4a80d4",
      iforest     = "#f39c12"
    )
    
    output$plot <- renderPlot({
      df_long <- long_df()
      if (is.null(df_long) || nrow(df_long)==0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="No outliers flagged yet.\nRun detection methods first.",
                            colour="#495057", size=5) + ggplot2::theme_void()
        return()
      }
      counts   <- table(df_long$id)
      keep_ids <- names(counts[counts >= input$min_count])
      plot_df  <- df_long[df_long$id %in% keep_ids, ]
      
      if (nrow(plot_df)==0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label=paste0("No observations flagged by ≥ ",
                                         input$min_count, " methods."),
                            colour="#495057", size=5) + ggplot2::theme_void()
        return()
      }
      
      id_order <- names(sort(table(plot_df$id), decreasing=TRUE))
      plot_df$id <- factor(plot_df$id, levels=id_order)
      
      ggplot2::ggplot(plot_df, ggplot2::aes(x=id, fill=Type)) +
        ggplot2::geom_bar(position="stack") +
        ggplot2::scale_fill_manual(values=method_colors) +
        ggplot2::labs(
          title="Observation Novelty — Cumulative Outlier Flags",
          x="ID", y="Flag count", fill="Method") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(
          axis.text.x=ggplot2::element_text(angle=45, hjust=1, size=11),
          plot.title=ggplot2::element_text(size=15, face="bold", hjust=0.5),
          legend.title=ggplot2::element_text(face="bold"))
    })
    
    output$table <- DT::renderDataTable({
      df_long <- long_df()
      if (is.null(df_long) || nrow(df_long)==0)
        return(data.frame(Message="No outliers flagged yet."))
      
      counts   <- table(df_long$id)
      keep_ids <- names(counts[counts >= input$min_count])
      plot_df  <- df_long[df_long$id %in% keep_ids, ]
      if (nrow(plot_df)==0)
        return(data.frame(Message="No observations meet threshold."))
      
      summary_df <- as.data.frame.matrix(table(plot_df$id, plot_df$Type))
      summary_df$id          <- rownames(summary_df)
      summary_df$total_flags <- rowSums(summary_df[, setdiff(names(summary_df),"id"), drop=FALSE])
      summary_df <- summary_df[order(-summary_df$total_flags), ]
      rownames(summary_df) <- NULL
      method_cols <- setdiff(names(summary_df), c("id","total_flags"))
      summary_df  <- summary_df[, c("id","total_flags", method_cols)]
      
      DT::datatable(summary_df,
                    options=list(pageLength=15, scrollX=TRUE),
                    rownames=FALSE)
    })
  })
}