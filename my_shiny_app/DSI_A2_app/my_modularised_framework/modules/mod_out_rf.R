# =================================================================================
# mod_out_rf.R
# =================================================================================

out_rf_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width=3,
      style="background-color:#e8f0fe; border-left:2px solid #a8c0fd;
             min-height:100vh; padding-left:20px;",
      div(
        style="font-size:13px; color:#343a40; background:white; padding:10px;
               border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;",
        icon("info-circle", style="color:#0d6efd;"),
        HTML("&nbsp; <b>Random Forest Residuals</b><br><br>
              Trains a Random Forest to predict the response, then flags
              observations whose residuals are outliers (IQR method).<br><br>
              Not sensitive to scaling. Does NOT tolerate missing values.<br><br>
              Uses <code>randomForest::randomForest()</code>.")
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
      helpText("⚠ Do not select ID, split columns."),
      hr(),
      numericInput(ns("iqr_k"), "IQR multiplier (k):",
                   value=2.0, min=0.5, step=0.5),
      helpText("Residuals beyond k × IQR are flagged.")
    ),
    mainPanel(
      width=9,
      plotOutput(ns("plot"), height="40vh"),
      hr(),
      verbatimTextOutput(ns("summary")),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_rf_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      resp   <- names(r)[r == "outcome"]
      preds  <- names(r)[r == "predictor"]
      rf_pred <- preds[!grepl("_shadow$", preds) & preds %in% names(df)]
      
      updateSelectInput(session, "id_col",
                        choices=names(df),
                        selected=if(length(id_col)>0) id_col[1] else names(df)[1])
      updateSelectInput(session, "response_col",
                        choices=names(df),
                        selected=if(length(resp)>0) resp[1] else names(df)[1])
      updateSelectizeInput(session, "pred_cols",
                           choices=names(df), selected=rf_pred, server=TRUE)
    })
    
    result <- reactive({
      req(input$id_col, input$response_col, input$pred_cols)
      df       <- get_data()
      response <- input$response_col
      preds    <- setdiff(input$pred_cols, response)
      if (length(preds) < 1 || !is.numeric(df[[response]])) return(NULL)
      
      cols     <- intersect(union(preds, response), names(df))
      df_sub   <- df[, cols, drop=FALSE]
      mask     <- complete.cases(df_sub)
      orig     <- which(mask)
      df_comp  <- df_sub[mask,,drop=FALSE]
      if (nrow(df_comp) < 10) return(NULL)
      
      tryCatch({
        library(recipes)
        num_pred <- preds[sapply(preds, function(v)
          v %in% names(df_comp) && is.numeric(df_comp[[v]]))]
        if (length(num_pred) > 0) {
          rec <- recipe(as.formula(paste(response,"~.")), data=df_comp) |>
            step_nzv(all_predictors()) |>
            step_lincomb(all_numeric_predictors()) |>
            prep(training=df_comp)
          df_model <- bake(rec, new_data=NULL)
        } else {
          df_model <- df_comp
        }
        if (ncol(df_model) < 2) return(NULL)
        
        gen_model <- randomForest::randomForest(
          as.formula(paste(response,"~.")), data=df_model)
        resids <- df_comp[[response]] - predict(gen_model, newdata=df_comp)
        names(resids) <- as.character(df[[input$id_col]])[orig]
        
        k      <- input$iqr_k
        limits <- boxplot.stats(x=resids, coef=k)$stats
        is_out <- resids < limits[1] | resids > limits[5]
        
        list(resids=resids, is_out=is_out, limits=limits,
             orig_rows=orig, k=k)
      }, error=function(e) NULL)
    })
    
    output$plot <- renderPlot({
      res <- result()
      if (is.null(res)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Could not fit RF.\nCheck selections, ensure no NAs remain\nand randomForest is installed.",
                            colour="#C41E3A", size=5) + ggplot2::theme_void()
        return()
      }
      resids <- res$resids
      k      <- res$k
      label  <- ifelse(res$is_out, names(resids), NA_character_)
      plot_df <- data.frame(
        resids=resids,
        Obs=ifelse(res$is_out,"outlier","non-outlier"),
        label=label)
      
      ggplot2::ggplot(plot_df, ggplot2::aes(x=resids, y=0)) +
        ggplot2::geom_boxplot(coef=k, outlier.colour="#C41E3A") +
        ggrepel::geom_text_repel(ggplot2::aes(label=label), max.overlaps=50,
                                 na.rm=TRUE, size=3.2) +
        ggplot2::labs(
          title=paste0("RF Residuals | IQR k=", k,
                       " | n=", length(resids), " complete rows"),
          x="Residuals") +
        ggplot2::theme_minimal(base_size=13) +
        ggplot2::theme(plot.title=ggplot2::element_text(size=12, face="bold", hjust=0.5),
                       axis.title.y=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),
                       axis.ticks.y=ggplot2::element_blank())
    })
    
    output$summary <- renderPrint({
      res <- result()
      if (is.null(res)) { cat("No result.\n"); return(invisible(NULL)) }
      resids <- res$resids
      n_out  <- sum(res$is_out)
      cat(sprintf("RF Residual Outliers | k=%.1f\n", res$k))
      cat(sprintf("SD of residuals : %.4f\n", sd(resids)))
      cat(sprintf("Whisker limits  : [%.4f, %.4f]\n", res$limits[1], res$limits[5]))
      cat(sprintf("Complete rows   : %d\n", length(resids)))
      cat(sprintf("Outliers        : %d\n", n_out))
      cat("──────────────────────────────\n")
      if (n_out==0) cat("No outliers flagged.\n")
      else { cat("Flagged IDs:\n"); cat(paste(names(resids)[res$is_out], collapse="  "), "\n") }
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