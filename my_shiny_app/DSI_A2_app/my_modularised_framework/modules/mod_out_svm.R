# =================================================================================
# mod_out_svm.R
# =================================================================================

out_svm_ui <- function(id) {
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
        HTML("&nbsp; <b>One-Class SVM</b><br><br>
              Novelty/outlier detection using a one-class SVM boundary.
              Observations outside the learned boundary are flagged.<br><br>
              <b>nu</b>: upper bound on fraction of outliers.<br>
              Uses <code>e1071::svm()</code>.")
      ),
      hr(),
      selectInput(ns("id_col"), "ID / label column:", choices=NULL),
      hr(),
      selectizeInput(ns("pred_cols"), "Predictor columns (numeric only):",
                     choices=NULL, multiple=TRUE,
                     options=list(placeholder="Defaults to Predictor roles")),
      hr(),
      sliderInput(ns("nu"), "nu (outlier fraction):",
                  min=0.001, max=0.5, value=0.05, step=0.005, width="100%"),
      helpText("~nu × n observations will be flagged."),
      hr(),
      selectInput(ns("kernel"), "Kernel:",
                  choices=c("linear","polynomial","radial","sigmoid"),
                  selected="linear"),
      hr(),
      checkboxInput(ns("scale"), "Scale inside SVM", value=FALSE),
      helpText("Set TRUE if predictors are not already scaled.")
    ),
    mainPanel(
      width=9,
      verbatimTextOutput(ns("summary")),
      hr(),
      h5("Outlier Rows (raw data)"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

out_svm_server <- function(id, get_data, get_raw, roles) {
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
      if (length(num_cols) < 1) return(NULL)
      
      df_sub <- df[, num_cols, drop=FALSE]
      mask   <- complete.cases(df_sub)
      orig   <- which(mask)
      df_comp <- df_sub[mask,,drop=FALSE]
      if (nrow(df_comp) < 5) return(NULL)
      
      tryCatch({
        library(recipes)
        baked <- recipe(~., data=df_comp) |>
          step_nzv(all_predictors()) |>
          step_lincomb(all_numeric_predictors()) |>
          prep(training=df_comp) |>
          bake(new_data=NULL)
        if (ncol(baked) < 1) return(NULL)
        mat   <- as.matrix(baked)
        model <- e1071::svm(mat, y=NULL, type="one-classification",
                            nu=input$nu, scale=isTRUE(input$scale),
                            kernel=input$kernel)
        is_out <- !predict(model, mat)
        list(is_out=is_out, orig_rows=orig, n_total=nrow(mat))
      }, error=function(e) NULL)
    })
    
    output$summary <- renderPrint({
      res <- result()
      if (is.null(res)) {
        cat("Could not fit SVM.\nCheck selections and ensure e1071 is installed.\n")
        return(invisible(NULL))
      }
      df        <- get_data()
      is_out    <- res$is_out
      orig_rows <- res$orig_rows
      n_out     <- sum(is_out)
      out_ids   <- as.character(df[[input$id_col]])[orig_rows[is_out]]
      
      cat(sprintf("One-Class SVM | kernel=%s | nu=%.4f | scale=%s\n",
                  input$kernel, input$nu, if(isTRUE(input$scale)) "TRUE" else "FALSE"))
      cat(sprintf("Complete rows : %d\n", res$n_total))
      cat(sprintf("Outliers      : %d  (%.1f%%)\n", n_out, 100*n_out/res$n_total))
      cat("──────────────────────────────\n")
      if (n_out==0) cat("No outliers flagged.\n")
      else { cat("Flagged IDs:\n"); cat(paste(out_ids, collapse="  "), "\n") }
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