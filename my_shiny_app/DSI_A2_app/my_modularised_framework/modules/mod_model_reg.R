# =================================================================================
# mod_model_reg.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

model_reg_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd;
               min-height: 100vh; padding-left: 20px;",
      
      # ── Tab note ──────────────────────────────────────────────────────────
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Regression</b><br><br>
              Fit the final regularised regression model using confirmed
              &alpha; and &lambda; from the Tune tab.")
      ),
      hr(),
      
      # ── Split variable ────────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom: 4px;",
        tags$label("Split variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("split_var"), label = NULL,
                    choices = c("(none)"), width = "100%")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != '(none)'", ns("split_var")),
        fluidRow(
          column(6,
                 tags$label("Train level:",
                            style = "font-size:12px; font-weight:600; color:#343a40;"),
                 selectInput(ns("train_level"), label = NULL,
                             choices = c("(none)"), width = "100%")
          ),
          column(6,
                 tags$label("Test level:",
                            style = "font-size:12px; font-weight:600; color:#343a40;"),
                 selectInput(ns("test_level"), label = NULL,
                             choices = c("(none)"), width = "100%")
          )
        )
      ),
      hr(),
      
      # ── Family ────────────────────────────────────────────────────────────
      tags$label("Response family:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("family"), label = NULL,
                   choices = c(
                     "Gaussian (continuous)" = "gaussian",
                     "Binomial (binary)"     = "binomial",
                     "Poisson (counts)"      = "poisson"
                   ),
                   selected = "gaussian"),
      hr(),
      
      # ── Evaluation metric ─────────────────────────────────────────────────
      tags$label("Evaluation metric:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectizeInput(ns("metric"), label = NULL,
                     choices  = c("MSE"  = "mse",
                                  "RMSE" = "rmse",
                                  "MAE"  = "mae",
                                  "R²"   = "r2"),
                     selected = c("mse", "rmse", "mae", "r2"),
                     multiple = TRUE,
                     options  = list(placeholder = "Select metrics..."),
                     width    = "100%"),
      hr(),
      
      # ── Buttons ───────────────────────────────────────────────────────────
      actionButton(
        ns("run"),
        label = "Run Model",
        icon  = icon("play"),
        width = "100%",
        style = "background-color:#185FA5; color:white; border:none; margin-bottom:8px;"
      ),
      actionButton(
        ns("reset"),
        label = "Reset",
        icon  = icon("rotate-left"),
        width = "100%",
        style = "margin-bottom:8px;"
      ),
      hr(),
      actionButton(
        ns("add_to_comparison"),
        label = "Add to Comparison",
        icon  = icon("table"),
        width = "100%",
        style = "background-color:#6610f2; color:white; border:none; margin-bottom:4px;"
      ),
      uiOutput(ns("add_feedback_ui"))
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        type = "tabs",
        id   = ns("main_tabs"),
        
        # ── Tab 1: Training ───────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("hammer"), " Training"),
          style = "padding-top:16px;",
          
          # confirmed params from tune
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(
              icon("circle-check", style = "color:#185FA5; margin-right:6px;"),
              "Confirmed Parameters from Tune",
              style = "font-weight:600; color:#343a40; margin-bottom:12px;"
            ),
            uiOutput(ns("confirmed_params_ui"))
          ),
          
          # model selector
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(
              icon("brain", style = "color:#185FA5; margin-right:6px;"),
              "Model",
              style = "font-weight:600; color:#343a40; margin-bottom:12px;"
            ),
            fluidRow(
              column(6,
                     tags$label("Model method:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectInput(ns("model_method"), label = NULL,
                                 choices = c("glmnet (Regularised Regression)" = "glmnet"),
                                 selected = "glmnet", width = "100%")
              ),
              column(6,
                     tags$label("Run notes (optional):",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     textInput(ns("run_notes"), label = NULL,
                               placeholder = "e.g. Lasso lambda.min, no scaling",
                               width = "100%")
              )
            )
          ),
          
          # run status
          uiOutput(ns("run_status_ui"))
        ),
        
        # ── Tab 2: Summary ────────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("list"), " Summary"),
          style = "padding-top:16px;",
          uiOutput(ns("summary_ui"))
        ),
        
        # ── Tab 3: Prediction ─────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("chart-line"), " Prediction"),
          style = "padding-top:16px;",
          
          # ── Static plot controls ─────────────────────────────────────────
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("sliders", style = "color:#185FA5; margin-right:6px;"),
                    "Plot Controls",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            fluidRow(
              column(4,
                     tags$label("Label outliers by:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectInput(ns("label_col"), label = NULL,
                                 choices  = c("None" = "none", "Row index" = "rowindex"),
                                 selected = "rowindex", width = "100%")
              ),
              column(4,
                     tags$label("Outlier threshold (IQR coef):",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     sliderInput(ns("outlier_coef"), label = NULL,
                                 min = 1.0, max = 3.0, value = 2.2, step = 0.1, width = "100%")
              )
            )
          ),
          
          uiOutput(ns("prediction_ui"))
        ),
        
        # ── Tab 4: Residual Diagnostics ───────────────────────────────────
        tabPanel(
          title = tagList(icon("stethoscope"), " Residual Diagnostics"),
          style = "padding-top:16px;",
          uiOutput(ns("residual_ui"))
        ),
        
        # ── Tab 5: Comparison ─────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("table"), " Comparison"),
          style = "padding-top:16px;",
          uiOutput(ns("comparison_ui"))
        )
        
      ) # end tabsetPanel
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

model_reg_server <- function(id, get_data, roles, get_recipe, model_tune) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Helpers ───────────────────────────────────────────────────────────────
    
    make_card <- function(label, value, color = "#185FA5") {
      div(style = paste0(
        "flex:1; min-width:130px; background:white; border-radius:10px;",
        "border:0.5px solid #dee2e6; padding:14px 18px;",
        "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
      ),
      div(style = "font-size:11px; color:#6c757d; font-weight:500;
                   text-transform:uppercase; letter-spacing:.5px;", label),
      div(style = paste0("font-size:22px; font-weight:700; color:", color, ";"), value)
      )
    }
    
    # ── Split var from roles ──────────────────────────────────────────────────
    
    observe({
      req(get_data(), roles())
      r    <- roles()
      vars <- names(get_data())
      updateSelectInput(session, "split_var",
                        choices  = c("(none)", vars),
                        selected = { v <- names(r)[r == "split"]; if (length(v)) v[1] else "(none)" })
    })
    
    observe({
      req(input$split_var, input$split_var != "(none)")
      df   <- get_data(); req(df)
      lvls <- as.character(unique(df[[input$split_var]]))
      lvls <- lvls[!is.na(lvls)]
      train_sel <- if ("Train" %in% lvls) "Train" else if ("train" %in% lvls) "train" else lvls[1]
      test_sel  <- if ("Test"  %in% lvls) "Test"  else if ("test"  %in% lvls) "test"  else
        if (length(lvls) >= 2) lvls[2] else lvls[1]
      updateSelectInput(session, "train_level", choices = c("(none)", lvls), selected = train_sel)
      updateSelectInput(session, "test_level",  choices = c("(none)", lvls), selected = test_sel)
    })
    
    # ── Confirmed params display ──────────────────────────────────────────────
    
    output$confirmed_params_ui <- renderUI({
      alpha  <- model_tune$tuned_alpha()
      lambda <- model_tune$tuned_lambda()
      
      if (is.null(alpha) || is.null(lambda)) {
        return(div(
          style = "font-size:12px; color:#856404; background:#fff3cd;
                   border-left:3px solid #ffc107; padding:8px 10px; border-radius:4px;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML(" No parameters confirmed yet. Go to <b>Tune &gt; Set Parameters</b>
                and click Confirm Parameters first.")
        ))
      }
      
      badge <- function(label, value, color) {
        div(style = paste0(
          "display:inline-block; background:", color, "22;",
          "color:", color, "; border:1px solid ", color, "55;",
          "border-radius:6px; padding:6px 14px; font-size:14px;",
          "font-weight:600; margin-right:8px;"
        ), paste0(label, " = ", value))
      }
      
      mode_label <- if (alpha == 0) "Ridge" else if (alpha == 1) "Lasso" else paste0("ElasticNet (α=", alpha, ")")
      
      div(
        badge("α", alpha,                "#185FA5"),
        badge("λ", round(lambda, 6),     "#0F6E56"),
        badge("Model", mode_label,       "#534AB7"),
        div(style = "margin-top:10px; font-size:12px; color:#6c757d;",
            icon("circle-info"), " These values were confirmed in the Tune tab. Override alpha/lambda manually if needed by going back to Tune > Set Parameters.")
      )
    })
    
    # ── State ─────────────────────────────────────────────────────────────────
    
    model_result <- reactiveVal(NULL)
    comparison   <- reactiveVal(data.frame(
      Run        = integer(),
      Model      = character(),
      Alpha      = numeric(),
      Lambda     = numeric(),
      N_Pred     = integer(),
      Test_MSE   = numeric(),
      Test_RMSE  = numeric(),
      Test_MAE   = numeric(),
      Test_R2    = numeric(),
      CV_Time    = numeric(),
      Train_Time = numeric(),
      Total_Time = numeric(),
      Notes      = character(),
      stringsAsFactors = FALSE
    ))
    
    # ── Run ───────────────────────────────────────────────────────────────────
    
    observeEvent(input$run, {
      
      rec    <- get_recipe()
      alpha  <- model_tune$tuned_alpha()
      lambda <- model_tune$tuned_lambda()
      
      if (is.null(rec)) {
        model_result(list(error = "No recipe built. Go to Pre-Processing > Recipe first."))
        return()
      }
      if (is.null(alpha) || is.null(lambda)) {
        model_result(list(error = "No parameters confirmed. Go to Tune > Set Parameters and click Confirm Parameters."))
        return()
      }
      
      df        <- get_data(); req(df)
      sv        <- input$split_var
      tl        <- input$train_level
      ts        <- input$test_level
      splits_ok <- !is.null(sv) && sv != "(none)" &&
        !is.null(tl) && tl != "(none)" &&
        !is.null(ts) && ts != "(none)"
      
      train_df <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop = FALSE] else df
      test_df  <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop = FALSE] else NULL
      
      if (nrow(train_df) == 0) {
        model_result(list(error = "No training data. Check split variable and train level."))
        return()
      }
      
      withProgress(message = "Fitting model...", value = 0.1, {
        tryCatch({
          library(caret)
          library(glmnet)
          
          incProgress(0.2, message = "Training...")
          
          train_time <- system.time({
            set.seed(2026)
            model <- caret::train(
              rec,
              data      = train_df,
              method    = "glmnet",
              family    = input$family,
              tuneGrid  = data.frame(alpha = alpha, lambda = lambda),
              trControl = trainControl(method = "none")
            )
          })
          
          incProgress(0.7, message = "Predicting on test set...")
          
          # get y col from recipe
          y_col <- model$recipe$var_info$variable[model$recipe$var_info$role == "outcome"][1]
          
          preds   <- if (!is.null(test_df)) predict(model, newdata = test_df) else NULL
          y_test  <- if (!is.null(test_df) && y_col %in% names(test_df)) test_df[[y_col]] else NULL
          
          # metrics
          metrics <- if (!is.null(preds) && !is.null(y_test)) {
            resids  <- y_test - preds
            mse     <- mean(resids^2, na.rm = TRUE)
            rmse    <- sqrt(mse)
            mae     <- mean(abs(resids), na.rm = TRUE)
            ss_res  <- sum(resids^2, na.rm = TRUE)
            ss_tot  <- sum((y_test - mean(y_test, na.rm = TRUE))^2, na.rm = TRUE)
            r2      <- 1 - ss_res / ss_tot
            list(mse = mse, rmse = rmse, mae = mae, r2 = r2,
                 resids = resids, y_test = y_test, preds = preds)
          } else NULL
          
          # non-zero coefs
          coef_mat <- as.matrix(coef(model$finalModel, s = lambda))
          coef_df  <- data.frame(
            Predictor   = rownames(coef_mat),
            Coefficient = round(coef_mat[, 1], 6),
            stringsAsFactors = FALSE
          )
          coef_df <- coef_df[coef_df$Predictor != "(Intercept)" &
                               coef_df$Coefficient != 0, ]
          coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]
          
          # cv time from tune
          cv_time <- tryCatch({
            res <- model_tune$tune_result()
            if (!is.null(res) && is.null(res$error)) res$cv_time else 0
          }, error = function(e) 0)
          
          train_t <- round(train_time["elapsed"], 3)
          
          model_result(list(
            model      = model,
            metrics    = metrics,
            coef_df    = coef_df,
            y_col      = y_col,
            alpha      = alpha,
            lambda     = lambda,
            family     = input$family,
            cv_time    = cv_time,
            train_time = train_t,
            total_time = round(cv_time + train_t, 3),
            notes      = input$run_notes,
            error      = NULL
          ))
          
          setProgress(1)
          
        }, error = function(e) {
          model_result(list(error = conditionMessage(e)))
        })
      })
    })
    
    # ── Reset ─────────────────────────────────────────────────────────────────
    
    observeEvent(input$reset, {
      model_result(NULL)
      updateTextInput(session, "run_notes", value = "")
    })
    
    # ── Run status UI ─────────────────────────────────────────────────────────
    
    output$run_status_ui <- renderUI({
      res <- model_result()
      if (is.null(res)) return(NULL)
      if (!is.null(res$error)) {
        div(
          style = "background:#fff5f5; border:1px solid #f5c2c7; border-radius:10px;
                   padding:16px; margin-top:8px;",
          icon("circle-xmark", style = "color:#dc3545; font-size:18px;"),
          tags$span(" Model error", style = "font-weight:600; color:#dc3545;"),
          br(), br(),
          tags$code(res$error, style = "font-size:12px;")
        )
      } else {
        div(
          style = "background:#e1f5ee; border:1px solid #9fe1cb; border-radius:10px;
                   padding:16px; margin-top:8px;",
          icon("circle-check", style = "color:#0f6e56; font-size:18px;"),
          tags$span(" Model fitted successfully.", style = "font-weight:600; color:#0f6e56;"),
          br(),
          div(style = "font-size:12px; color:#0f6e56; margin-top:4px;",
              paste0("Training time: ", res$train_time, "s | Total time: ", res$total_time, "s"))
        )
      }
    })
    
    # ── Summary UI ────────────────────────────────────────────────────────────
    
    output$summary_ui <- renderUI({
      res <- model_result()
      if (is.null(res)) return(div(style = "text-align:center; color:#6c757d; padding:60px 0;",
                                   icon("list", style = "font-size:32px; color:#adb5bd;"), br(),
                                   tags$span("Run the model first.", style = "font-size:15px;")))
      if (!is.null(res$error)) return(NULL)
      
      m <- res$metrics
      tagList(
        div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:16px;",
            make_card("Alpha",       res$alpha,                         "#185FA5"),
            make_card("Lambda",      round(res$lambda, 6),              "#0F6E56"),
            make_card("N Predictors", nrow(res$coef_df),                "#534AB7"),
            make_card("Train Time",  paste0(res$train_time, "s"),       "#fd7e14"),
            make_card("Total Time",  paste0(res$total_time, "s"),       "#fd7e14")
        ),
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("ranking-star", style = "color:#185FA5; margin-right:6px;"),
                  "Variable Importance",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          plotOutput(ns("varimp_plot"), height = "400px")
        ),
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"),
                  "Non-zero Coefficients",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          DT::dataTableOutput(ns("coef_tbl"))
        )
      )
    })
    
    output$coef_tbl <- DT::renderDataTable({
      res <- model_result()
      req(res, is.null(res$error))
      DT::datatable(res$coef_df,
                    options = list(pageLength = 15, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Coefficient",
                        color      = DT::styleInterval(0, c("#C41E3A", "#198754")),
                        fontWeight = "bold")
    })
    
    output$varimp_plot <- renderPlot({
      res <- model_result()
      req(res, is.null(res$error), nrow(res$coef_df) > 0)
      df <- res$coef_df
      df$Predictor <- factor(df$Predictor, levels = df$Predictor[order(abs(df$Coefficient))])
      df$Direction <- ifelse(df$Coefficient > 0, "Positive", "Negative")
      
      ggplot2::ggplot(df, ggplot2::aes(x = Predictor, y = abs(Coefficient), fill = Direction)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = c("Positive" = "#198754", "Negative" = "#C41E3A")) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "|Coefficient|",
                      title = "Variable Importance (|Coefficient|)") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })
    
    # ── Prediction UI ─────────────────────────────────────────────────────────
    
    output$prediction_ui <- renderUI({
      res <- model_result()
      if (is.null(res)) return(div(style = "text-align:center; color:#6c757d; padding:60px 0;",
                                   icon("chart-line", style = "font-size:32px; color:#adb5bd;"), br(),
                                   tags$span("Run the model first.", style = "font-size:15px;")))
      if (!is.null(res$error)) return(NULL)
      m <- res$metrics
      if (is.null(m)) return(div(style = "padding:20px; color:#6c757d;",
                                 "No test set available for prediction."))
      tagList(
        div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:16px;",
            if ("mse"  %in% input$metric) make_card("Test MSE",  round(m$mse,  4), "#C41E3A"),
            if ("rmse" %in% input$metric) make_card("Test RMSE", round(m$rmse, 4), "#C41E3A"),
            if ("mae"  %in% input$metric) make_card("Test MAE",  round(m$mae,  4), "#fd7e14"),
            if ("r2"   %in% input$metric) make_card("Test R²",   round(m$r2,   4), "#198754")
        ),
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("chart-line", style = "color:#185FA5; margin-right:6px;"),
                  "Prediction Plot",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          plotOutput(ns("pred_plot"), height = "500px")
        )
      )
    })
    
    # ── Populate label col selector ───────────────────────────────────────────
    
    observe({
      df   <- get_data(); req(df)
      cols <- names(df)
      updateSelectInput(session, "label_col",
                        choices  = c("None" = "none", "Row index" = "rowindex",
                                     setNames(cols, cols)),
                        selected = "rowindex")
    })
    
    # ── Prediction plot ───────────────────────────────────────────────────────
    output$pred_plot <- renderPlot({
      res <- model_result()
      req(res, is.null(res$error), !is.null(res$metrics))
      m <- res$metrics
      
      df       <- get_data()
      sv       <- input$split_var
      ts       <- input$test_level
      test_df  <- if (!is.null(sv) && sv != "(none)" && !is.null(ts) && ts != "(none)")
        df[as.character(df[[sv]]) == ts, , drop = FALSE]
      else df
      
      df_plot <- data.frame(
        predicted = m$preds,
        actual    = m$y_test,
        residual  = m$resids
      )
      
      limits     <- boxplot.stats(df_plot$residual, coef = input$outlier_coef)$stats
      is_outlier <- df_plot$residual < limits[1] | df_plot$residual > limits[5]
      
      df_plot$label <- if (input$label_col == "none") {
        ""
      } else if (input$label_col == "rowindex") {
        ifelse(is_outlier, as.character(seq_len(nrow(df_plot))), "")
      } else if (input$label_col %in% names(test_df) && nrow(test_df) == nrow(df_plot)) {
        ifelse(is_outlier, as.character(test_df[[input$label_col]]), "")
      } else {
        ifelse(is_outlier, as.character(seq_len(nrow(df_plot))), "")
      }
      
      rang <- range(c(df_plot$predicted, df_plot$actual), na.rm = TRUE)
      
      make_pred_actual <- function(df) {
        ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = actual)) +
          ggplot2::geom_point(alpha = 0.6, size = 2.5, color = "#343a40") +
          ggplot2::geom_abline(slope = 1, intercept = 0, color = "#185FA5", linewidth = 1) +
          ggrepel::geom_text_repel(ggplot2::aes(label = label),
                                   max.overlaps = 50, na.rm = TRUE, size = 3.5,
                                   color = "#C41E3A") +
          ggplot2::coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE) +
          ggplot2::labs(title = "Predicted vs Actual", x = "Predicted", y = "Actual") +
          ggplot2::theme_minimal(base_size = 13)
      }
      
      make_pred_actual(df_plot)
    })
    
    # ── Residual Diagnostics UI ───────────────────────────────────────────────
    
    output$residual_ui <- renderUI({
      res <- model_result()
      if (is.null(res)) return(div(style = "text-align:center; color:#6c757d; padding:60px 0;",
                                   icon("stethoscope", style = "font-size:32px; color:#adb5bd;"), br(),
                                   tags$span("Run the model first.", style = "font-size:15px;")))
      if (!is.null(res$error)) return(NULL)
      div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("stethoscope", style = "color:#185FA5; margin-right:6px;"),
                "Residual Diagnostics",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
        plotOutput(ns("residual_plot"), height = "600px")
      )
    })
    
    output$residual_plot <- renderPlot({
      res <- model_result()
      req(res, is.null(res$error), !is.null(res$metrics))
      m <- res$metrics
      par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
      
      # 1. Residuals vs Fitted
      plot(m$preds, m$resids,
           xlab = "Fitted", ylab = "Residuals",
           main = "Residuals vs Fitted", pch = 16, col = "#34343466")
      abline(h = 0, col = "#C41E3A", lty = 2)
      
      # 2. QQ plot
      qqnorm(m$resids, main = "Normal Q-Q", pch = 16, col = "#34343466")
      qqline(m$resids, col = "#C41E3A", lty = 2)
      
      # 3. Scale-Location
      plot(m$preds, sqrt(abs(m$resids)),
           xlab = "Fitted", ylab = expression(sqrt("|Residuals|")),
           main = "Scale-Location", pch = 16, col = "#34343466")
      
      # 4. Residuals histogram
      hist(m$resids, breaks = 20, col = "#e8f0fe", border = "white",
           main = "Residual Distribution", xlab = "Residuals")
      abline(v = 0, col = "#C41E3A", lty = 2)
    })
    
    # ── Comparison UI ─────────────────────────────────────────────────────────
    
    output$comparison_ui <- renderUI({
      tbl <- comparison()
      if (nrow(tbl) == 0) {
        return(div(
          style = "text-align:center; color:#6c757d; padding:60px 0;",
          icon("table", style = "font-size:32px; color:#adb5bd;"), br(),
          tags$span("No runs added yet. Run a model and click Add to Comparison.",
                    style = "font-size:15px;")
        ))
      }
      div(
        div(style = "display:flex; justify-content:flex-end; margin-bottom:8px;",
            actionButton(ns("clear_comparison"), "Clear All",
                         icon = icon("trash-can"),
                         style = "background-color:#6c757d; color:white; border:none; font-size:12px;")
        ),
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          DT::dataTableOutput(ns("comparison_tbl"))
        )
      )
    })
    
    output$comparison_tbl <- DT::renderDataTable({
      tbl <- comparison()
      req(nrow(tbl) > 0)
      
      # base columns always shown
      base_cols   <- c("Run", "Model", "Alpha", "Lambda", "N_Pred")
      metric_cols <- c(
        if ("mse"  %in% input$metric) "Test_MSE",
        if ("rmse" %in% input$metric) "Test_RMSE",
        if ("mae"  %in% input$metric) "Test_MAE",
        if ("r2"   %in% input$metric) "Test_R2"
      )
      time_cols <- c("CV_Time", "Train_Time", "Total_Time", "Notes")
      show_cols <- c(base_cols, metric_cols, time_cols)
      tbl <- tbl[, intersect(show_cols, names(tbl)), drop = FALSE]
      
      dt <- DT::datatable(tbl,
                          options  = list(pageLength = 15, dom = "tip"),
                          rownames = FALSE,
                          selection = "single")
      
      round_cols <- intersect(c("Alpha", "Lambda", metric_cols,
                                "CV_Time", "Train_Time", "Total_Time"), names(tbl))
      if (length(round_cols) > 0)
        dt <- DT::formatRound(dt, round_cols, 4)
      if ("Test_RMSE" %in% names(tbl))
        dt <- DT::formatStyle(dt, "Test_RMSE", fontWeight = "bold", color = "#C41E3A")
      dt
    })
    
    # ── Add to Comparison ─────────────────────────────────────────────────────
    
    output$add_feedback_ui <- renderUI({ NULL })
    
    observeEvent(input$add_to_comparison, {
      res <- model_result()
      if (is.null(res) || !is.null(res$error)) {
        output$add_feedback_ui <- renderUI({
          div(style = "margin-top:6px; font-size:12px; color:#dc3545; background:#fff5f5;
                       border-left:3px solid #f5c2c7; padding:6px 10px; border-radius:6px;",
              icon("circle-xmark", style = "color:#dc3545;"),
              HTML(" No successful run to add."))
        })
        return()
      }
      
      m   <- res$metrics
      cur <- comparison()
      run_n <- nrow(cur) + 1
      
      new_row <- data.frame(
        Run        = run_n,
        Model      = paste0(if (res$alpha == 0) "Ridge" else if (res$alpha == 1) "Lasso" else "ElasticNet",
                            " (", input$model_method, ")"),
        Alpha      = res$alpha,
        Lambda     = res$lambda,
        N_Pred     = nrow(res$coef_df),
        Test_MSE   = if (!is.null(m) && "mse"  %in% input$metric) round(m$mse,  4) else NA,
        Test_RMSE  = if (!is.null(m) && "rmse" %in% input$metric) round(m$rmse, 4) else NA,
        Test_MAE   = if (!is.null(m) && "mae"  %in% input$metric) round(m$mae,  4) else NA,
        Test_R2    = if (!is.null(m) && "r2"   %in% input$metric) round(m$r2,   4) else NA,
        CV_Time    = res$cv_time,
        Train_Time = res$train_time,
        Total_Time = res$total_time,
        Notes      = res$notes,
        stringsAsFactors = FALSE
      )
      comparison(rbind(cur, new_row))
      
      output$add_feedback_ui <- renderUI({
        div(style = "margin-top:6px; font-size:12px; color:#0f6e56; background:#e1f5ee;
                     border-left:3px solid #9fe1cb; padding:6px 10px; border-radius:6px;",
            icon("circle-check", style = "color:#0f6e56;"),
            HTML(paste0(" <b>Run ", run_n, "</b> added to comparison.")))
      })
    })
    
    # ── Delete selected row ────────────────────────────────────────────────────
    
    observeEvent(input$comparison_tbl_rows_selected, {
      sel <- input$comparison_tbl_rows_selected
      if (!is.null(sel)) {
        cur <- comparison()
        cur <- cur[-sel, , drop = FALSE]
        cur$Run <- seq_len(nrow(cur))
        comparison(cur)
      }
    })
    
    # ── Clear comparison ──────────────────────────────────────────────────────
    
    observeEvent(input$clear_comparison, {
      comparison(data.frame(
        Run = integer(), Model = character(), Alpha = numeric(),
        Lambda = numeric(), N_Pred = integer(),
        Test_MSE = numeric(), Test_RMSE = numeric(),
        Test_MAE = numeric(), Test_R2 = numeric(),
        CV_Time = numeric(), Train_Time = numeric(),
        Total_Time = numeric(), Notes = character(),
        stringsAsFactors = FALSE
      ))
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      model      = reactive(model_result()$model),
      result     = reactive(model_result()),
      comparison = comparison
    ))
    
  })
}