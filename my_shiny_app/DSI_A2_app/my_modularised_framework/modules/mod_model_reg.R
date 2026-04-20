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
                     selected = c("mse", "rmse"),
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
                     tags$label("Outlier detection:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     radioButtons(ns("outlier_method"), label = NULL,
                                  choices  = c("IQR fence"       = "iqr",
                                               "Tail percentile" = "pct"),
                                  selected = "iqr", inline = TRUE)
              ),
              column(4,
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'iqr'", ns("outlier_method")),
                       tags$label("IQR coef:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       sliderInput(ns("outlier_coef"), label = NULL,
                                   min = 1.0, max = 3.0, value = 2.4, step = 0.1, width = "100%")
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'pct'", ns("outlier_method")),
                       tags$label("Tail % (each side):",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       sliderInput(ns("outlier_pct"), label = NULL,
                                   min = 1, max = 20, value = 5, step = 1, width = "100%")
                     )
              )
            )
          ),
          
          uiOutput(ns("prediction_ui")),
          uiOutput(ns("outlier_tbl_ui"))
        ),
        
        # ── Tab 4: Influence ──────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("bullseye"), " Influence"),
          style = "padding-top:16px;",
          
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("sliders", style = "color:#185FA5; margin-right:6px;"),
                    "Boxplot Controls",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            fluidRow(
              column(4,
                     tags$label("Data to show:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     radioButtons(ns("influence_data"), label = NULL,
                                  choices  = c("Train only"         = "train",
                                               "Test only"          = "test",
                                               "Both (side by side)"= "both"),
                                  selected = "train", inline = TRUE)
              ),
              column(4,
                     tags$label("IQR multiplier (k):",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     sliderInput(ns("influence_iqr_k"), label = NULL,
                                 min = 1.0, max = 5.0, value = 2.4, step = 0.1, width = "100%")
              ),
              column(4,
                     tags$label("Label outliers by:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectInput(ns("influence_label_col"), label = NULL,
                                 choices  = c("Row index" = "rowindex"),
                                 selected = "rowindex", width = "100%")
              )
            )
          ),
          uiOutput(ns("influence_ui"))
        ),
        
        # ── Tab 5: Residual Diagnostics ───────────────────────────────────
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

model_reg_server <- function(id, get_data, roles, get_recipe, model_tune, get_raw, seed = reactive(42)) {
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
    
    detect_outliers <- function(resids) {
      if (isTRUE(input$outlier_method == "pct")) {
        pct   <- input$outlier_pct / 100
        lo    <- quantile(resids, pct,       na.rm = TRUE)
        hi    <- quantile(resids, 1 - pct,   na.rm = TRUE)
        resids < lo | resids > hi
      } else {
        limits <- boxplot.stats(resids, coef = input$outlier_coef)$stats
        resids < limits[1] | resids > limits[5]
      }
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
            set.seed(seed())
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
          tags$code(res$error, style = "font-size:12px;"),
          br(), br(),
          div(
            style = "font-size:12px; color:#856404; background:#fff3cd;
                     border-left:3px solid #ffc107; padding:8px 10px;
                     border-radius:4px;",
            icon("triangle-exclamation", style = "color:#ffc107;"),
            HTML(" <b>Hint:</b> If the error mentions <code>NULL</code>, missing columns,
                  or unexpected data types, check that you have set your
                  <b>Data Roles</b> correctly under <b>Config &gt; Data Roles</b>:<br>
                  &nbsp;• One column must be assigned <b>Outcome</b> (response Y)<br>
                  &nbsp;• At least one column must be assigned <b>Predictor</b><br>
                  &nbsp;• The <b>Train-Test Split</b> column must contain
                  exactly <code>Train</code> and <code>Test</code> values<br>
                  &nbsp;• The <b>Observation ID</b> column should not be a predictor")
          )
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
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
          plot.title      = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title.x    = ggplot2::element_text(size = 16, face = "bold"),
          axis.text       = ggplot2::element_text(size = 14),
          legend.title    = ggplot2::element_text(size = 16, face = "bold"),
          legend.text     = ggplot2::element_text(size = 14),
          legend.position = "bottom"
        )
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
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
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
      
      is_outlier <- detect_outliers(df_plot$residual)
      
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
                                   max.overlaps = 50, na.rm = TRUE, size = 5,
                                   color = "#C41E3A", seed = seed()) +
          ggplot2::coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE) +
          ggplot2::labs(title = "Predicted vs Actual", x = "Predicted", y = "Actual") +
          ggplot2::theme_minimal(base_size = 16) +
          ggplot2::theme(
            plot.title   = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
            axis.title   = ggplot2::element_text(size = 16, face = "bold"),
            axis.text    = ggplot2::element_text(size = 14)
          )
      }
      
      make_pred_actual(df_plot)
    })
    
    # ── Outlier table ─────────────────────────────────────────────────────────
    
    output$outlier_tbl_ui <- renderUI({
      res <- model_result()
      req(res, is.null(res$error), !is.null(res$metrics))
      m <- res$metrics
      
      df      <- get_data()
      sv      <- input$split_var
      ts      <- input$test_level
      test_df <- if (!is.null(sv) && sv != "(none)" && !is.null(ts) && ts != "(none)")
        df[as.character(df[[sv]]) == ts, , drop = FALSE]
      else df
      
      is_outlier <- detect_outliers(m$resids)
      
      if (!any(is_outlier)) return(NULL)
      
      div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; margin-top:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("triangle-exclamation", style = "color:#C41E3A; margin-right:6px;"),
                paste0("Outlier Observations (", sum(is_outlier), ") — Full Test Row Data"),
                style = "font-weight:600; color:#343a40; margin-bottom:4px;"),
        div(style = "font-size:12px; color:#6c757d; margin-bottom:10px;",
            "Full rows from the test set for outlier observations. Investigate predictor patterns that may explain poor predictions."),
        DT::dataTableOutput(ns("outlier_tbl"))
      )
    })
    
    output$outlier_tbl <- DT::renderDataTable({
      res <- model_result()
      req(res, is.null(res$error), !is.null(res$metrics))
      m <- res$metrics
      
      df      <- get_data()
      sv      <- input$split_var
      ts      <- input$test_level
      test_df <- if (!is.null(sv) && sv != "(none)" && !is.null(ts) && ts != "(none)")
        df[as.character(df[[sv]]) == ts, , drop = FALSE]
      else df
      
      is_outlier <- detect_outliers(m$resids)
      req(any(is_outlier))
      
      # get raw test rows — match by rownames from transformed test_df
      raw_df   <- tryCatch(get_raw(), error = function(e) NULL)
      raw_test <- if (!is.null(raw_df)) {
        # match transformed test row indices back to raw data
        test_row_names <- rownames(test_df)
        raw_idx <- suppressWarnings(as.integer(test_row_names))
        if (!is.null(raw_idx) && !any(is.na(raw_idx)) && max(raw_idx) <= nrow(raw_df)) {
          raw_df[raw_idx, , drop = FALSE]
        } else if (!is.null(sv) && sv != "(none)" && sv %in% names(raw_df)) {
          raw_df[as.character(raw_df[[sv]]) == ts, , drop = FALSE]
        } else NULL
      } else NULL
      
      # build full row table with prediction columns prepended
      outlier_df <- if (!is.null(raw_test) && nrow(raw_test) == nrow(test_df)) {
        raw_test[is_outlier, , drop = FALSE]
      } else {
        test_df[is_outlier, , drop = FALSE]  # fallback to transformed
      }
      
      outlier_df <- cbind(
        data.frame(
          Actual    = round(m$y_test[is_outlier],  4),
          Predicted = round(m$preds[is_outlier],   4),
          Residual  = round(m$resids[is_outlier],  4)
        ),
        outlier_df
      )
      outlier_df <- outlier_df[order(-abs(outlier_df$Residual)), ]
      
      DT::datatable(outlier_df,
                    options  = list(pageLength = 10, dom = "tip", scrollX = TRUE),
                    rownames = FALSE) |>
        DT::formatStyle("Residual",
                        color      = DT::styleInterval(0, c("#C41E3A", "#198754")),
                        fontWeight = "bold")
    })
    
    outputOptions(output, "outlier_tbl_ui", suspendWhenHidden = FALSE)
    
    # ── Populate influence label col selector ─────────────────────────────────
    observe({
      df   <- get_data(); req(df)
      cols <- names(df)
      updateSelectInput(session, "influence_label_col",
                        choices  = c("Row index" = "rowindex", setNames(cols, cols)),
                        selected = "rowindex")
    })
    
    # ── Influence UI ──────────────────────────────────────────────────────────
    output$influence_ui <- renderUI({
      res <- model_result()
      if (is.null(res)) return(div(
        style = "text-align:center; color:#6c757d; padding:60px 0;",
        icon("bullseye", style = "font-size:32px; color:#adb5bd;"), br(),
        tags$span("Run the model first.", style = "font-size:15px;")))
      if (!is.null(res$error)) return(NULL)
      
      tagList(
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("bullseye", style = "color:#185FA5; margin-right:6px;"),
                  "Residual Boxplot",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          plotOutput(ns("influence_plot"), height = "350px")
        ),
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("triangle-exclamation", style = "color:#C41E3A; margin-right:6px;"),
                  "Influential Observations",
                  style = "font-weight:600; color:#343a40; margin-bottom:4px;"),
          div(style = "font-size:12px; color:#6c757d; margin-bottom:10px;",
              "Rows whose residuals fall outside the IQR whiskers."),
          DT::dataTableOutput(ns("influence_tbl"))
        ),
        
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-top:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("lightbulb", style = "color:#185FA5; margin-right:6px;"),
                  "Interpretation Hints",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          div(
            style = "font-size:12px; color:#343a40; line-height:1.8;",
            tags$b("1. Median not on the zero line:"), br(),
            HTML("&nbsp;&nbsp;• <b>Median right of 0</b> — model under-predicts.
                  Actual values are consistently higher than predicted.<br>
                  &nbsp;&nbsp;• <b>Median left of 0</b> — model over-predicts.
                  Actual values are consistently lower than predicted.<br>
                  &nbsp;&nbsp;• If only the test median is off but train is centred —
                  suspect data shift or overfitting.<br>
                  &nbsp;&nbsp;• If both are off in the same direction —
                  a structural issue: missing predictor, wrong family, or response needs transformation."),
            hr(),
            tags$b("2. Outliers clustering on one side:"), br(),
            HTML("&nbsp;&nbsp;• <b>More positive outliers</b> — model fails to capture
                  high-value cases. The response may be right-skewed and Gaussian GLM
                  pulls predictions toward the mean, missing the tail.<br>
                  &nbsp;&nbsp;• Check flagged rows — do they share common characteristics
                  (e.g. specific GOVERN_TYPE, high POPULATION)?<br>
                  &nbsp;&nbsp;• Cross-reference with outlier detection methods (Mahalanobis,
                  Cook's, LOF). If the same observations appear across multiple methods,
                  they are genuinely unusual."),
            hr(),
            tags$b("3. More outliers than expected at this IQR multiplier:"), br(),
            HTML("&nbsp;&nbsp;• At k=2.4, only ~0.1% of observations are expected to be
                  flagged under normality (~0–1 for n=357).<br>
                  &nbsp;&nbsp;• <b>Many flagged points</b> suggest heavy-tailed residuals,
                  model misspecification, or heteroscedasticity.<br>
                  &nbsp;&nbsp;• Check the <b>QQ plot</b> and <b>Scale-Location</b> in the
                  Residual Diagnostics tab.<br>
                  &nbsp;&nbsp;• Try increasing k to 3.0–3.5 — if the count drops dramatically,
                  the threshold is too strict for this data rather than genuine influential points.")
          )
        )
      )
    })
    
    output$influence_plot <- renderPlot({
      res <- model_result(); req(res, is.null(res$error))
      m   <- res$metrics;   req(m)
      
      df  <- get_data()
      sv  <- input$split_var
      tl  <- input$train_level
      ts  <- input$test_level
      k   <- input$influence_iqr_k
      
      # ── build train residuals ──────────────────────────────────────────────
      splits_ok <- !is.null(sv) && sv != "(none)" &&
        !is.null(tl) && tl != "(none)" &&
        !is.null(ts) && ts != "(none)"
      train_df <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop=FALSE] else df
      test_df  <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop=FALSE] else NULL
      
      # get label values for train
      get_labels <- function(df_sub, n) {
        lc <- input$influence_label_col
        if (lc == "rowindex") as.character(seq_len(n))
        else if (lc %in% names(df_sub) && nrow(df_sub) == n)
          as.character(df_sub[[lc]])
        else as.character(seq_len(n))
      }
      
      train_resids <- tryCatch({
        rec    <- get_recipe(); req(rec)
        set.seed(seed())
        prepped <- recipes::prep(rec, training = train_df, verbose = FALSE)
        baked   <- recipes::bake(prepped, new_data = train_df)
        y_col   <- res$y_col
        y_train <- baked[[y_col]]
        preds_train <- predict(res$model, newdata = train_df)
        y_train - preds_train
      }, error = function(e) NULL)
      
      if (is.null(train_resids)) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="Could not compute train residuals.",
                            colour="#C41E3A", size=5) + ggplot2::theme_void()
        return()
      }
      
      # ── assemble plot data ─────────────────────────────────────────────────
      train_row <- data.frame(
        residual = train_resids,
        group    = "Train",
        label_id = get_labels(train_df, length(train_resids)),
        stringsAsFactors = FALSE
      )
      
      test_row <- if (!is.null(m$resids) && !is.null(test_df)) {
        data.frame(
          residual = m$resids,
          group    = "Test",
          label_id = get_labels(test_df, length(m$resids)),
          stringsAsFactors = FALSE
        )
      } else NULL
      
      plot_df <- switch(input$influence_data,
                        "train" = train_row,
                        "test"  = if (!is.null(test_row)) test_row else train_row,
                        "both"  = if (!is.null(test_row)) rbind(train_row, test_row) else train_row
      )
      
      # ── flag outliers per group ────────────────────────────────────────────
      plot_df$is_out <- ave(plot_df$residual, plot_df$group, FUN = function(x) {
        lims <- boxplot.stats(x, coef = k)$stats
        as.numeric(x < lims[1] | x > lims[5])
      }) == 1
      plot_df$label <- ifelse(plot_df$is_out, plot_df$label_id, NA_character_)
      
      group_colors <- c("Train" = "#185FA5", "Test" = "#C41E3A")
      
      ggplot2::ggplot(plot_df, ggplot2::aes(x = residual, y = group)) +
        ggplot2::geom_boxplot(coef = k, outlier.colour = "#C41E3A",
                              fill = "#e8f0fe", width = 0.4) +
        ggrepel::geom_text_repel(ggplot2::aes(label = label),
                                 max.overlaps = 30, na.rm = TRUE,
                                 size = 5, colour = "#C41E3A", seed = seed()) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#6c757d") +
        ggplot2::labs(
          title = paste0("Residual Boxplot | IQR k = ", k, " | ",
                         switch(input$influence_data,
                                "train" = "Train only",
                                "test"  = "Test only",
                                "both"  = "Train + Test")),
          x = "Residual", y = NULL) +
        ggplot2::theme_minimal(base_size = 16) +
        ggplot2::theme(
          plot.title         = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title.x       = ggplot2::element_text(size = 16, face = "bold"),
          axis.text.x        = ggplot2::element_text(size = 14),
          axis.text.y        = ggplot2::element_text(face = "bold", size = 14),
          panel.grid.major.y = ggplot2::element_blank())
    })
    
    output$influence_tbl <- DT::renderDataTable({
      res <- model_result(); req(res, is.null(res$error))
      m   <- res$metrics;   req(m)
      
      df  <- get_data()
      sv  <- input$split_var
      tl  <- input$train_level
      ts  <- input$test_level
      k   <- input$influence_iqr_k
      
      splits_ok <- !is.null(sv) && sv != "(none)" &&
        !is.null(tl) && tl != "(none)" &&
        !is.null(ts) && ts != "(none)"
      train_df <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop=FALSE] else df
      test_df  <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop=FALSE] else NULL
      raw_df   <- tryCatch(get_raw(), error = function(e) NULL)
      
      # train residuals
      train_resids <- tryCatch({
        rec     <- get_recipe(); req(rec)
        set.seed(seed())
        prepped <- recipes::prep(rec, training = train_df, verbose = FALSE)
        baked   <- recipes::bake(prepped, new_data = train_df)
        y_train <- baked[[res$y_col]]
        preds_train <- predict(res$model, newdata = train_df)
        y_train - preds_train
      }, error = function(e) NULL)
      
      if (is.null(train_resids)) return(data.frame(Message = "Could not compute train residuals."))
      
      # flag outliers
      flag_outliers <- function(resids, k) {
        lims <- boxplot.stats(resids, coef = k)$stats
        resids < lims[1] | resids > lims[5]
      }
      
      train_out <- flag_outliers(train_resids, k)
      rows_list <- list()
      
      if (any(train_out) && input$influence_data %in% c("train","both")) {
        raw_train <- if (!is.null(raw_df)) {
          idx <- suppressWarnings(as.integer(rownames(train_df)))
          if (!any(is.na(idx)) && max(idx) <= nrow(raw_df)) raw_df[idx,,drop=FALSE]
          else train_df
        } else train_df
        tbl_train <- raw_train[train_out,,drop=FALSE]
        tbl_train <- cbind(
          data.frame(Set="Train",
                     Residual=round(train_resids[train_out], 4)),
          tbl_train)
        rows_list[["train"]] <- tbl_train
      }
      
      if (input$influence_data %in% c("both","test") && !is.null(m$resids)) {
        test_out <- flag_outliers(m$resids, k)
        if (any(test_out)) {
          raw_test <- if (!is.null(raw_df) && !is.null(test_df)) {
            idx <- suppressWarnings(as.integer(rownames(test_df)))
            if (!any(is.na(idx)) && max(idx) <= nrow(raw_df)) raw_df[idx,,drop=FALSE]
            else test_df
          } else test_df
          if (!is.null(raw_test)) {
            tbl_test <- raw_test[test_out,,drop=FALSE]
            tbl_test <- cbind(
              data.frame(Set="Test",
                         Residual=round(m$resids[test_out], 4)),
              tbl_test)
            rows_list[["test"]] <- tbl_test
          }
        }
      }
      
      if (length(rows_list) == 0) return(data.frame(Message = "No influential observations flagged."))
      
      out_df <- do.call(rbind, rows_list)
      out_df <- out_df[order(-abs(out_df$Residual)), ]
      
      DT::datatable(out_df,
                    options  = list(pageLength = 10, dom = "tip", scrollX = TRUE),
                    rownames = FALSE) |>
        DT::formatStyle("Residual",
                        color      = DT::styleInterval(0, c("#C41E3A", "#198754")),
                        fontWeight = "bold") |>
        DT::formatStyle("Set",
                        color      = DT::styleEqual(c("Train","Test"), c("#185FA5","#C41E3A")),
                        fontWeight = "bold")
    })
    
    # ── Residual Diagnostics UI ───────────────────────────────────────────────
    
    output$residual_ui <- renderUI({
      res <- model_result()
      if (is.null(res)) return(div(style = "text-align:center; color:#6c757d; padding:60px 0;",
                                   icon("stethoscope", style = "font-size:32px; color:#adb5bd;"), br(),
                                   tags$span("Run the model first.", style = "font-size:15px;")))
      if (!is.null(res$error)) return(NULL)
      tagList(
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("stethoscope", style = "color:#185FA5; margin-right:6px;"),
                  "Residual Diagnostics",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          plotOutput(ns("residual_plot"), height = "600px")
        ),
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("chart-bar", style = "color:#185FA5; margin-right:6px;"),
                  "Chi-Squared Goodness-of-Fit Test on Residuals",
                  style = "font-weight:600; color:#343a40; margin-bottom:4px;"),
          div(style = "font-size:12px; color:#6c757d; margin-bottom:10px;",
              "Tests whether residuals follow a normal distribution by comparing observed
               bin frequencies against expected frequencies under normality.
               Complements the QQ plot with a formal p-value."),
          verbatimTextOutput(ns("chisq_test"))
        )
      )
    })
    
    output$residual_plot <- renderPlot({
      res <- model_result()
      req(res, is.null(res$error), !is.null(res$metrics))
      m <- res$metrics
      par(mfrow = c(2, 2), mar = c(4, 4, 3, 1),
          cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)
      
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
    
    output$chisq_test <- renderPrint({
      res <- model_result()
      req(res, is.null(res$error), !is.null(res$metrics))
      m      <- res$metrics
      resids <- m$resids
      n      <- length(resids)
      
      # bin residuals into k bins based on normal quantiles
      k       <- max(5, min(20, round(sqrt(n))))
      breaks  <- qnorm(seq(0, 1, length.out = k + 1),
                       mean = mean(resids), sd = sd(resids))
      breaks[1]     <- -Inf
      breaks[k + 1] <-  Inf
      
      observed <- as.integer(table(cut(resids, breaks = breaks, include.lowest = TRUE)))
      expected <- rep(n / k, k)
      
      tryCatch({
        test <- chisq.test(observed, p = rep(1/k, k))
        cat(sprintf("Chi-Squared Goodness-of-Fit | Normality of Residuals
"))
        cat(sprintf("Bins (k)    : %d
", k))
        cat(sprintf("n           : %d
", n))
        cat(sprintf("Chi-sq      : %.4f
", test$statistic))
        cat(sprintf("df          : %d
", test$parameter))
        cat(sprintf("p-value     : %.4f
", test$p.value))
        cat("──────────────────────────────
")
        if (test$p.value < 0.05) {
          cat("Conclusion  : Reject H0 — residuals are NOT normally distributed (p < 0.05).
")
          cat("              Check the QQ plot for where normality breaks down.
")
          cat("              Consider a different response family or transform.
")
        } else {
          cat("Conclusion  : Fail to reject H0 — residuals are consistent with normality (p ≥ 0.05).
")
          cat("              The Gaussian family assumption appears reasonable.
")
        }
      }, error = function(e) {
        cat("Could not compute chi-squared test:
", conditionMessage(e), "
")
      })
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