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
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Regularised Regression</b><br><br>
              Fits Ridge, Lasso, or ElasticNet via <b>caret::train</b> using
              the recipe defined in Pre-Processing. Fitted on train only.")
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
      
      # ── Model type ────────────────────────────────────────────────────────
      tags$label("Model type:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("model_type"), label = NULL,
                   choices = c(
                     "Ridge"      = "ridge",
                     "Lasso"      = "lasso",
                     "ElasticNet" = "elasticnet"
                   ),
                   selected = "elasticnet"),
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
      
      # ── Run ───────────────────────────────────────────────────────────────
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
        width = "100%"
      )
    ),
    
    mainPanel(
      width = 9,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

model_reg_server <- function(id, get_data, roles, split, precipe) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate split var from roles ─────────────────────────────────────────
    
    observe({
      req(get_data(), roles())
      r    <- roles()
      vars <- names(get_data())
      updateSelectInput(session, "split_var",
                        choices  = c("(none)", vars),
                        selected = { v <- names(r)[r == "split"]; if (length(v)) v[1] else "(none)" })
    })
    
    # ── Train / Test level selectors ─────────────────────────────────────────
    
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
    
    # ── State ─────────────────────────────────────────────────────────────────
    
    model_result <- reactiveVal(NULL)
    
    # ── Run ───────────────────────────────────────────────────────────────────
    
    observeEvent(input$run, {
      
      rec <- precipe$recipe()
      
      if (is.null(rec)) {
        model_result(list(error = "No recipe built. Go to Pre-Processing > Recipe and click Build Recipe first."))
        return()
      }
      
      withProgress(message = "Fitting model...", value = 0.1, {
        tryCatch({
          library(caret)
          library(glmnet)
          
          df  <- get_data(); req(df)
          sv  <- input$split_var
          tl  <- input$train_level
          ts  <- input$test_level
          splits_ok <- !is.null(sv) && sv != "(none)" &&
            !is.null(tl) && tl != "(none)" &&
            !is.null(ts) && ts != "(none)"
          
          train_df <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop = FALSE] else df
          test_df  <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop = FALSE] else NULL
          
          if (nrow(train_df) == 0)
            stop("No training data available. Check your split variable and train level.")
          
          # alpha based on model type
          alpha_val <- switch(input$model_type,
                              "ridge"      = 0,
                              "lasso"      = 1,
                              "elasticnet" = NULL
          )
          
          tune_grid <- if (!is.null(alpha_val)) {
            expand.grid(alpha  = alpha_val,
                        lambda = 10^seq(3, -4, length = 50))
          } else {
            expand.grid(alpha  = seq(0, 1, by = 0.1),
                        lambda = 10^seq(3, -4, length = 50))
          }
          
          incProgress(0.3, message = "Training...")
          
          set.seed(2026)
          model <- caret::train(
            rec,
            data      = train_df,
            method    = "glmnet",
            family    = input$family,
            tuneGrid  = tune_grid,
            trControl = trainControl(method = "cv", number = 5)
          )
          
          incProgress(0.7, message = "Predicting on test set...")
          
          # predict on test
          preds <- if (!is.null(test_df) && nrow(test_df) > 0) {
            predict(model, newdata = test_df)
          } else NULL
          
          # get outcome col from recipe
          y_col <- model$recipe$var_info$variable[model$recipe$var_info$role == "outcome"][1]
          
          # metrics
          metrics <- if (!is.null(preds) && !is.null(test_df) && y_col %in% names(test_df)) {
            y_test  <- test_df[[y_col]]
            if (input$family == "gaussian") {
              rmse <- sqrt(mean((y_test - preds)^2, na.rm = TRUE))
              mae  <- mean(abs(y_test - preds), na.rm = TRUE)
              ss_res <- sum((y_test - preds)^2, na.rm = TRUE)
              ss_tot <- sum((y_test - mean(y_test, na.rm = TRUE))^2, na.rm = TRUE)
              r2   <- 1 - ss_res / ss_tot
              list(rmse = rmse, mae = mae, r2 = r2,
                   y_test = y_test, preds = preds)
            } else {
              list(y_test = y_test, preds = preds)
            }
          } else NULL
          
          model_result(list(
            model   = model,
            metrics = metrics,
            y_col   = y_col,
            alpha   = model$bestTune$alpha,
            lambda  = model$bestTune$lambda,
            family  = input$family,
            type    = input$model_type
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
      updateRadioButtons(session, "model_type", selected = "elasticnet")
      updateRadioButtons(session, "family",     selected = "gaussian")
    })
    
    # ── Main UI ───────────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      res <- model_result()
      
      # placeholder
      if (is.null(res)) {
        return(div(
          style = "text-align:center; color:#6c757d; padding:60px 0;",
          icon("brain", style = "font-size:32px; color:#adb5bd; margin-bottom:10px;"),
          br(),
          tags$span("Configure the recipe in Pre-Processing, then click Run Model.",
                    style = "font-size:15px;")
        ))
      }
      
      # error
      if (!is.null(res$error)) {
        return(div(
          style = "background:#fff5f5; border:1px solid #f5c2c7; border-radius:10px;
                   padding:20px; margin-top:10px;",
          icon("circle-xmark", style = "color:#dc3545; font-size:20px;"),
          tags$span(" Model error", style = "font-weight:600; color:#dc3545;"),
          br(), br(),
          tags$code(res$error, style = "font-size:12px;")
        ))
      }
      
      # ── stat cards ────────────────────────────────────────────────────────
      card <- function(label, value, color) {
        tags$div(
          style = paste0(
            "flex:1; min-width:130px; background:white; border-radius:10px;",
            "border:0.5px solid #dee2e6; padding:14px 18px;",
            "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
          ),
          tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                            text-transform:uppercase; letter-spacing:.5px;", label),
          tags$div(style = paste0("font-size:24px; font-weight:700; color:", color, ";"), value)
        )
      }
      
      m <- res$metrics
      cards <- tags$div(
        style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
        card("Model",  toupper(res$type),              "#185FA5"),
        card("Family", res$family,                     "#534AB7"),
        card("Alpha",  round(res$alpha, 4),            "#0F6E56"),
        card("Lambda", round(res$lambda, 6),           "#BA7517"),
        if (!is.null(m$rmse)) card("RMSE", round(m$rmse, 4), "#C41E3A"),
        if (!is.null(m$mae))  card("MAE",  round(m$mae,  4), "#C41E3A"),
        if (!is.null(m$r2))   card("R²",   round(m$r2,   4), "#198754")
      )
      
      # ── predicted vs actual plot ───────────────────────────────────────────
      pred_plot <- if (!is.null(m) && res$family == "gaussian") {
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("chart-line", style = "color:#185FA5; margin-right:6px;"),
                  "Predicted vs Actual (Test Set)",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          plotly::plotlyOutput(ns("pred_plot"), height = "400px")
        )
      } else NULL
      
      # ── coefficient table ──────────────────────────────────────────────────
      coef_section <- tags$div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"),
                "Non-zero Coefficients",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
        DT::dataTableOutput(ns("coef_tbl"))
      )
      
      tagList(cards, pred_plot, coef_section)
    })
    
    # ── Predicted vs actual plot ───────────────────────────────────────────────
    
    output$pred_plot <- plotly::renderPlotly({
      res <- model_result()
      req(res, is.null(res$error), !is.null(res$metrics))
      m <- res$metrics
      req(!is.null(m$y_test), !is.null(m$preds))
      
      lim <- range(c(m$y_test, m$preds), na.rm = TRUE)
      
      plotly::plot_ly() |>
        plotly::add_markers(x = m$y_test, y = m$preds,
                            marker = list(color = "#185FA5", opacity = 0.6, size = 7),
                            name = "Predictions") |>
        plotly::add_lines(x = lim, y = lim,
                          line = list(color = "#C41E3A", dash = "dash", width = 1.5),
                          name = "Perfect fit") |>
        plotly::layout(
          xaxis = list(title = "<b>Actual</b>"),
          yaxis = list(title = "<b>Predicted</b>"),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.05)
        )
    })
    
    # ── Coefficient table ──────────────────────────────────────────────────────
    
    output$coef_tbl <- DT::renderDataTable({
      res <- model_result()
      req(res, is.null(res$error))
      
      coef_mat <- as.matrix(coef(res$model$finalModel,
                                 s = res$model$bestTune$lambda))
      coef_df <- data.frame(
        Predictor   = rownames(coef_mat),
        Coefficient = round(coef_mat[, 1], 6),
        stringsAsFactors = FALSE
      )
      coef_df <- coef_df[coef_df$Predictor != "(Intercept)" &
                           coef_df$Coefficient != 0, ]
      coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]
      rownames(coef_df) <- NULL
      
      DT::datatable(coef_df,
                    options = list(pageLength = 15, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Coefficient",
                        color = DT::styleInterval(0, c("#C41E3A", "#198754")),
                        fontWeight = "bold")
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      model  = reactive(model_result()$model),
      result = reactive(model_result())
    ))
    
  })
}