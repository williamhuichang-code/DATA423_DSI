# =================================================================================
# mod_model_tune.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

model_tune_ui <- function(id) {
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
        HTML("&nbsp; <b>Optimise &alpha; and &lambda;</b><br><br>
              Use cross-validation to find the optimal regularisation
              parameters before fitting the final model.")
      ),
      hr(),
      
      # ── Mode ──────────────────────────────────────────────────────────────
      tags$label("Mode:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("mode"), label = NULL,
                   choices  = c("Ridge / Lasso" = "ridge_lasso",
                                "ElasticNet"    = "elasticnet"),
                   selected = "ridge_lasso"),
      hr(),
      
      # ── Run / Reset ───────────────────────────────────────────────────────
      actionButton(
        ns("run"),
        label = "Run CV",
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
      
      tabsetPanel(
        type = "tabs",
        
        # ── Tab 1: Configuration ─────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("sliders"), " Configuration"),
          style = "padding-top:16px;",
          
          # ── Split config ──────────────────────────────────────────────────
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(
              icon("scissors", style = "color:#185FA5; margin-right:6px;"),
              "Split",
              style = "font-weight:600; color:#343a40; margin-bottom:12px;"
            ),
            fluidRow(
              column(4,
                     tags$label("Split variable:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectInput(ns("split_var"), label = NULL,
                                 choices = c("(none)"), width = "100%")
              ),
              column(4,
                     tags$label("Train level:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectInput(ns("train_level"), label = NULL,
                                 choices = c("(none)"), width = "100%")
              ),
              column(4,
                     tags$label("Test level:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectInput(ns("test_level"), label = NULL,
                                 choices = c("(none)"), width = "100%")
              )
            )
          ),
          
          # ── Ridge / Lasso config ──────────────────────────────────────────
          conditionalPanel(
            condition = sprintf("input['%s'] == 'ridge_lasso'", ns("mode")),
            
            div(
              style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                       padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              tags$h5(
                icon("sliders", style = "color:#185FA5; margin-right:6px;"),
                "Ridge / Lasso — Define",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"
              ),
              fluidRow(
                column(6,
                       tags$label("Alpha (model type):",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                           "0 = Ridge, 1 = Lasso"),
                       sliderInput(ns("rl_alpha"), label = NULL,
                                   min = 0, max = 1, value = 0, step = 0.05, width = "100%")
                ),
                column(6,
                       tags$label("Lambda grid:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                           "10^seq(from, to, length)"),
                       fluidRow(
                         column(4, numericInput(ns("rl_lambda_from"),   "From (exp):", value =  3, step = 1, width = "100%")),
                         column(4, numericInput(ns("rl_lambda_to"),     "To (exp):",   value = -4, step = 1, width = "100%")),
                         column(4, numericInput(ns("rl_lambda_length"), "Length:",     value = 100, step = 10, width = "100%"))
                       )
                )
              ),
              fluidRow(
                column(6,
                       tags$label("Convergence threshold (optional):",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                           "Smaller = more precise but slower. Default: 1e-7"),
                       selectInput(ns("rl_thresh"), label = NULL,
                                   choices  = c("Default (1e-7)" = "1e-7",
                                                "Strict (1e-10)" = "1e-10",
                                                "Fast (1e-5)"    = "1e-5"),
                                   selected = "1e-7", width = "100%")
                )
              )
            ),
            
            div(
              style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                       padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              tags$h5(
                icon("rotate", style = "color:#185FA5; margin-right:6px;"),
                "Ridge / Lasso — CV Settings",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"
              ),
              fluidRow(
                column(6,
                       tags$label("CV method:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       radioButtons(ns("rl_cv_method"), label = NULL,
                                    choices  = c("k-Fold CV" = "kfold",
                                                 "LOOCV"     = "loocv"),
                                    selected = "kfold")
                ),
                column(6,
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'kfold'", ns("rl_cv_method")),
                         tags$label("Number of folds:",
                                    style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                         sliderInput(ns("rl_nfolds"), label = NULL,
                                     min = 3, max = 20, value = 10, step = 1, width = "100%")
                       ),
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'loocv'", ns("rl_cv_method")),
                         div(
                           style = "font-size:12px; color:#6c757d; background:#e8f0fe;
                               border-left:3px solid #a8c0fd; padding:8px 10px; border-radius:4px;",
                           icon("circle-info", style = "color:#185FA5;"),
                           HTML(" LOOCV sets <b>nfolds = n</b> (number of training rows).
                            Recommended for small datasets.")
                         )
                       )
                )
              )
            )
          ),
          
          # ── ElasticNet config ─────────────────────────────────────────────
          conditionalPanel(
            condition = sprintf("input['%s'] == 'elasticnet'", ns("mode")),
            
            div(
              style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                       padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              tags$h5(
                icon("sliders", style = "color:#185FA5; margin-right:6px;"),
                "ElasticNet — Define",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"
              ),
              fluidRow(
                column(6,
                       tags$label("Alpha grid:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                           "seq(from, to, by) — searches over alpha values"),
                       fluidRow(
                         column(4, numericInput(ns("en_alpha_from"), "From:", value = 0,    min = 0, max = 1, step = 0.1, width = "100%")),
                         column(4, numericInput(ns("en_alpha_to"),   "To:",   value = 1,    min = 0, max = 1, step = 0.1, width = "100%")),
                         column(4, numericInput(ns("en_alpha_by"),   "By:",   value = 0.1,  min = 0.01, step = 0.05, width = "100%"))
                       )
                ),
                column(6,
                       tags$label("Lambda grid:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                           "10^seq(from, to, length)"),
                       fluidRow(
                         column(4, numericInput(ns("en_lambda_from"),   "From (exp):", value =  3, step = 1, width = "100%")),
                         column(4, numericInput(ns("en_lambda_to"),     "To (exp):",   value = -4, step = 1, width = "100%")),
                         column(4, numericInput(ns("en_lambda_length"), "Length:",     value = 100, step = 10, width = "100%"))
                       )
                )
              ),
              fluidRow(
                column(6,
                       tags$label("Convergence threshold (optional):",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                           "Smaller = more precise but slower. Default: 1e-7"),
                       selectInput(ns("en_thresh"), label = NULL,
                                   choices  = c("Default (1e-7)" = "1e-7",
                                                "Strict (1e-10)" = "1e-10",
                                                "Fast (1e-5)"    = "1e-5"),
                                   selected = "1e-7", width = "100%")
                )
              )
            ),
            
            div(
              style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                       padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              tags$h5(
                icon("rotate", style = "color:#185FA5; margin-right:6px;"),
                "ElasticNet — CV Settings",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"
              ),
              div(
                style = "font-size:12px; color:#856404; background:#fff3cd;
                         border-left:3px solid #ffc107; padding:6px 10px;
                         border-radius:4px; margin-bottom:12px;",
                icon("triangle-exclamation", style = "color:#ffc107;"),
                HTML(" ElasticNet loops <b>cv.glmnet</b> over the full alpha grid.
                      LOOCV may be preferred for small datasets despite the loop,
                      as glmnet's warm-start makes it efficient.")
              ),
              fluidRow(
                column(6,
                       tags$label("CV method:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       radioButtons(ns("en_cv_method"), label = NULL,
                                    choices  = c("k-Fold CV" = "kfold",
                                                 "LOOCV"     = "loocv"),
                                    selected = "kfold")
                ),
                column(6,
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'kfold'", ns("en_cv_method")),
                         tags$label("Number of folds:",
                                    style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                         sliderInput(ns("en_nfolds"), label = NULL,
                                     min = 3, max = 20, value = 10, step = 1, width = "100%")
                       ),
                       conditionalPanel(
                         condition = sprintf("input['%s'] == 'loocv'", ns("en_cv_method")),
                         div(
                           style = "font-size:12px; color:#6c757d; background:#e8f0fe;
                               border-left:3px solid #a8c0fd; padding:8px 10px; border-radius:4px;",
                           icon("circle-info", style = "color:#185FA5;"),
                           HTML(" LOOCV sets <b>nfolds = n</b> (number of training rows).
                            Recommended for small datasets.")
                         )
                       )
                )
              )
            )
          )
        ), # end Configuration tab
        
        # ── Tab 2: CV Results ────────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("chart-line"), " CV Results"),
          style = "padding-top:16px;",
          uiOutput(ns("cv_results_ui"))
        )
        
      ) # end tabsetPanel
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

model_tune_server <- function(id, get_data, roles, get_recipe) {
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
    
    tune_result <- reactiveVal(NULL)
    
    # ── Helper: prep + bake recipe to get X matrix and y ─────────────────────
    
    get_xy <- function(df, rec, y_col) {
      library(recipes)
      prepped <- prep(rec, training = df, verbose = FALSE)
      baked   <- bake(prepped, new_data = df)
      y <- baked[[y_col]]
      x <- as.matrix(baked[, setdiff(names(baked), y_col), drop = FALSE])
      list(x = x, y = y)
    }
    
    # ── Run CV ────────────────────────────────────────────────────────────────
    
    observeEvent(input$run, {
      
      rec <- get_recipe()
      if (is.null(rec)) {
        tune_result(list(error = "No recipe built. Go to Pre-Processing > Recipe and click Build Recipe first."))
        return()
      }
      
      df       <- get_data(); req(df)
      sv       <- input$split_var
      tl       <- input$train_level
      ts       <- input$test_level
      splits_ok <- !is.null(sv) && sv != "(none)" &&
        !is.null(tl) && tl != "(none)" &&
        !is.null(ts) && ts != "(none)"
      
      train_df <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop = FALSE] else df
      test_df  <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop = FALSE] else NULL
      
      if (nrow(train_df) == 0) {
        tune_result(list(error = "No training data. Check split variable and train level."))
        return()
      }
      
      # get outcome column from recipe
      y_col <- tryCatch({
        rec$var_info$variable[rec$var_info$role == "outcome"][1]
      }, error = function(e) NULL)
      
      if (is.null(y_col) || is.na(y_col)) {
        tune_result(list(error = "Could not determine response variable from recipe. Rebuild recipe first."))
        return()
      }
      
      mode <- input$mode
      
      withProgress(message = "Running CV...", value = 0.1, {
        tryCatch({
          library(glmnet)
          
          # prep and bake
          incProgress(0.1, message = "Preparing data...")
          xy_train <- get_xy(train_df, rec, y_col)
          x_train  <- xy_train$x
          y_train  <- xy_train$y
          
          xy_test  <- if (!is.null(test_df)) get_xy(test_df, rec, y_col) else NULL
          
          if (mode == "ridge_lasso") {
            
            alpha       <- input$rl_alpha
            lambda_grid <- 10^seq(input$rl_lambda_from, input$rl_lambda_to,
                                  length.out = input$rl_lambda_length)
            thresh      <- as.numeric(input$rl_thresh)
            nfolds      <- if (input$rl_cv_method == "loocv") nrow(x_train) else input$rl_nfolds
            
            incProgress(0.3, message = "Running cv.glmnet...")
            set.seed(2026)
            cv_time <- system.time({
              cv_fit <- cv.glmnet(x_train, y_train,
                                  alpha  = alpha,
                                  lambda = lambda_grid,
                                  nfolds = nfolds,
                                  thresh = thresh)
            })
            
            tune_result(list(
              mode         = "ridge_lasso",
              cv_fit       = cv_fit,
              alpha        = alpha,
              lambda_min   = cv_fit$lambda.min,
              lambda_1se   = cv_fit$lambda.1se,
              cv_time      = round(cv_time["elapsed"], 3),
              nfolds       = nfolds,
              cv_method    = input$rl_cv_method,
              x_train      = x_train,
              y_train      = y_train,
              xy_test      = xy_test,
              y_col        = y_col,
              error        = NULL
            ))
            
          } else {
            
            alpha_grid  <- seq(input$en_alpha_from, input$en_alpha_to, by = input$en_alpha_by)
            lambda_grid <- 10^seq(input$en_lambda_from, input$en_lambda_to,
                                  length.out = input$en_lambda_length)
            thresh      <- as.numeric(input$en_thresh)
            nfolds      <- if (input$en_cv_method == "loocv") nrow(x_train) else input$en_nfolds
            
            incProgress(0.2, message = paste0("Looping over ", length(alpha_grid), " alpha values..."))
            
            set.seed(2026)
            cv_time <- system.time({
              cv_fits <- lapply(alpha_grid, function(a) {
                cv.glmnet(x_train, y_train,
                          alpha  = a,
                          lambda = lambda_grid,
                          nfolds = nfolds,
                          thresh = thresh)
              })
            })
            names(cv_fits) <- as.character(alpha_grid)
            
            # find best alpha — lowest min CV error
            min_errors  <- sapply(cv_fits, function(f) min(f$cvm))
            best_alpha  <- alpha_grid[which.min(min_errors)]
            best_cv_fit <- cv_fits[[as.character(best_alpha)]]
            
            tune_result(list(
              mode         = "elasticnet",
              cv_fits      = cv_fits,
              cv_fit       = best_cv_fit,
              alpha_grid   = alpha_grid,
              best_alpha   = best_alpha,
              min_errors   = min_errors,
              lambda_min   = best_cv_fit$lambda.min,
              lambda_1se   = best_cv_fit$lambda.1se,
              cv_time      = round(cv_time["elapsed"], 3),
              nfolds       = nfolds,
              cv_method    = input$en_cv_method,
              x_train      = x_train,
              y_train      = y_train,
              xy_test      = xy_test,
              y_col        = y_col,
              error        = NULL
            ))
          }
          
          setProgress(1, message = "Done.")
          
        }, error = function(e) {
          tune_result(list(error = conditionMessage(e)))
        })
      })
    })
    
    # ── Reset ─────────────────────────────────────────────────────────────────
    
    observeEvent(input$reset, {
      tune_result(NULL)
    })
    
    # ── CV Results UI ─────────────────────────────────────────────────────────
    
    output$cv_results_ui <- renderUI({
      res <- tune_result()
      
      if (is.null(res)) {
        return(div(
          style = "text-align:center; color:#6c757d; padding:60px 0;",
          icon("chart-line", style = "font-size:32px; color:#adb5bd; margin-bottom:10px;"),
          br(),
          tags$span("Configure settings and click Run CV.",
                    style = "font-size:15px;")
        ))
      }
      
      if (!is.null(res$error)) {
        return(div(
          style = "background:#fff5f5; border:1px solid #f5c2c7; border-radius:10px; padding:20px;",
          icon("circle-xmark", style = "color:#dc3545; font-size:20px;"),
          tags$span(" CV error", style = "font-weight:600; color:#dc3545;"),
          br(), br(),
          tags$code(res$error, style = "font-size:12px;")
        ))
      }
      
      # ── stat cards ──────────────────────────────────────────────────────────
      card <- function(label, value, color = "#185FA5") {
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
      
      cards <- if (res$mode == "ridge_lasso") {
        div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:16px;",
            card("Mode",        if (res$alpha == 0) "Ridge" else if (res$alpha == 1) "Lasso" else paste0("α = ", res$alpha), "#185FA5"),
            card("Alpha",       res$alpha,                   "#534AB7"),
            card("lambda.min",  round(res$lambda_min, 6),    "#0F6E56"),
            card("lambda.1se",  round(res$lambda_1se, 6),    "#BA7517"),
            card("CV Method",   toupper(res$cv_method),      "#5F5E5A"),
            card("Folds",       res$nfolds,                  "#5F5E5A"),
            card("CV Time (s)", res$cv_time,                 "#fd7e14")
        )
      } else {
        div(style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:16px;",
            card("Best Alpha",  res$best_alpha,              "#185FA5"),
            card("lambda.min",  round(res$lambda_min, 6),    "#0F6E56"),
            card("lambda.1se",  round(res$lambda_1se, 6),    "#BA7517"),
            card("CV Method",   toupper(res$cv_method),      "#5F5E5A"),
            card("Folds",       res$nfolds,                  "#5F5E5A"),
            card("CV Time (s)", res$cv_time,                 "#fd7e14")
        )
      }
      
      # ── alpha search table (ElasticNet only) ────────────────────────────────
      alpha_table <- if (res$mode == "elasticnet") {
        div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"),
                  "Alpha Search Results",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          DT::dataTableOutput(ns("alpha_tbl"))
        )
      } else NULL
      
      # ── CV error curve ───────────────────────────────────────────────────────
      cv_plot <- div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("chart-line", style = "color:#185FA5; margin-right:6px;"),
                if (res$mode == "elasticnet")
                  paste0("CV Error Curve — Best Alpha (α = ", res$best_alpha, ")")
                else
                  "CV Error Curve",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
        plotly::plotlyOutput(ns("cv_plot"), height = "600px")
      )
      
      tagList(cards, cv_plot, alpha_table)
    })
    
    # ── Alpha search table ─────────────────────────────────────────────────────
    
    output$alpha_tbl <- DT::renderDataTable({
      res <- tune_result()
      req(res, is.null(res$error), res$mode == "elasticnet")
      
      tbl <- data.frame(
        Alpha      = res$alpha_grid,
        Min_CV_MSE = round(res$min_errors, 6),
        stringsAsFactors = FALSE
      )
      tbl$Best <- ifelse(tbl$Alpha == res$best_alpha, "★", "")
      
      DT::datatable(tbl, options = list(pageLength = 15, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Best",
                        color      = "#198754",
                        fontWeight = "bold",
                        fontSize   = "16px")
    })
    
    # ── CV error curve plot ────────────────────────────────────────────────────
    
    output$cv_plot <- plotly::renderPlotly({
      res <- tune_result()
      req(res, is.null(res$error))
      
      cv  <- res$cv_fit
      log_lambda <- -log(cv$lambda)
      
      plotly::plot_ly() |>
        plotly::add_ribbons(
          x    = log_lambda,
          ymin = cv$cvlo,
          ymax = cv$cvup,
          fillcolor = "rgba(24,95,165,0.15)",
          line      = list(color = "transparent"),
          name      = "CV Error Band",
          showlegend = TRUE
        ) |>
        plotly::add_lines(
          x    = log_lambda,
          y    = cv$cvm,
          line = list(color = "#185FA5", width = 2),
          name = "Mean CV Error"
        ) |>
        plotly::add_lines(
          x    = c(-log(res$lambda_min), -log(res$lambda_min)),
          y    = c(min(cv$cvlo), max(cv$cvup)),
          line = list(color = "#198754", dash = "dash", width = 1.5),
          name = paste0("lambda.min (", round(res$lambda_min, 5), ")")
        ) |>
        plotly::add_lines(
          x    = c(-log(res$lambda_1se), -log(res$lambda_1se)),
          y    = c(min(cv$cvlo), max(cv$cvup)),
          line = list(color = "#BA7517", dash = "dash", width = 1.5),
          name = paste0("lambda.1se (", round(res$lambda_1se, 5), ")")
        ) |>
        plotly::layout(
          xaxis  = list(title = "<b>-log(λ)</b>"),
          yaxis  = list(title = "<b>CV MSE</b>"),
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = 1.05, yanchor = "bottom"),
          hovermode = "x unified"
        )
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      tuned_alpha  = reactive({
        res <- tune_result()
        if (is.null(res) || !is.null(res$error)) return(NULL)
        if (res$mode == "elasticnet") res$best_alpha else res$alpha
      }),
      tuned_lambda = reactive({
        res <- tune_result()
        if (is.null(res) || !is.null(res$error)) return(NULL)
        res$lambda_min
      }),
      tune_result  = tune_result
    ))
    
  })
}