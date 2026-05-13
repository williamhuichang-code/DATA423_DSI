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
      
      # ── Split ────────────────────────────────────────────────────────────
      tags$label("Split variable:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      selectInput(ns("split_var"), label = NULL,
                  choices = c("(none)"), width = "100%"),
      conditionalPanel(
        condition = paste0("input['", ns("split_var"), "'] != '(none)'"),
        fluidRow(
          column(6,
                 tags$label("Train level:", style = "font-size:12px; font-weight:600; color:#343a40;"),
                 selectInput(ns("train_level"), label = NULL, choices = c("(none)"), width = "100%")
          ),
          column(6,
                 tags$label("Test level:", style = "font-size:12px; font-weight:600; color:#343a40;"),
                 selectInput(ns("test_level"), label = NULL, choices = c("(none)"), width = "100%")
          )
        )
      ),
      hr(),
      
      # ── Mode ──────────────────────────────────────────────────────────────
      tags$label("Mode:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("mode"), label = NULL,
                   choices  = c("Ridge / Lasso" = "ridge_lasso",
                                "ElasticNet"    = "elasticnet"),
                   selected = "elasticnet"),
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
                                   min = 0, max = 1, value = 0, step = 1, width = "100%")
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
              ),
              fluidRow(
                column(6,
                       tags$label("CV error measure:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       selectInput(ns("rl_type_measure"), label = NULL,
                                   choices  = c("MSE (default)"      = "mse",
                                                "MAE"                = "mae",
                                                "Deviance"           = "deviance",
                                                "Misclassification"  = "class",
                                                "AUC"                = "auc"),
                                   selected = "mse", width = "100%")
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
              ),
              fluidRow(
                column(6,
                       tags$label("CV error measure:",
                                  style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                       selectInput(ns("en_type_measure"), label = NULL,
                                   choices  = c("MSE (default)"      = "mse",
                                                "MAE"                = "mae",
                                                "Deviance"           = "deviance",
                                                "Misclassification"  = "class",
                                                "AUC"                = "auc"),
                                   selected = "mse", width = "100%")
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
        ),
        
        # ── Tab 3: Set Parameters ────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("circle-check"), " Set Parameters"),
          style = "padding-top:16px;",
          
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            
            tags$h5(
              icon("circle-check", style = "color:#185FA5; margin-right:6px;"),
              "Set Parameters",
              style = "font-weight:600; color:#343a40; margin-bottom:4px;"
            ),
            div(
              style = "font-size:12px; color:#6c757d; background:#e8f0fe;
                       border-left:3px solid #a8c0fd; padding:6px 10px;
                       border-radius:4px; margin-bottom:16px;",
              icon("circle-info", style = "color:#185FA5;"),
              HTML(" Parameters are <b>auto-filled from CV results</b>. You can manually
                    override them if needed before passing to the Regression module.")
            ),
            
            # ElasticNet hint
            conditionalPanel(
              condition = sprintf("input['%s'] == 'elasticnet'", ns("mode")),
              div(
                style = "font-size:12px; color:#856404; background:#fff3cd;
                         border-left:3px solid #ffc107; padding:6px 10px;
                         border-radius:4px; margin-bottom:16px;",
                icon("triangle-exclamation", style = "color:#ffc107;"),
                HTML(" For ElasticNet, consider training with <b>both lambda.min and lambda.1se</b>
                      and comparing test MSE in the comparison table to decide which generalises better.")
              )
            ),
            
            fluidRow(
              # ── Alpha ────────────────────────────────────────────────────
              column(4,
                     tags$label("Alpha (α):",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     div(style = "font-size:11px; color:#6c757d; margin-bottom:6px;",
                         "0 = Ridge · 1 = Lasso · between = ElasticNet"),
                     sliderInput(ns("set_alpha"), label = NULL,
                                 min = 0, max = 1, value = 0, step = 0.05, width = "100%")
              ),
              
              # ── Lambda ───────────────────────────────────────────────────
              column(4,
                     tags$label("Lambda (λ):",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     radioButtons(ns("lambda_source"), label = NULL,
                                  choices  = c("lambda.min" = "lambda_min",
                                               "lambda.1se" = "lambda_1se",
                                               "Manual"     = "manual"),
                                  selected = "lambda_min"),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'manual'", ns("lambda_source")),
                       numericInput(ns("set_lambda_manual"), label = NULL,
                                    value = 0.01, min = 0, step = 0.001, width = "100%")
                     )
              ),
              
              # ── Resolved value display ────────────────────────────────────
              column(4,
                     tags$label("Resolved values:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     uiOutput(ns("resolved_params_ui"))
              )
            ),
            
            hr(),
            actionButton(
              ns("confirm_params"),
              label = "Confirm Parameters",
              icon  = icon("circle-check"),
              width = "100%",
              style = "background-color:#198754; color:white; border:none;"
            ),
            uiOutput(ns("confirm_feedback_ui"))
          )
        ) # end Set Parameters tabPanel
        
      ) # end tabsetPanel
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

model_tune_server <- function(id, get_data, roles, get_recipe, seed = reactive(42)) {
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
      
      
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      cat("\n── get_xy called with nrow(df):", nrow(df), "\n")
      cat("── recipe outcome var:", y_col, "\n")
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      
      
      prepped   <- prep(rec, training = df, verbose = FALSE)
      baked     <- bake(prepped, new_data = df)
      
      
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      cat("── baked nrow:", nrow(baked), "\n")
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      
      
      y         <- baked[[y_col]]
      
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      cat("── y length:", length(y), "\n")
      cat("── y NAs:", sum(is.na(y)), "\n")
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      
      
      # use only predictor-role columns — excludes ignored cols like CODE, OBS_TYPE
      pred_cols <- prepped$var_info$variable[prepped$var_info$role == "predictor"]
      pred_cols <- intersect(pred_cols, names(baked))
      x_df      <- baked[, pred_cols, drop = FALSE]
      # model.matrix properly expands factors into dummy columns
      old_na <- options(na.action = "na.pass")
      x      <- model.matrix(~ ., data = x_df)[, -1]
      options(old_na)
      
      
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      cat("── x nrow:", nrow(x), "\n")
      # ── DEBUG ─────────────────────────────────────────────────────────────────
      
      
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
            set.seed(seed())
            cv_time <- system.time({
              cv_fit <- cv.glmnet(x_train, y_train,
                                  alpha        = alpha,
                                  lambda       = lambda_grid,
                                  nfolds       = nfolds,
                                  thresh       = thresh,
                                  type.measure = input$rl_type_measure)
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
            
            cv_time <- system.time({
              cv_fits <- lapply(alpha_grid, function(a) {
                set.seed(seed())
                cv.glmnet(x_train, y_train,
                          alpha        = a,
                          lambda       = lambda_grid,
                          nfolds       = nfolds,
                          thresh       = thresh,
                          type.measure = input$en_type_measure)
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
    
    # ── Auto-fill Set Parameters from CV results ──────────────────────────────
    
    observeEvent(tune_result(), {
      res <- tune_result()
      if (is.null(res) || !is.null(res$error)) return()
      
      alpha <- if (res$mode == "elasticnet") res$best_alpha else res$alpha
      updateSliderInput(session, "set_alpha", value = alpha)
      updateRadioButtons(session, "lambda_source", selected = "lambda_min")
    })
    
    # ── Resolved params display ───────────────────────────────────────────────
    
    output$resolved_params_ui <- renderUI({
      res <- tune_result()
      
      lambda_val <- if (is.null(res) || !is.null(res$error)) {
        if (input$lambda_source == "manual") input$set_lambda_manual else NA
      } else {
        switch(input$lambda_source,
               "lambda_min" = res$lambda_min,
               "lambda_1se" = res$lambda_1se,
               "manual"     = input$set_lambda_manual)
      }
      
      badge <- function(label, value, color) {
        div(style = paste0(
          "display:inline-block; background:", color, "22;",
          "color:", color, "; border:1px solid ", color, "55;",
          "border-radius:6px; padding:4px 10px; font-size:13px;",
          "font-weight:600; margin-bottom:6px; margin-right:4px;"
        ), paste0(label, ": ", value))
      }
      
      tagList(
        badge("α", input$set_alpha, "#185FA5"),
        br(),
        badge("λ", if (is.na(lambda_val)) "—" else round(lambda_val, 6), "#0F6E56")
      )
    })
    
    # ── Confirmed params (locked in after button click) ───────────────────────
    
    confirmed <- reactiveVal(list(alpha = NULL, lambda = NULL))
    
    observeEvent(input$confirm_params, {
      res <- tune_result()
      
      lambda_val <- if (is.null(res) || !is.null(res$error)) {
        input$set_lambda_manual
      } else {
        switch(input$lambda_source,
               "lambda_min" = res$lambda_min,
               "lambda_1se" = res$lambda_1se,
               "manual"     = input$set_lambda_manual)
      }
      
      confirmed(list(alpha = input$set_alpha, lambda = lambda_val))
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
        plotOutput(ns("cv_plot"), height = "600px")
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
    
    output$cv_plot <- renderPlot({
      res <- tune_result()
      req(res, is.null(res$error))
      
      title <- if (res$mode == "elasticnet") {
        paste0("ElasticNet (α = ", res$best_alpha, "): CV Error vs Lambda")
      } else if (res$alpha == 0) {
        "Ridge: CV Error vs Lambda"
      } else if (res$alpha == 1) {
        "Lasso: CV Error vs Lambda"
      } else {
        paste0("α = ", res$alpha, ": CV Error vs Lambda")
      }
      
      par(mar = c(5, 5, 6, 2),
          cex.axis = 1.3,
          cex.lab  = 1.4)
      plot(res$cv_fit, main = "")
      title(main = title, line = 4, cex.main = 1.6, font.main = 2)
    })
    
    # ── Confirm feedback ──────────────────────────────────────────────────────
    
    output$confirm_feedback_ui <- renderUI({
      req(input$confirm_params > 0)
      c <- confirmed()
      if (is.null(c$alpha) || is.null(c$lambda)) return(NULL)
      div(
        style = "margin-top:10px; font-size:12px; color:#0f6e56; background:#e1f5ee;
                 border-left:3px solid #9fe1cb; padding:8px 12px; border-radius:6px;",
        icon("circle-check", style = "color:#0f6e56;"),
        HTML(paste0(" <b>Parameters confirmed</b> — α = <b>", c$alpha,
                    "</b>, λ = <b>", round(c$lambda, 6),
                    "</b>. Ready to pass to the Regression module."))
      )
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      tuned_alpha  = reactive({ confirmed()$alpha }),
      tuned_lambda = reactive({ confirmed()$lambda }),
      tune_result  = tune_result
    ))
    
  })
}