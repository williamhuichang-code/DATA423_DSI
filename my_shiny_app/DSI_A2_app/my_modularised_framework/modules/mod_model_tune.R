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


# ── SERVER (placeholder) ──────────────────────────────────────────────────────

model_tune_server <- function(id, get_data, roles, precipe) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # server logic to be implemented
    
    return(list(
      best_alpha  = reactive({ NULL }),
      best_lambda = reactive({ NULL })
    ))
    
  })
}