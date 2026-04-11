# =================================================================================
# mod_prep_recipe.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

prep_recipe_ui <- function(id) {
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
        HTML("&nbsp; <b>Recipe Builder</b><br><br>
              Define a <b>recipes</b>-based preprocessing pipeline to feed
              into model training. Steps are fitted on train only.")
      ),
      hr(),
      
      actionButton(
        ns("build"),
        label = "Build Recipe",
        icon  = icon("hammer"),
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
          
          # ── Part 1: Data Roles ─────────────────────────────────────────────
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            
            tags$h5(
              icon("tags", style = "color:#185FA5; margin-right:6px;"),
              "Part 1 — Data Roles",
              style = "font-weight:600; color:#343a40; margin-bottom:4px;"
            ),
            div(
              style = "font-size:12px; color:#856404; background:#fff3cd;
                       border-left:3px solid #ffc107; padding:6px 10px;
                       border-radius:4px; margin-bottom:16px;",
              icon("triangle-exclamation", style = "color:#ffc107;"),
              HTML(" <b>Required.</b> You must define the response (Y), predictors,
                    and any columns to exclude. Defaults are pulled from Data Roles config —
                    fall back to <b>(none)</b> if not configured.")
            ),
            
            fluidRow(
              column(4,
                     tags$label("Response (Y):",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectizeInput(ns("y_var"), label = NULL,
                                    choices = NULL, multiple = FALSE,
                                    options = list(placeholder = "Auto-filled from roles..."),
                                    width = "100%")
              ),
              column(4,
                     tags$label("Predictors:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectizeInput(ns("pred_cols"), label = NULL,
                                    choices = NULL, multiple = TRUE,
                                    options = list(placeholder = "Auto-filled from roles..."),
                                    width = "100%")
              ),
              column(4,
                     tags$label("Ignore / Exclude:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     selectizeInput(ns("ignore_cols"), label = NULL,
                                    choices = NULL, multiple = TRUE,
                                    options = list(placeholder = "Auto-filled from roles..."),
                                    width = "100%")
              )
            )
          ),
          
          # ── Part 2: Previously Handled Steps ──────────────────────────────
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            
            tags$h5(
              icon("rotate", style = "color:#185FA5; margin-right:6px;"),
              "Part 2 — Previously Handled Steps",
              style = "font-weight:600; color:#343a40; margin-bottom:4px;"
            ),
            div(
              style = "font-size:12px; color:#6c757d; background:#e8f0fe;
                       border-left:3px solid #a8c0fd; padding:6px 10px;
                       border-radius:4px; margin-bottom:16px;",
              icon("circle-info", style = "color:#185FA5;"),
              HTML(" These steps default to <b>None</b> and are safe to skip if already
                    handled in the Miss Strategy pipeline. Only enable if you want the
                    recipe to handle them instead.")
            ),
            
            fluidRow(
              column(6,
                     tags$label("Imputation:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     radioButtons(ns("impute_method"), label = NULL,
                                  choices = c(
                                    "None (already handled)" = "none",
                                    "KNN"                    = "knn",
                                    "Bagged Trees"           = "bag",
                                    "Mean / Median / Mode"   = "mmm"
                                  ),
                                  selected = "none"),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'knn'", ns("impute_method")),
                       sliderInput(ns("knn_k"), "Neighbours (k):",
                                   min = 1, max = 25, value = 5, step = 1, width = "100%")
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'bag'", ns("impute_method")),
                       sliderInput(ns("bag_trees"), "Number of trees:",
                                   min = 5, max = 50, value = 25, step = 5, width = "100%")
                     )
              ),
              column(6,
                     tags$label("Scaling:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     radioButtons(ns("scale_method"), label = NULL,
                                  choices = c(
                                    "None (already handled)"               = "none",
                                    "Standardise — (x - mean) / sd"        = "standardise",
                                    "Normalise — (x - min) / (max - min)"  = "normalise",
                                    "Centre only — x - mean"               = "centre",
                                    "Scale only — x / sd"                  = "scale"
                                  ),
                                  selected = "none")
              )
            )
          ),
          
          # ── Part 3: Model-Specific Steps ───────────────────────────────────
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            
            tags$h5(
              icon("wand-magic-sparkles", style = "color:#185FA5; margin-right:6px;"),
              "Part 3 — Model-Specific Steps",
              style = "font-weight:600; color:#343a40; margin-bottom:4px;"
            ),
            div(
              style = "font-size:12px; color:#6c757d; background:#fff3e0;
                       border-left:3px solid #fd7e14; padding:6px 10px;
                       border-radius:4px; margin-bottom:16px;",
              icon("triangle-exclamation", style = "color:#fd7e14;"),
              HTML(" Select based on your intended model.
                    <b>Linear models</b> (e.g. glmnet) require dummy encoding and
                    do not handle near-zero variance implicitly.
                    <b>Tree models</b> handle these internally.")
            ),
            
            fluidRow(
              column(4,
                     tags$label("Dummy Encoding:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     checkboxInput(ns("dummy_encode"),
                                   "Dummy encode all nominal predictors",
                                   value = TRUE)
              ),
              column(4,
                     tags$label("Near-Zero Variance:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     checkboxInput(ns("remove_nzv"),
                                   "Remove near-zero variance predictors",
                                   value = FALSE)
              ),
              column(4,
                     tags$label("Linear Combinations:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     checkboxInput(ns("remove_lincomb"),
                                   "Remove linear combinations",
                                   value = FALSE)
              )
            )
          )
        ), # end Configuration tabPanel
        
        # ── Tab 2: Preview ───────────────────────────────────────────────────
        tabPanel(
          title = tagList(icon("eye"), " Preview"),
          style = "padding-top:16px;",
          
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = tagList(icon("code"), " Recipe Code"),
              style = "padding-top:14px;",
              verbatimTextOutput(ns("recipe_code"))
            ),
            tabPanel(
              title = tagList(icon("table"), " prep() Summary"),
              style = "padding-top:14px;",
              verbatimTextOutput(ns("recipe_summary"))
            )
          )
        ) # end Preview tabPanel
        
      ) # end outer tabsetPanel
    )
  )
}


# ── SERVER (placeholder) ──────────────────────────────────────────────────────

prep_recipe_server <- function(id, get_data, roles, split) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # server logic to be implemented
    
    return(list(
      recipe = reactive({ NULL })
    ))
    
  })
}