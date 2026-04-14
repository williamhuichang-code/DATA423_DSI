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
      uiOutput(ns("build_feedback_ui")),
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


# ── SERVER ───────────────────────────────────────────────────────────────────

prep_recipe_server <- function(id, get_data, roles, split) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate selectors from roles ────────────────────────────────────────
    
    observe({
      req(get_data(), roles())
      df <- get_data()
      r  <- roles()
      
      all_vars <- names(df)
      
      y_default      <- names(r)[r == "outcome"]
      pred_default   <- names(r)[r == "predictor"]
      ignore_default <- names(r)[tolower(as.character(r)) %in%
                                   c("obs_id", "split", "sensitive",
                                     "weight", "stratifier", "ignore")]
      
      # keep only vars that exist in df
      y_default      <- intersect(y_default,      all_vars)
      pred_default   <- intersect(pred_default,   all_vars)
      ignore_default <- intersect(ignore_default, all_vars)
      
      updateSelectizeInput(session, "y_var",
                           choices  = all_vars,
                           selected = if (length(y_default) > 0) y_default[1] else NULL,
                           server   = TRUE)
      updateSelectizeInput(session, "pred_cols",
                           choices  = all_vars,
                           selected = pred_default,
                           server   = TRUE)
      updateSelectizeInput(session, "ignore_cols",
                           choices  = all_vars,
                           selected = ignore_default,
                           server   = TRUE)
    }) |> bindEvent(get_data(), roles())
    
    # ── Build feedback ────────────────────────────────────────────────────────
    
    output$build_feedback_ui <- renderUI({
      rec <- built_recipe()
      if (is.null(rec)) return(NULL)
      if (!is.null(rec$error)) {
        div(
          style = "margin-top:6px; font-size:12px; color:#dc3545; background:#fff5f5;
                   border-left:3px solid #f5c2c7; padding:6px 10px; border-radius:6px;",
          icon("circle-xmark", style = "color:#dc3545;"),
          HTML(" <b>Build failed.</b> Check Preview tab for details.")
        )
      } else {
        div(
          style = "margin-top:6px; font-size:12px; color:#0f6e56; background:#e1f5ee;
                   border-left:3px solid #9fe1cb; padding:6px 10px; border-radius:6px;",
          icon("circle-check", style = "color:#0f6e56;"),
          HTML(" <b>Recipe built.</b> Check Preview tab to inspect steps.")
        )
      }
    })
    
    # ── Reset ─────────────────────────────────────────────────────────────────
    
    observeEvent(input$reset, {
      built_recipe(NULL)
      updateRadioButtons(session, "impute_method", selected = "none")
      updateRadioButtons(session, "scale_method",  selected = "none")
      updateSliderInput(session,  "knn_k",          value = 5)
      updateSliderInput(session,  "bag_trees",      value = 25)
      updateCheckboxInput(session, "dummy_encode",  value = TRUE)
      updateCheckboxInput(session, "remove_nzv",    value = FALSE)
      updateCheckboxInput(session, "remove_lincomb", value = FALSE)
      # re-populate from roles
      req(get_data(), roles())
      df <- get_data()
      r  <- roles()
      all_vars       <- names(df)
      y_default      <- intersect(names(r)[r == "outcome"],   all_vars)
      pred_default   <- intersect(names(r)[r == "predictor"], all_vars)
      ignore_default <- intersect(names(r)[tolower(as.character(r)) %in%
                                             c("obs_id", "split", "sensitive",
                                               "weight", "stratifier", "ignore")], all_vars)
      updateSelectizeInput(session, "y_var",       choices = all_vars, selected = if (length(y_default) > 0) y_default[1] else NULL, server = TRUE)
      updateSelectizeInput(session, "pred_cols",   choices = all_vars, selected = pred_default,   server = TRUE)
      updateSelectizeInput(session, "ignore_cols", choices = all_vars, selected = ignore_default, server = TRUE)
    })
    
    # ── Built recipe (reactive, triggered by Build button) ────────────────────
    
    built_recipe <- reactiveVal(NULL)
    
    observeEvent(input$build, {
      df   <- get_data(); req(df)
      y    <- input$y_var
      preds <- input$pred_cols
      
      req(nzchar(y), length(preds) > 0)
      
      tryCatch({
        library(recipes)
        
        # use train split if available, otherwise full df
        train_df <- tryCatch(split$train(), error = function(e) df)
        if (is.null(train_df) || nrow(train_df) == 0) train_df <- df
        
        rec <- recipe(as.formula(paste(y, "~ .")), data = train_df)
        
        # update roles for ignored columns
        excl <- unique(c(input$ignore_cols))
        excl <- intersect(excl, names(train_df))
        excl <- setdiff(excl, c(y, preds))
        if (length(excl) > 0)
          rec <- rec |> update_role(all_of(excl), new_role = "ignore")
        
        # remove non-predictor, non-outcome cols from recipe scope
        keep <- union(preds, y)
        drop <- setdiff(names(train_df), keep)
        drop <- setdiff(drop, excl)  # already handled above
        if (length(drop) > 0)
          rec <- rec |> update_role(all_of(drop), new_role = "ignore")
        
        # ── Part 2: imputation ────────────────────────────────────────────
        rec <- switch(input$impute_method,
                      "knn" = rec |> step_impute_knn(all_predictors(), neighbors = input$knn_k),
                      "bag" = rec |> step_impute_bag(all_predictors(), trees = input$bag_trees),
                      "mmm" = rec |> step_impute_mode(all_nominal_predictors()) |>
                        step_impute_mean(all_numeric_predictors()),
                      rec  # none
        )
        
        # ── Part 2: scaling ───────────────────────────────────────────────
        rec <- switch(input$scale_method,
                      "standardise" = rec |> step_center(all_numeric_predictors()) |>
                        step_scale(all_numeric_predictors()),
                      "normalise"   = rec |> step_range(all_numeric_predictors()),
                      "centre"      = rec |> step_center(all_numeric_predictors()),
                      "scale"       = rec |> step_scale(all_numeric_predictors()),
                      rec  # none
        )
        
        # ── Part 3: model-specific ────────────────────────────────────────
        if (isTRUE(input$remove_nzv))
          rec <- rec |> step_nzv(all_predictors())
        if (isTRUE(input$remove_lincomb))
          rec <- rec |> step_lincomb(all_numeric_predictors())
        if (isTRUE(input$dummy_encode))
          rec <- rec |> step_dummy(all_nominal_predictors())
        
        built_recipe(rec)
        
      }, error = function(e) {
        built_recipe(list(error = conditionMessage(e)))
      })
    })
    
    # ── Recipe code preview ───────────────────────────────────────────────────
    
    output$recipe_code <- renderPrint({
      rec <- built_recipe()
      
      if (is.null(rec)) {
        cat("# Click 'Build Recipe' to generate the recipe.\n")
        return(invisible(NULL))
      }
      if (!is.null(rec$error)) {
        cat("# Error building recipe:\n#", rec$error, "\n")
        return(invisible(NULL))
      }
      
      # build human-readable recipe code string
      y     <- input$y_var
      preds <- input$pred_cols
      excl  <- input$ignore_cols
      
      lines <- c(
        paste0('rec <- recipe(', y, ' ~ ., data = train)'),
        if (length(excl) > 0)
          paste0('  |> update_role(c("', paste(excl, collapse = '", "'), '"), new_role = "ignore")')
      )
      
      if (input$impute_method == "knn")
        lines <- c(lines, paste0('  |> step_impute_knn(all_predictors(), neighbors = ', input$knn_k, ')'))
      else if (input$impute_method == "bag")
        lines <- c(lines, paste0('  |> step_impute_bag(all_predictors(), trees = ', input$bag_trees, ')'))
      else if (input$impute_method == "mmm")
        lines <- c(lines,
                   '  |> step_impute_mode(all_nominal_predictors())',
                   '  |> step_impute_mean(all_numeric_predictors())')
      
      if (input$scale_method == "standardise")
        lines <- c(lines,
                   '  |> step_center(all_numeric_predictors())',
                   '  |> step_scale(all_numeric_predictors())')
      else if (input$scale_method == "normalise")
        lines <- c(lines, '  |> step_range(all_numeric_predictors())')
      else if (input$scale_method == "centre")
        lines <- c(lines, '  |> step_center(all_numeric_predictors())')
      else if (input$scale_method == "scale")
        lines <- c(lines, '  |> step_scale(all_numeric_predictors())')
      
      if (isTRUE(input$remove_nzv))
        lines <- c(lines, '  |> step_nzv(all_predictors())')
      if (isTRUE(input$remove_lincomb))
        lines <- c(lines, '  |> step_lincomb(all_numeric_predictors())')
      if (isTRUE(input$dummy_encode))
        lines <- c(lines, '  |> step_dummy(all_nominal_predictors())')
      
      cat(paste(lines, collapse = "\n"), "\n")
    })
    
    # ── prep() summary ────────────────────────────────────────────────────────
    
    output$recipe_summary <- renderPrint({
      rec <- built_recipe()
      
      if (is.null(rec)) {
        cat("Click 'Build Recipe' first.\n")
        return(invisible(NULL))
      }
      if (!is.null(rec$error)) {
        cat("Error:", rec$error, "\n")
        return(invisible(NULL))
      }
      
      tryCatch({
        df       <- get_data()
        train_df <- tryCatch(split$train(), error = function(e) df)
        if (is.null(train_df) || nrow(train_df) == 0) train_df <- df
        prepped  <- prep(rec, training = train_df, verbose = FALSE)
        print(summary(prepped))
        cat("\n── Baked dimensions ────────────────────\n")
        baked <- bake(prepped, new_data = NULL)
        cat(sprintf("Rows : %d\nCols : %d\n", nrow(baked), ncol(baked)))
        cat("\nColumn types after baking:\n")
        type_tbl <- table(sapply(baked, function(x) class(x)[1]))
        for (nm in names(type_tbl))
          cat(sprintf("  %-12s : %d\n", nm, type_tbl[[nm]]))
        
        # ── NZV and lincomb removals ───────────────────────────────────────
        removals_found <- FALSE
        for (step in prepped$steps) {
          if (inherits(step, "step_nzv") || inherits(step, "step_lincomb")) {
            if (!removals_found) {
              cat("\n── Column Removals ──────────────────────\n")
              removals_found <- TRUE
            }
            step_name <- class(step)[1]
            removed   <- step$removals
            if (length(removed) == 0) {
              cat(sprintf("%-20s : none removed\n", step_name))
            } else {
              cat(sprintf("%-20s : %d removed\n", step_name, length(removed)))
              cat("  ", paste(removed, collapse = ", "), "\n")
            }
          }
        }
      }, error = function(e) {
        cat("Could not prep recipe:\n", conditionMessage(e), "\n")
      })
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      recipe = reactive({
        rec <- built_recipe()
        if (is.null(rec) || !is.null(rec$error)) return(NULL)
        rec
      })
    ))
    
  })
}