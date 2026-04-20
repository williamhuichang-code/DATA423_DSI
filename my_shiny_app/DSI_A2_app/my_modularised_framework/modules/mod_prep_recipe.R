# =================================================================================
# mod_prep_recipe.R
# =================================================================================

library(recipes)
library(glmnet)


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
              Define a <b>recipes</b>-based preprocessing pipeline.
              All steps are <b>fitted on train only</b> and applied to
              both train and test to prevent data leakage.")
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
      uiOutput(ns("split_counts_ui")),
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
              HTML(" <b>Required.</b> Define the response (Y), predictors, and columns
                    to exclude. Auto-filled from Data Roles — override if needed.")
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
          
          # ── Part 2: Imputation & Scaling ───────────────────────────────────
          div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:20px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            
            tags$h5(
              icon("rotate", style = "color:#185FA5; margin-right:6px;"),
              "Part 2 — Imputation & Scaling",
              style = "font-weight:600; color:#343a40; margin-bottom:4px;"
            ),
            div(
              style = "font-size:12px; color:#6c757d; background:#e8f0fe;
                       border-left:3px solid #a8c0fd; padding:6px 10px;
                       border-radius:4px; margin-bottom:16px;",
              icon("circle-info", style = "color:#185FA5;"),
              HTML(" All steps are <b>fitted on train only</b> and applied to both
                    train and test. Imputation is required if missing values remain.
                    Scaling is recommended for linear models (Ridge, Lasso, ElasticNet)
                    but not needed for tree-based models.")
            ),
            
            fluidRow(
              column(6,
                     tags$label("Imputation:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     radioButtons(ns("impute_method"), label = NULL,
                                  choices = c(
                                    "None"         = "none",
                                    "KNN"          = "knn",
                                    "Bagged Trees" = "bag",
                                    "Mean / Median / Mode" = "mmm"
                                  ),
                                  selected = "bag"),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'knn'", ns("impute_method")),
                       sliderInput(ns("knn_k"), "Neighbours (k):",
                                   min = 1, max = 25, value = 5, step = 1, width = "100%")
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'bag'", ns("impute_method")),
                       sliderInput(ns("bag_trees"), "Number of trees:",
                                   min = 2, max = 50, value = 4, step = 1, width = "100%")
                     )
              ),
              column(6,
                     tags$label("Scaling:",
                                style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
                     radioButtons(ns("scale_method"), label = NULL,
                                  choices = c(
                                    "None"                                 = "none",
                                    "Standardise — (x - mean) / sd"        = "standardise",
                                    "Normalise — (x - min) / (max - min)"  = "normalise",
                                    "Centre only — x - mean"               = "centre",
                                    "Scale only — x / sd"                  = "scale"
                                  ),
                                  selected = "standardise")
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

prep_recipe_server <- function(id, get_data, roles, seed = reactive(42)) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Helper: resolve train df from split settings ──────────────────────────
    
    get_train_df <- function(df) {
      sv <- input$split_var
      tl <- input$train_level
      if (!is.null(sv) && sv != "(none)" && sv %in% names(df) &&
          !is.null(tl) && tl != "(none)") {
        df[as.character(df[[sv]]) == tl, , drop = FALSE]
      } else {
        df
      }
    }
    
    # ── Populate split selectors from roles ───────────────────────────────────
    
    observe({
      req(get_data(), roles())
      r    <- roles()
      df   <- get_data()
      vars <- names(df)
      sv   <- names(r)[r == "split"]
      
      updateSelectInput(session, "split_var",
                        choices  = c("(none)", vars),
                        selected = if (length(sv) > 0 && sv[1] %in% vars) sv[1] else "(none)")
    })
    
    observe({
      req(input$split_var, input$split_var != "(none)")
      df   <- get_data(); req(df)
      lvls <- as.character(unique(df[[input$split_var]]))
      lvls <- lvls[!is.na(lvls)]
      train_sel <- if ("Train" %in% lvls) "Train" else lvls[1]
      test_sel  <- if ("Test"  %in% lvls) "Test"  else if (length(lvls) >= 2) lvls[2] else lvls[1]
      updateSelectInput(session, "train_level", choices = c("(none)", lvls), selected = train_sel)
      updateSelectInput(session, "test_level",  choices = c("(none)", lvls), selected = test_sel)
    })
    
    # ── Split counts hint ─────────────────────────────────────────────────────
    
    output$split_counts_ui <- renderUI({
      sv <- input$split_var
      tl <- input$train_level
      ts <- input$test_level
      req(!is.null(sv) && sv != "(none)")
      df <- get_data(); req(df)
      req(sv %in% names(df))
      
      n_train <- if (!is.null(tl) && tl != "(none)") sum(as.character(df[[sv]]) == tl, na.rm = TRUE) else NA
      n_test  <- if (!is.null(ts) && ts != "(none)") sum(as.character(df[[sv]]) == ts, na.rm = TRUE) else NA
      
      div(
        style = "font-size:12px; color:#185FA5; background:#E6F1FB;
                 border-radius:6px; padding:6px 10px; margin-top:8px;",
        icon("circle-info"),
        HTML(paste0(" Train: <b>", if (is.na(n_train)) "—" else n_train,
                    "</b> &nbsp;|&nbsp; Test: <b>",
                    if (is.na(n_test))  "—" else n_test, "</b>"))
      )
    })
    
    # ── Populate role selectors ───────────────────────────────────────────────
    
    observe({
      req(get_data(), roles())
      df <- get_data()
      r  <- roles()
      
      all_vars <- names(df)
      
      y_default      <- intersect(names(r)[r == "outcome"],   all_vars)
      pred_default   <- intersect(names(r)[r == "predictor"], all_vars)
      ignore_default <- intersect(
        names(r)[tolower(as.character(r)) %in%
                   c("obs_id", "split", "sensitive", "weight", "stratifier", "ignore")],
        all_vars
      )
      
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
      updateRadioButtons(session, "impute_method",  selected = "bag")
      updateRadioButtons(session, "scale_method",   selected = "standardise")
      updateSliderInput(session,  "knn_k",           value = 5)
      updateSliderInput(session,  "bag_trees",       value = 4)
      updateCheckboxInput(session, "dummy_encode",   value = TRUE)
      updateCheckboxInput(session, "remove_nzv",     value = FALSE)
      updateCheckboxInput(session, "remove_lincomb", value = FALSE)
      
      req(get_data(), roles())
      df <- get_data()
      r  <- roles()
      all_vars       <- names(df)
      y_default      <- intersect(names(r)[r == "outcome"],   all_vars)
      pred_default   <- intersect(names(r)[r == "predictor"], all_vars)
      ignore_default <- intersect(
        names(r)[tolower(as.character(r)) %in%
                   c("obs_id", "split", "sensitive", "weight", "stratifier", "ignore")],
        all_vars
      )
      updateSelectizeInput(session, "y_var",
                           choices = all_vars,
                           selected = if (length(y_default) > 0) y_default[1] else NULL,
                           server = TRUE)
      updateSelectizeInput(session, "pred_cols",   choices = all_vars, selected = pred_default,   server = TRUE)
      updateSelectizeInput(session, "ignore_cols", choices = all_vars, selected = ignore_default, server = TRUE)
    })
    
    # ── Built recipe (triggered by Build button) ──────────────────────────────
    
    built_recipe <- reactiveVal(NULL)
    
    observeEvent(input$build, {
      df    <- get_data(); req(df)
      y     <- input$y_var
      preds <- input$pred_cols
      
      req(nzchar(y), length(preds) > 0)
      
      tryCatch({
        set.seed(seed())
        
        # ── use train rows only ───────────────────────────────────────────
        train_df <- get_train_df(df)
        
        rec <- recipe(as.formula(paste(y, "~ .")), data = train_df)
        
        # update roles for ignored columns
        excl <- unique(input$ignore_cols)
        excl <- intersect(excl, names(train_df))
        excl <- setdiff(excl, c(y, preds))
        if (length(excl) > 0)
          rec <- rec |> update_role(all_of(excl), new_role = "ignore")
        
        # drop non-predictor non-outcome cols
        keep <- union(preds, y)
        drop <- setdiff(names(train_df), keep)
        drop <- setdiff(drop, excl)
        if (length(drop) > 0)
          rec <- rec |> update_role(all_of(drop), new_role = "ignore")
        
        # ── Part 2: imputation ────────────────────────────────────────────
        rec <- switch(input$impute_method,
                      "knn" = rec |> step_impute_knn(all_predictors(), neighbors = input$knn_k),
                      "bag" = rec |> step_impute_bag(all_predictors(), trees = input$bag_trees),
                      "mmm" = rec |>
                        step_impute_mode(all_nominal_predictors()) |>
                        step_impute_mean(all_numeric_predictors()),
                      rec  # none
        )
        
        # ── Part 2: scaling ───────────────────────────────────────────────
        rec <- switch(input$scale_method,
                      "standardise" = rec |>
                        step_center(all_numeric_predictors()) |>
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
          rec <- rec |>
          step_unknown(all_nominal_predictors()) |>   # handle unseen factor levels
          step_dummy(all_nominal_predictors())
        
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
      
      y    <- input$y_var
      excl <- input$ignore_cols
      
      lines <- c(
        paste0('rec <- recipe(', y, ' ~ ., data = train_df)')
      )
      
      if (length(excl) > 0)
        lines <- c(lines, paste0('  |> update_role(c("', paste(excl, collapse = '", "'), '"), new_role = "ignore")'))
      
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
        lines <- c(lines,
                   '  |> step_unknown(all_nominal_predictors())',
                   '  |> step_dummy(all_nominal_predictors())')
      
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
        train_df <- get_train_df(df)
        
        set.seed(seed())
        prepped <- prep(rec, training = train_df, verbose = FALSE)
        print(summary(prepped))
        
        cat("\n── Train dimensions (after baking) ─────\n")
        baked_train <- bake(prepped, new_data = NULL)
        cat(sprintf("Rows : %d\nCols : %d\n", nrow(baked_train), ncol(baked_train)))
        
        # test dimensions if split is set
        sv <- input$split_var
        ts <- input$test_level
        if (!is.null(sv) && sv != "(none)" && !is.null(ts) && ts != "(none)" && sv %in% names(df)) {
          test_df    <- df[as.character(df[[sv]]) == ts, , drop = FALSE]
          baked_test <- bake(prepped, new_data = test_df)
          cat(sprintf("\n── Test dimensions (after baking) ──────\n"))
          cat(sprintf("Rows : %d\nCols : %d\n", nrow(baked_test), ncol(baked_test)))
        }
        
        cat("\nColumn types after baking:\n")
        type_tbl <- table(sapply(baked_train, function(x) class(x)[1]))
        for (nm in names(type_tbl))
          cat(sprintf("  %-12s : %d\n", nm, type_tbl[[nm]]))
        
        # NZV and lincomb removals
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