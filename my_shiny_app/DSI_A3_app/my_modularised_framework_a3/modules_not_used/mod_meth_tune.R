# =================================================================================
# mod_meth_tune.R
# =================================================================================
# LEGACY: monolithic category module kept for reference only.
# The app now uses the per-category modules in modules/method/category/.
# Global utilities (startMode, stopMode, description, saveToRds, loadRds,
# deleteRds, dynamicSteps, .section_hdr, etc.) live in mod_meth_shared.R.
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_tune_ui <- function(id,
                         methods            = character(0),
                         model_seed         = NULL,
                         general_preprocess  = NULL,
                         glmnet_preprocess   = NULL,
                         pls_preprocess      = NULL,
                         rpart_preprocess    = NULL,
                         pp_choices          = character(0)) {
  ns <- NS(id)
  pc <- pp_choices
  
  # ── per-method sub-tabs factory ───────────────────────────────────────────
  method_subtabs <- function(m) {
    has_tuning <- m != "null"
    
    tabs <- list(
      tabPanel(
        title = tagList(icon("list"), " Summary"),
        style = "padding-top:14px;",
        verbatimTextOutput(ns(paste0(m, "_desc"))),
        hr(),
        tags$h6("Resampled performance",
                style = "font-weight:700; color:#343a40; margin-bottom:8px;"),
        tableOutput(ns(paste0(m, "_metrics")))
      )
    )
    
    if (has_tuning) {
      tabs <- c(tabs, list(
        tabPanel(
          title = tagList(icon("chart-line"), " Tuning"),
          style = "padding-top:14px;",
          plotOutput(ns(paste0(m, "_tune_plot")), width = "100%", height = "640px")
        )
      ))
    }
    
    tabs <- c(tabs, list(
      tabPanel(
        title = tagList(icon("utensils"), " Recipe"),
        style = "padding-top:14px;",
        uiOutput(ns(paste0(m, "_recipe_html"))),
        hr(),
        tableOutput(ns(paste0(m, "_recipe_table")))
      ),
      tabPanel(
        title = tagList(icon("terminal"), " Model Output"),
        style = "padding-top:14px;",
        fluidRow(
          column(6,
                 tags$h6("Training summary",
                         style = "font-weight:700; color:#343a40; margin-bottom:8px;"),
                 verbatimTextOutput(ns(paste0(m, "_train_summary")))
          ),
          column(6,
                 tags$h6("Coefficients",
                         style = "font-weight:700; color:#343a40; margin-bottom:8px;"),
                 tableOutput(ns(paste0(m, "_coef")))
          )
        )
      )
    ))
    
    do.call(tabsetPanel, c(list(type = "tabs"), tabs))
  }
  
  # ── sidebar ───────────────────────────────────────────────────────────────
  sidebar <- div(
    style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
             min-height:100vh; padding:15px; font-size:13px;",
    
    div(
      style = "background:white; padding:10px; border-left:4px solid #0d6efd;
               border-radius:6px; margin-bottom:12px; font-size:13px; color:#343a40;
               box-shadow:0 1px 2px rgba(0,0,0,0.05);",
      icon("circle-info", style = "color:#0d6efd;"),
      HTML("&nbsp;<b>Training controls:</b><br><br>
            Choose preprocessing, tune settings, then train or load the selected model.")
    ),
    
    # ── Action buttons ────────────────────────────────────────────────────────
    actionButton(ns("train"), "Train", icon = icon("play"),        width = "100%",
                 style = "background-color:#185FA5; color:white; border:none; margin-bottom:6px;"),
    actionButton(ns("load"),  "Load",  icon = icon("folder-open"), width = "100%",
                 style = "margin-bottom:6px;"),
    actionButton(ns("delete"),"Delete",icon = icon("trash"),       width = "100%",
                 style = "background-color:#dc3545; color:white; border:none; margin-bottom:6px;"),
    uiOutput(ns("action_feedback")),
    
    hr(),
    
    # ── Training Setup ────────────────────────────────────────────────────────
    .section_hdr("Training Setup", "#e9ecef", "#6c757d"),
    
    tags$label("Train/resampling seed:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    numericInput(ns("train_seed"), NULL,
                 value = model_seed %||% 2026, min = 0, step = 1, width = "100%"),
    checkboxInput(ns("follow_global_seed"), "Follow global seed", value = FALSE),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("follow_global_seed")),
      uiOutput(ns("global_seed_display"))
    ),
    
    hr(),
    
    checkboxInput(ns("parallel"), "Use parallel processing", value = TRUE),
    
    tags$label("Resampling method:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    selectInput(ns("resample_method"), NULL,
                choices  = c("Bootstrap"                  = "boot",
                             "Cross-validation"           = "cv",
                             "Repeated cross-validation"  = "repeatedcv"),
                selected = "boot", width = "100%"),
    
    conditionalPanel(
      condition = sprintf("input['%s'] === 'boot'", ns("resample_method")),
      tags$label("Bootstrap resamples:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("boot_n"), NULL, min = 5, max = 50, value = 25, step = 5, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] === 'cv'", ns("resample_method")),
      tags$label("Number of folds:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("cv_folds"), NULL, min = 3, max = 20, value = 10, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] === 'repeatedcv'", ns("resample_method")),
      tags$label("Number of folds:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("rcv_folds"),   NULL, min = 2, max = 10, value = 4, step = 1, width = "100%"),
      tags$label("Repeats:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("rcv_repeats"), NULL, min = 1, max = 10, value = 6, step = 1, width = "100%")
    ),
    
    tags$label("Search type:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    selectInput(ns("search_type"), NULL,
                choices  = c("Grid search"   = "grid",
                             "Random search" = "random"),
                selected = "grid", width = "100%"),
    
    hr(),
    
    # ── Config Mode ───────────────────────────────────────────────────────────
    .section_hdr("Config Mode", "#fff3cd", "#ffc107"),
    
    radioButtons(ns("config_mode"), NULL,
                 choices  = c("General configs"       = "general",
                              "Model specific configs" = "specific"),
                 selected = "general"),
    
    hr(),
    
    # ── Preprocessing ─────────────────────────────────────────────────────────
    .section_hdr("Preprocessing", "#d1ecf1", "#17a2b8"),
    
    tags$label("Pre-processing:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    selectizeInput(ns("preprocess"), NULL,
                   choices  = pc,
                   selected = general_preprocess %||% character(0),
                   multiple = TRUE,
                   options  = list(plugins = list("remove_button"),
                                   placeholder = "Select preprocessing steps..."),
                   width = "100%"),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('impute_bag')", ns("preprocess")),
      tags$label("Bagged imputation trees:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("impute_bag_trees"), NULL, min = 2, max = 50, value = 4, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('impute_knn')", ns("preprocess")),
      tags$label("KNN imputation neighbours (k):",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("impute_knn_neighbors"), NULL, min = 1, max = 20, value = 5, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('nzv')", ns("preprocess")),
      tags$label("NZV frequency cut:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      numericInput(ns("nzv_freq_cut"), NULL, value = 19, min = 1, step = 1, width = "100%"),
      tags$label("NZV unique cut:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      numericInput(ns("nzv_unique_cut"), NULL, value = 10, min = 1, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('other')", ns("preprocess")),
      tags$label("Rare level threshold:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("other_threshold"), NULL,
                  min = 0.01, max = 0.2, value = 0.05, step = 0.01, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('interact')", ns("preprocess")),
      tags$label("Interaction terms:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      selectInput(ns("interact_type"), NULL,
                  choices  = c("All encoded predictors" = "all",
                               "Numeric × Numeric" = "numeric_numeric",
                               "Nominal × Numeric" = "nominal_numeric",
                               "Nominal × Nominal" = "nominal_nominal"),
                  selected = "all", width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('pca')", ns("preprocess")),
      tags$label("PCA components:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("pca_num_comp"), NULL, min = 1, max = 100, value = 25, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('pls')", ns("preprocess")),
      tags$label("PLS components:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("pls_num_comp"), NULL, min = 1, max = 100, value = 25, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('ica')", ns("preprocess")),
      tags$label("ICA components:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("ica_num_comp"), NULL, min = 1, max = 100, value = 25, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('poly')", ns("preprocess")),
      tags$label("Polynomial degree:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("poly_degree"), NULL, min = 2, max = 10, value = 2, step = 1, width = "100%")
    ),
    
    conditionalPanel(
      condition = sprintf("(input['%s'] || []).includes('corr')", ns("preprocess")),
      tags$label("Correlation threshold:",
                 style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
      sliderInput(ns("corr_threshold"), NULL, min = 0.1, max = 0.99, value = 0.9, step = 0.01, width = "100%")
    ),
    
    tags$label("Tune length:",
               style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
    sliderInput(ns("tune_length"), NULL, min = 1, max = 50, value = 5, step = 1, width = "100%"),
    
    hr(),
    
    # ── Model Specific Configs (conditional) ──────────────────────────────────
    conditionalPanel(
      condition = sprintf("input['%s'] === 'specific'", ns("config_mode")),
      
      .section_hdr("Model Specific Configs", "#f8d7da", "#dc3545"),
      
      # glmnet only (shown when method_inner tab is glmnet, or single-method glmnet)
      conditionalPanel(
        condition = sprintf("input['%s'] === 'glmnet'", ns("method_inner")),
        
        tags$label("Penalty type:",
                   style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
        selectInput(ns("glmnet_penalty"), NULL,
                    choices  = c("Elastic net (tune alpha + lambda)" = "elasticnet",
                                 "Ridge (alpha = 0)"                  = "ridge",
                                 "Lasso (alpha = 1)"                  = "lasso"),
                    selected = "elasticnet", width = "100%"),
        
        tags$label("Tuning grid:",
                   style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
        selectInput(ns("glmnet_grid_type"), NULL,
                    choices  = c("Tune length default"   = "tunelength",
                                 "Custom alpha/lambda grid" = "custom"),
                    selected = "tunelength", width = "100%"),
        
        # custom grid controls
        conditionalPanel(
          condition = sprintf("input['%s'] === 'custom'", ns("glmnet_grid_type")),
          
          conditionalPanel(
            condition = sprintf("input['%s'] === 'elasticnet'", ns("glmnet_penalty")),
            tags$label("Alpha minimum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("glmnet_alpha_min"),  NULL, min = 0,    max = 1,   value = 0.1,  step = 0.05, width = "100%"),
            tags$label("Alpha maximum:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("glmnet_alpha_max"),  NULL, min = 0,    max = 1,   value = 1.0,  step = 0.05, width = "100%"),
            tags$label("Alpha step:",
                       style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
            sliderInput(ns("glmnet_alpha_step"), NULL, min = 0.05, max = 0.5, value = 0.1,  step = 0.05, width = "100%")
          ),
          
          tags$label("Log10 lambda minimum:",
                     style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          sliderInput(ns("glmnet_log_lam_min"), NULL, min = -6,  max = 2,   value = -3,  step = 0.5,  width = "100%"),
          tags$label("Log10 lambda maximum:",
                     style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          sliderInput(ns("glmnet_log_lam_max"), NULL, min = -2,  max = 6,   value = 3,   step = 0.5,  width = "100%"),
          tags$label("Lambda values:",
                     style = "font-weight:600; color:#343a40; display:block; margin-bottom:4px;"),
          sliderInput(ns("glmnet_lam_n"),       NULL, min = 10,  max = 100, value = 50,  step = 5,    width = "100%")
        )
      ),
      
    ),
    
    hr(),
    
  )
  
  # ── method labels ─────────────────────────────────────────────────────────
  method_labels <- c(null = "NULL", glmnet = "GLMnet", pls = "PLS", rpart = "Rpart")
  
  # ── main content area (single method or nested tabs) ──────────────────────
  main_content <- if (length(methods) == 0) {
    tags$p("No methods configured yet.",
           style = "color:#adb5bd; font-style:italic; padding-top:14px;")
  } else if (length(methods) == 1) {
    div(style = "padding-top:12px;", method_subtabs(methods[[1]]))
  } else {
    tabs <- lapply(methods, function(m) {
      tabPanel(method_labels[[m]] %||% m, value = m,
               style = "padding-top:12px;", method_subtabs(m))
    })
    do.call(tabsetPanel, c(list(type = "tabs", id = ns("method_inner")), tabs))
  }
  
  # ── assembled layout ──────────────────────────────────────────────────────
  fluidRow(
    column(9, main_content),
    column(3, sidebar)
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_tune_server <- function(id, get_data, roles, get_raw,
                             methods            = character(0),
                             seed               = reactive(2026),
                             model_seed         = NULL,
                             general_preprocess  = NULL,
                             glmnet_preprocess   = NULL,
                             pls_preprocess      = NULL,
                             rpart_preprocess    = NULL,
                             pp_choices          = character(0)) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Shared state ─────────────────────────────────────────────────────────
    
    models <- reactiveValues()
    
    # ── Effective seed ────────────────────────────────────────────────────────
    
    effective_seed <- reactive({
      if (isTRUE(input$follow_global_seed)) seed() else input$train_seed
    })
    
    output$global_seed_display <- renderUI({
      s <- seed()
      div(
        style = "font-size:12px; color:#0f6e56; background:#e1f5ee;
                 border-left:3px solid #9fe1cb; padding:5px 10px;
                 border-radius:4px; margin-top:2px; margin-bottom:4px;",
        icon("circle-check", style = "color:#0f6e56;"),
        HTML(paste0(" Using global seed: <b>", s, "</b>"))
      )
    })
    
    # ── Current method (single method or derived from nested tab) ────────────
    
    current_method <- reactive({
      if (length(methods) == 0) return(NULL)
      if (length(methods) == 1) return(methods[[1]])
      input$method_inner %||% methods[[1]]
    })
    
    # ── Split data ────────────────────────────────────────────────────────────
    
    get_train <- reactive({
      df <- get_data(); req(df)
      r  <- roles();   req(r)
      sc <- names(r)[r == "split"]
      if (length(sc) == 0) return(df)
      df[as.character(df[[sc[1]]]) %in% c("Train", "train"), , drop = FALSE]
    })
    
    get_test <- reactive({
      df <- get_data(); req(df)
      r  <- roles();   req(r)
      sc <- names(r)[r == "split"]
      if (length(sc) == 0) return(NULL)
      df[as.character(df[[sc[1]]]) %in% c("Test", "test", "Validation", "val"), , drop = FALSE]
    })
    
    # ── trainControl builder ──────────────────────────────────────────────────
    
    build_tr_control <- function(train_df) {
      method <- input$resample_method %||% "boot"
      search <- input$search_type %||% "grid"
      eseed  <- effective_seed()
      r      <- roles()
      y      <- train_df[[names(r)[r == "outcome"][1]]]
      
      set.seed(eseed)
      
      if (method == "cv") {
        n    <- input$cv_folds %||% 10
        reps <- NA
        idx  <- caret::createFolds(y = y, k = n, returnTrain = TRUE)
      } else if (method == "repeatedcv") {
        n    <- input$rcv_folds %||% 4
        reps <- input$rcv_repeats %||% 6
        idx  <- caret::createMultiFolds(y = y, k = n, times = reps)
      } else {
        method <- "boot"
        n      <- input$boot_n %||% 25
        reps   <- NA
        idx    <- caret::createResample(y = y, times = n)
      }
      
      trainControl(
        method          = method,
        number          = n,
        repeats         = reps,
        allowParallel   = isTRUE(input$parallel),
        search          = search,
        index           = idx,
        savePredictions = "final",
        trim            = TRUE
      )
    }
    
    # ── cfg for dynamicSteps ──────────────────────────────────────────────────
    
    get_cfg <- reactive({
      list(
        impute_bag_trees     = input$impute_bag_trees,
        impute_knn_neighbors = input$impute_knn_neighbors,
        nzv_freq_cut         = input$nzv_freq_cut,
        nzv_unique_cut       = input$nzv_unique_cut,
        other_threshold      = input$other_threshold,
        interact_type        = input$interact_type,
        pca_num_comp         = input$pca_num_comp,
        pls_num_comp         = input$pls_num_comp,
        ica_num_comp         = input$ica_num_comp,
        poly_degree          = input$poly_degree,
        corr_threshold       = input$corr_threshold
      )
    })
    
    # ── Recipe builder ────────────────────────────────────────────────────────
    
    build_recipe <- function(train_df, preprocess_steps, is_null = FALSE) {
      r           <- roles()
      outcome_col <- names(r)[r == "outcome"][1]
      form        <- as.formula(paste0("`", outcome_col, "` ~ ."))
      rec         <- recipes::recipe(form, data = train_df)
      
      if (is_null) return(rec)
      
      # remove non-predictor role columns before preprocessing
      drop_roles <- c("split", "obs_id", "ignore", "sensitive", "stratifier", "weight")
      drop_cols  <- intersect(names(r)[r %in% drop_roles], names(train_df))
      if (length(drop_cols) > 0)
        rec <- recipes::step_rm(rec, tidyselect::any_of(drop_cols))
      
      rec <- dynamicSteps(rec, preprocess_steps, cfg = get_cfg())
      rec <- recipes::step_rm(rec, recipes::has_type("date"))
      rec
    }
    
    # ── glmnet tuning grid ────────────────────────────────────────────────────
    
    build_glmnet_grid <- function() {
      penalty <- input$glmnet_penalty
      alpha_g <- switch(penalty,
                        "ridge"      = 0,
                        "lasso"      = 1,
                        "elasticnet" = seq(input$glmnet_alpha_min, input$glmnet_alpha_max,
                                           by = input$glmnet_alpha_step)
      )
      lambda_g <- 10^seq(input$glmnet_log_lam_min, input$glmnet_log_lam_max,
                         length.out = input$glmnet_lam_n)
      expand.grid(alpha = alpha_g, lambda = lambda_g)
    }
    
    # ── Preprocessing update when method / config mode changes ────────────────
    
    observe({
      method <- current_method()
      mode   <- input$config_mode
      
      initial <- if (is.null(mode) || mode == "general") {
        general_preprocess %||% character(0)
      } else {
        switch(method %||% "null",
               "glmnet" = glmnet_preprocess %||% general_preprocess %||% character(0),
               "pls"    = pls_preprocess    %||% general_preprocess %||% character(0),
               "rpart"  = rpart_preprocess  %||% general_preprocess %||% character(0),
               character(0)
        )
      }
      updateSelectizeInput(session, "preprocess", choices = pp_choices, selected = initial)
    }) |> bindEvent(current_method(), input$config_mode)
    
    # ── Interaction terms choices update by config mode ───────────────────────
    
    observe({
      choices <- c(
        "All encoded predictors" = "all",
        "Numeric × Numeric" = "numeric_numeric",
        "Nominal × Numeric" = "nominal_numeric",
        "Nominal × Nominal" = "nominal_nominal"
      )
      selected <- input$interact_type %||% "all"
      if (!selected %in% choices) selected <- "all"
      updateSelectInput(session, "interact_type", choices = choices, selected = selected)
    }) |> bindEvent(input$config_mode, input$preprocess)
    
    # ── Train dispatcher ──────────────────────────────────────────────────────
    
    observeEvent(input$train, {
      method   <- current_method()
      req(!is.null(method))
      train_df <- get_train(); req(train_df, nrow(train_df) > 0)
      prep     <- input$preprocess
      eseed    <- effective_seed()
      tr_ctrl  <- build_tr_control(train_df)
      
      models[[method]] <- NULL
      output$action_feedback <- renderUI(NULL)
      showNotification(id = method,
                       paste("Training", method, "— please wait..."),
                       session = session, duration = NULL)
      
      obj <- startMode(isTRUE(input$parallel))
      t0  <- proc.time()
      
      tryCatch({
        set.seed(eseed)
        
        model <- if (method == "null") {
          rec <- build_recipe(train_df, prep, is_null = TRUE)
          caret::train(rec, data = train_df, method = "null",
                       metric = "RMSE", trControl = tr_ctrl)
          
        } else if (method == "glmnet") {
          library(glmnet)
          rec <- build_recipe(train_df, prep)
          if (isTRUE(input$config_mode == "specific") &&
              isTRUE(input$glmnet_grid_type == "custom")) {
            caret::train(rec, data = train_df, method = "glmnet",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneGrid = build_glmnet_grid(), na.action = na.pass)
          } else if (isTRUE(input$config_mode == "specific") &&
                     input$glmnet_penalty %in% c("ridge", "lasso")) {
            alpha_fixed <- if (input$glmnet_penalty == "ridge") 0 else 1
            caret::train(rec, data = train_df, method = "glmnet",
                         metric = "RMSE", trControl = tr_ctrl, na.action = na.pass,
                         tuneGrid = expand.grid(
                           alpha  = alpha_fixed,
                           lambda = 10^seq(3, -4, length.out = 50)
                         ))
          } else {
            caret::train(rec, data = train_df, method = "glmnet",
                         metric = "RMSE", trControl = tr_ctrl,
                         tuneLength = input$tune_length %||% 5, na.action = na.pass)
          }
          
        } else if (method == "pls") {
          library(pls)
          rec <- build_recipe(train_df, prep)
          caret::train(rec, data = train_df, method = "pls",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.pass)
          
        } else if (method == "rpart") {
          library(rpart)
          rec <- build_recipe(train_df, prep)
          caret::train(rec, data = train_df, method = "rpart",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.rpart)
        }
        
        t_train <- round((proc.time() - t0)["elapsed"], 2)
        
        # attach timing BEFORE saving so it's preserved in the RDS
        model$.wall_train <- t_train
        model$.wall_total <- t_train  # save time unknown yet; updated below
        
        deleteRds(method)
        saveToRds(model, method)
        t_total <- round((proc.time() - t0)["elapsed"], 2)
        
        # update session copy with accurate total (train + save)
        model$.wall_total <- t_total
        
        models[[method]] <- model
        
        output$action_feedback <- renderUI({
          div(style = "margin-top:8px; font-size:12px; color:#0f6e56; background:#e1f5ee;
                       border-left:3px solid #9fe1cb; padding:6px 10px; border-radius:6px;",
              icon("circle-check", style = "color:#0f6e56;"),
              HTML(paste0(" <b>", method, "</b> trained &mdash; ",
                          t_train, "s fit | ", t_total, "s total (incl. save)")))
        })
        
      }, error = function(e) {
        output$action_feedback <- renderUI({
          div(style = "margin-top:8px; font-size:12px; color:#dc3545; background:#fff5f5;
                       border-left:3px solid #f5c2c7; padding:6px 10px; border-radius:6px;",
              icon("circle-xmark", style = "color:#dc3545;"),
              HTML(paste0(" <b>Error:</b> ", conditionMessage(e))))
        })
      }, finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    # ── Load dispatcher ───────────────────────────────────────────────────────
    
    observeEvent(input$load, {
      method <- current_method()
      req(!is.null(method))
      model  <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
        output$action_feedback <- renderUI({
          div(style = "margin-top:8px; font-size:12px; color:#185FA5; background:#e8f0fe;
                       border-left:3px solid #a8c0fd; padding:6px 10px; border-radius:6px;",
              icon("folder-open", style = "color:#185FA5;"),
              HTML(paste0(" <b>", method, "</b> loaded from file.")))
        })
      }
    })
    
    # ── Delete dispatcher ─────────────────────────────────────────────────────
    
    observeEvent(input$delete, {
      method <- current_method()
      req(!is.null(method))
      models[[method]] <- NULL
      gc()
      output$action_feedback <- renderUI({
        div(style = "margin-top:8px; font-size:12px; color:#6c757d; background:#f8f9fa;
                     border-left:3px solid #ced4da; padding:6px 10px; border-radius:6px;",
            icon("trash", style = "color:#6c757d;"),
            HTML(paste0(" <b>", method, "</b> cleared from session.")))
      })
    })
    
    # ── Output renders ────────────────────────────────────────────────────────
    # Pattern per method: desc, metrics, tune_plot, recipe_html, recipe_table,
    #                     train_summary, coef
    
    for (m in methods) {
      local({
        meth <- m
        
        # description
        output[[paste0(meth, "_desc")]] <- renderText({
          tryCatch(description(meth), error = function(e) paste("Method:", meth))
        })
        
        # resampled metrics — best row, with wall-clock time substituted
        output[[paste0(meth, "_metrics")]] <- renderTable({
          mod <- models[[meth]]; req(mod)
          best <- mod$results[which.min(mod$results[["RMSE"]]), ]
          if (!is.null(mod$.wall_total)) {
            best$TimedFitStepSeconds <- NULL
            best$WallClockSecs       <- mod$.wall_total
          }
          best
        }, digits = 3)
        
        # tuning plot (not for null)
        if (meth != "null") {
          output[[paste0(meth, "_tune_plot")]] <- renderPlot({
            mod <- models[[meth]]; req(mod)
            plot(mod)
          })
        }
        
        # recipe HTML print
        output[[paste0(meth, "_recipe_html")]] <- renderUI({
          mod <- models[[meth]]; req(mod)
          tryCatch(.recipe_html_output(ns, mod),
                   error = function(e) tags$code(conditionMessage(e)))
        })
        
        # recipe predictor count table
        output[[paste0(meth, "_recipe_table")]] <- renderTable({
          mod <- models[[meth]]; req(mod)
          tryCatch(.recipe_term_table(mod), error = function(e) NULL)
        })
        
        # full caret training summary
        output[[paste0(meth, "_train_summary")]] <- renderPrint({
          mod <- models[[meth]]; req(mod)
          if (!is.null(mod$.wall_total))
            cat("Total wall-clock time:", mod$.wall_total, "s\n",
                "(fit + save; reported per TimedFitStepSeconds is per-resample only)\n\n")
          print(mod)
        })
        
        # coefficients (method-specific)
        output[[paste0(meth, "_coef")]] <- renderTable({
          mod <- models[[meth]]; req(mod)
          tryCatch({
            if (meth == "glmnet") {
              co <- as.matrix(coef(mod$finalModel, s = mod$bestTune$lambda))
              df <- data.frame(Coefficient = co[, 1], row.names = rownames(co))
              df[df$Coefficient != 0, , drop = FALSE]
            } else if (meth == "pls") {
              co <- coef(mod$finalModel)
              as.data.frame(co, row.names = rownames(co))
            } else if (meth == "rpart") {
              data.frame(
                Variable   = names(mod$finalModel$variable.importance),
                Importance = round(mod$finalModel$variable.importance, 4)
              )
            } else {
              data.frame(Note = "NULL model has no coefficients.")
            }
          }, error = function(e) data.frame(Note = conditionMessage(e)))
        }, rownames = (meth %in% c("glmnet", "pls")))
        
      })
    }
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      models         = models,
      effective_seed = effective_seed
    ))
    
  })
}
