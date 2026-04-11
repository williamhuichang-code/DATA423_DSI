# =================================================================================
# mod_miss_impute.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

miss_impute_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 4,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd;
               min-height: 100vh; padding-left: 20px; overflow-y: auto;",
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br>
              Select an imputation algorithm and training mode.
              KNN and Bag can be expensive — use the Run button to trigger.")
      ),
      hr(),
      
      # outcome variable
      tags$label("Outcome Variable:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      tags$p("Auto-selected from Data Roles. Defines the response so KNN only uses predictors.",
             style = "font-size:11px; color:#6c757d; margin-bottom:4px;"),
      selectInput(ns("outcome_var"), label = NULL,
                  choices = c("(none)"), selected = "(none)", width = "100%"),
      hr(),
      
      # training mode
      tags$label("Training Mode:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("train_mode"), label = NULL,
                   choices = c(
                     "All observations (diagnose)"    = "all",
                     "Impute on train, apply on test" = "traintest",
                     "Predict on future unseen data"  = "future"
                   ),
                   selected = "all"),
      hr(),
      
      # split col (traintest only)
      conditionalPanel(
        condition = paste0("input['", ns("train_mode"), "'] == 'traintest'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Split Column:",
                     style = "font-weight:600; font-size:12px; color:#343a40; display:block; margin-bottom:4px;"),
          tags$p("If split module is configured, it will be used automatically.",
                 style = "font-size:11px; color:#6c757d; margin-bottom:4px;"),
          selectInput(ns("split_col"), label = NULL, choices = c("(none)"), width = "100%")
        ),
        hr()
      ),
      
      # future col (future only)
      conditionalPanel(
        condition = paste0("input['", ns("train_mode"), "'] == 'future'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Future Data Column:",
                     style = "font-weight:600; font-size:12px; color:#343a40; display:block; margin-bottom:4px;"),
          tags$p("Select a column where a level indicates future/unseen rows.",
                 style = "font-size:11px; color:#6c757d; margin-bottom:4px;"),
          selectInput(ns("future_col"),   label = NULL,            choices = c("(none)"), width = "100%"),
          selectInput(ns("future_level"), label = "Future level:", choices = NULL,        width = "100%")
        ),
        hr()
      ),
      
      # algorithm
      tags$label("Algorithm:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("algorithm"), label = NULL,
                   choices = c(
                     "Mean / Median / Mode" = "mmm",
                     "KNN Imputation"       = "knn",
                     "Bag Imputation"       = "bag"
                   ),
                   selected = "mmm"),
      
      conditionalPanel(
        condition = paste0("input['", ns("algorithm"), "'] == 'mmm'"),
        tags$div(
          style = "margin-top:8px; padding:8px; background:white; border-radius:6px;
                   border:0.5px solid #dee2e6; font-size:12px; color:#6c757d;",
          "Numeric columns → mean or median; categorical columns → mode."
        ),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Numeric strategy:",
                     style = "font-weight:600; font-size:12px; color:#343a40;"),
          radioButtons(ns("mmm_numeric"), label = NULL,
                       choices = c("Mean" = "mean", "Median" = "median"),
                       selected = "mean")
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("algorithm"), "'] == 'knn'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Neighbours (K):",
                     style = "font-weight:600; font-size:12px; color:#343a40;"),
          sliderInput(ns("knn_k"), label = NULL,
                      min = 1, max = 20, value = 5, step = 1, width = "100%")
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("algorithm"), "'] == 'bag'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Number of trees:",
                     style = "font-weight:600; font-size:12px; color:#343a40;"),
          sliderInput(ns("bag_trees"), label = NULL,
                      min = 5, max = 50, value = 25, step = 5, width = "100%")
        )
      ),
      
      hr(),
      actionButton(ns("run"),   label = "Run Imputation",
                   icon = icon("play"), width = "100%",
                   style = "background-color:#185FA5; color:white; margin-bottom:8px;"),
      actionButton(ns("reset"), label = "Reset",
                   icon = icon("rotate-left"), width = "100%")
    ),
    
    mainPanel(
      width = 8,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_impute_server <- function(id, get_data, get_split = NULL, get_roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    impute_result <- reactiveVal(NULL)
    
    # ── reset ─────────────────────────────────────────────────────────────
    observeEvent(input$reset, {
      impute_result(NULL)
      updateRadioButtons(session, "algorithm",   selected = "mmm")
      updateRadioButtons(session, "train_mode",  selected = "all")
      updateRadioButtons(session, "mmm_numeric", selected = "mean")
      updateSliderInput(session,  "knn_k",       value = 5)
      updateSliderInput(session,  "bag_trees",   value = 25)
      updateSelectInput(session,  "outcome_var", selected = "(none)")
      updateSelectInput(session,  "split_col",   selected = "(none)")
    })
    
    # ── populate choices from data ────────────────────────────────────────
    observe({
      df <- get_data()
      req(df)
      
      default_outcome <- "(none)"
      default_split   <- "(none)"
      
      if (!is.null(get_roles)) {
        cur <- get_roles()
        ov  <- names(cur)[cur == "outcome"]
        sv  <- names(cur)[cur == "split"]
        if (length(ov) == 1) default_outcome <- ov
        if (length(sv) == 1) default_split   <- sv
      }
      
      updateSelectInput(session, "outcome_var",
                        choices  = c("(none)", names(df)),
                        selected = default_outcome)
      updateSelectInput(session, "split_col",
                        choices  = c("(none)", names(df)),
                        selected = default_split)
      updateSelectInput(session, "future_col",
                        choices  = c("(none)", names(df)),
                        selected = "(none)")
    }) |> bindEvent(get_data())
    
    # ── helper: columns to drop before recipe ────────────────────────────
    get_drop_cols <- function() {
      drop <- c()
      if (!is.null(input$split_col) && input$split_col != "(none)")
        drop <- c(drop, input$split_col)
      if (!is.null(get_roles)) {
        cur  <- get_roles()
        drop <- c(drop, names(cur)[cur %in% c("obs_id", "ignore", "split")])
      }
      unique(drop)
    }
    
    # ── helper: build recipe and bake ────────────────────────────────────
    run_recipe <- function(train_df, predict_df = NULL) {
      
      # drop non-feature columns
      drop_cols  <- get_drop_cols()
      train_df   <- train_df[,   setdiff(names(train_df),   drop_cols), drop = FALSE]
      if (!is.null(predict_df))
        predict_df <- predict_df[, setdiff(names(predict_df), drop_cols), drop = FALSE]
      
      # align factor levels: predict must match train
      if (!is.null(predict_df)) {
        for (col in names(train_df)) {
          if (is.factor(train_df[[col]]) && col %in% names(predict_df))
            predict_df[[col]] <- factor(predict_df[[col]], levels = levels(train_df[[col]]))
        }
      }
      
      # outcome variable — defines response so KNN uses only predictors
      ov <- input$outcome_var
      outcome_var <- if (!is.null(ov) && ov != "(none)" && ov %in% names(train_df)) ov else NULL
      
      rec <- if (!is.null(outcome_var)) {
        recipe(as.formula(paste(outcome_var, "~ .")), data = train_df)
      } else {
        recipe(~ ., data = train_df)
      }
      
      algo <- input$algorithm
      rec  <- if (algo == "mmm") {
        if (input$mmm_numeric == "mean")
          rec |> step_impute_mean(all_numeric_predictors()) |>
          step_impute_mode(all_nominal_predictors())
        else
          rec |> step_impute_median(all_numeric_predictors()) |>
          step_impute_mode(all_nominal_predictors())
      } else if (algo == "knn") {
        rec |> step_impute_knn(all_predictors(), neighbors = input$knn_k)
      } else {
        rec |> step_impute_bag(all_predictors(), trees = input$bag_trees)
      }
      
      trained       <- prep(rec, training = train_df, verbose = FALSE)
      imputed_train <- bake(trained, new_data = train_df)
      imputed_test  <- if (!is.null(predict_df)) bake(trained, new_data = predict_df) else NULL
      
      list(trained = trained, imputed = imputed_train, imputed_test = imputed_test)
    }
    
    # ── run ───────────────────────────────────────────────────────────────
    observeEvent(input$run, {
      
      df   <- get_data()
      mode <- input$train_mode
      req(df)
      
      start_time <- proc.time()
      
      tryCatch({
        
        if (!requireNamespace("recipes", quietly = TRUE))
          stop("Package 'recipes' is required. Please install it.")
        library(recipes)
        
        result <- if (mode == "all") {
          
          r <- run_recipe(df)
          list(imputed_train = r$imputed, imputed_test = NULL, recipe = r$trained)
          
        } else if (mode == "traintest") {
          
          train_df <- tryCatch(
            if (!is.null(get_split)) get_split$train() else NULL,
            error = function(e) NULL
          )
          if (is.null(train_df) || nrow(train_df) == 0) {
            req(input$split_col, input$split_col != "(none)")
            col_chr  <- tolower(trimws(as.character(df[[input$split_col]])))
            train_df <- df[col_chr %in% c("train", "1", "true"), , drop = FALSE]
          }
          
          test_df <- tryCatch(
            if (!is.null(get_split)) get_split$test() else NULL,
            error = function(e) NULL
          )
          if (is.null(test_df) || nrow(test_df) == 0) {
            req(input$split_col, input$split_col != "(none)")
            col_chr <- tolower(trimws(as.character(df[[input$split_col]])))
            test_df <- df[col_chr %in% c("test", "0", "false"), , drop = FALSE]
          }
          
          if (nrow(train_df) == 0) stop("train set has 0 rows — check split column or Split module")
          if (nrow(test_df)  == 0) stop("test set has 0 rows — check split column or Split module")
          
          r <- run_recipe(train_df, predict_df = test_df)
          list(imputed_train = r$imputed,
               imputed_test  = r$imputed_test,
               recipe        = r$trained)
          
        } else {
          r <- run_recipe(df)
          list(imputed_train = r$imputed, imputed_test = NULL, recipe = r$trained)
        }
        
        elapsed   <- (proc.time() - start_time)[["elapsed"]]
        orig_cols <- intersect(names(df), names(result$imputed_train))
        n_imputed <- sum(is.na(df[, orig_cols, drop = FALSE]) &
                           !is.na(result$imputed_train[, orig_cols, drop = FALSE]))
        
        impute_result(list(
          result    = result,
          algo      = input$algorithm,
          mode      = mode,
          n_imputed = n_imputed,
          elapsed   = round(elapsed, 2),
          df_orig   = df
        ))
        
      }, error = function(e) {
        impute_result(list(error = conditionMessage(e)))
      })
    })
    
    # ── main UI ───────────────────────────────────────────────────────────
    output$main_ui <- renderUI({
      
      if (is.null(impute_result())) {
        return(tags$div(
          style = "padding:40px; text-align:center; color:#adb5bd;",
          icon("play-circle", style = "font-size:48px; margin-bottom:16px; display:block;"),
          tags$p("Configure options and click Run Imputation.", style = "font-size:15px;")
        ))
      }
      
      res <- impute_result()
      
      if (!is.null(res$error)) {
        return(tags$div(
          style = "background:#f8d7da; border:0.5px solid #f5c2c7; border-radius:8px;
                   padding:14px; color:#842029;",
          icon("circle-xmark"), paste(" Error:", res$error)
        ))
      }
      
      algo_label <- switch(res$algo,
                           mmm = "Mean / Median / Mode",
                           knn = paste0("KNN (K = ", input$knn_k, ")"),
                           bag = paste0("Bag (trees = ", input$bag_trees, ")")
      )
      mode_label <- switch(res$mode,
                           all       = "All observations (diagnose)",
                           traintest = "Impute on train, apply on test",
                           future    = "Predict on future unseen data"
      )
      
      card <- function(label, value, color) {
        tags$div(
          style = paste0(
            "flex:1; min-width:130px; background:white; border-radius:10px;",
            "border:0.5px solid #dee2e6; padding:14px 18px;",
            "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
          ),
          tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                            text-transform:uppercase; letter-spacing:.5px;", label),
          tags$div(style = paste0("font-size:26px; font-weight:700; color:", color, ";"), value)
        )
      }
      
      cards <- tags$div(
        style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
        card("Cells Imputed", res$n_imputed, "#185FA5"),
        card("Algorithm",     algo_label,    "#0F6E56"),
        card("Mode",          mode_label,    "#534AB7"),
        card("Time (sec)",    res$elapsed,   "#BA7517")
      )
      
      col_section <- tags$div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"),
                "Per-Column Imputation Summary",
                style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
        DT::dataTableOutput(ns("col_summary_tbl"))
      )
      
      tagList(cards, col_section)
    })
    
    # ── summary table ─────────────────────────────────────────────────────
    output$col_summary_tbl <- DT::renderDataTable({
      req(impute_result(), is.null(impute_result()$error))
      res         <- impute_result()
      df_orig     <- res$df_orig
      df_imp      <- res$result$imputed_train
      common_cols <- intersect(names(df_orig), names(df_imp))
      
      tbl <- data.frame(
        Column         = common_cols,
        Type           = sapply(df_orig[, common_cols, drop = FALSE], function(x) class(x)[1]),
        Missing_Before = colSums(is.na(df_orig[, common_cols, drop = FALSE])),
        Missing_After  = colSums(is.na(df_imp[,  common_cols, drop = FALSE])),
        Imputed        = colSums(is.na(df_orig[, common_cols, drop = FALSE])) -
          colSums(is.na(df_imp[,  common_cols, drop = FALSE])),
        stringsAsFactors = FALSE
      )
      tbl <- tbl[order(-tbl$Imputed), ]
      
      DT::datatable(tbl, options = list(pageLength = 15, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Imputed",
                        color      = DT::styleInterval(0, c("#adb5bd", "#185FA5")),
                        fontWeight = "bold")
    })
    
    # ── return ────────────────────────────────────────────────────────────
    return(list(
      data = reactive({
        res <- impute_result()
        if (is.null(res) || !is.null(res$error)) get_data()
        else res$result$imputed_train
      })
    ))
    
  })
}