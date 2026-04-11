# =================================================================================
# mod_miss_impute.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

miss_impute_ui <- function(id) {
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
        HTML("&nbsp; Tab Note: <br><br>
              Select an imputation algorithm and training mode.
              KNN and Bag can be expensive — use the Run button to trigger.")
      ),
      hr(),
      
      # training mode
      tags$label("Training Mode:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("train_mode"), label = NULL,
                   choices = c(
                     "All observations (diagnose)" = "all",
                     "Train on train, predict test" = "traintest",
                     "Predict future unseen data"   = "future"
                   ),
                   selected = "all"),
      hr(),
      
      # split col selector
      conditionalPanel(
        condition = paste0("input['", ns("train_mode"), "'] == 'traintest'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Split Column:",
                     style = "font-weight:600; font-size:12px; color:#343a40; display:block; margin-bottom:4px;"),
          tags$p("If split module is configured, it will be used automatically.",
                 style = "font-size:11px; color:#6c757d; margin-bottom:4px;"),
          selectInput(ns("split_col"), label = NULL, choices = NULL, width = "100%")
        )
      ),
      
      # future: future data indicator col
      conditionalPanel(
        condition = paste0("input['", ns("train_mode"), "'] == 'future'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Future Data Column:",
                     style = "font-weight:600; font-size:12px; color:#343a40; display:block; margin-bottom:4px;"),
          tags$p("Select a column where a level indicates future/unseen rows (e.g. 'future', 'yes').",
                 style = "font-size:11px; color:#6c757d; margin-bottom:4px;"),
          selectInput(ns("future_col"), label = NULL, choices = NULL, width = "100%"),
          selectInput(ns("future_level"), label = "Future level:",
                      choices = NULL, width = "100%")
        )
      ),
      hr(),
      
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
      
      # mmm params
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
      
      # knn params
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
      
      # bag params
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
      
      # run button
      actionButton(ns("run"),   label = "Run Imputation",
                   icon = icon("play"), width = "100%",
                   style = "background-color:#185FA5; color:white; margin-bottom:8px;"),
      actionButton(ns("reset"), label = "Reset",
                   icon = icon("rotate-left"), width = "100%")
    ),
    
    mainPanel(
      width = 9,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_impute_server <- function(id, get_data, get_split = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # state
    impute_result <- reactiveVal(NULL)
    
    observeEvent(input$reset, {
      impute_result(NULL)
      updateRadioButtons(session, "algorithm",   selected = "mmm")
      updateRadioButtons(session, "train_mode",  selected = "all")
      updateRadioButtons(session, "mmm_numeric", selected = "mean")
      updateSliderInput(session,  "knn_k",       value = 5)
      updateSliderInput(session,  "bag_trees",   value = 25)
    })
    
    # ── populate split col selector ───────────────────────────────────────
    observe({
      df <- get_data()
      req(df)
      updateSelectInput(session, "split_col",
                        choices  = c("(none)", names(df)),
                        selected = "(none)")
      updateSelectInput(session, "future_col",
                        choices  = c("(none)", names(df)),
                        selected = "(none)")
    }) |> bindEvent(get_data())
    
    observeEvent(input$future_col, {
      req(input$future_col, input$future_col != "(none)")
      df  <- get_data()
      req(df)
      lvls <- as.character(unique(df[[input$future_col]]))
      updateSelectInput(session, "future_level", choices = lvls)
    })
    
    # ── run imputation ────────────────────────────────────────────────────
    
    observeEvent(input$run, {
      
      df        <- get_data()
      algo      <- input$algorithm
      mode      <- input$train_mode
      req(df)
      
      start_time <- proc.time()
      
      tryCatch({
        
        if (!requireNamespace("recipes", quietly = TRUE))
          stop("Package 'recipes' is required. Please install it.")
        
        library(recipes)
        
        # helper: build recipe and bake
        run_recipe <- function(train_df, predict_df = NULL) {
          rec <- recipe(~ ., data = train_df)
          
          rec <- if (algo == "mmm") {
            num_strategy <- input$mmm_numeric
            if (num_strategy == "mean")
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
          
          trained <- prep(rec, training = train_df, verbose = FALSE)
          target  <- if (is.null(predict_df)) train_df else predict_df
          list(trained = trained, imputed = bake(trained, new_data = target))
        }
        
        result <- if (mode == "all") {
          r <- run_recipe(df)
          list(imputed_train = r$imputed, imputed_test = NULL, recipe = r$trained)
          
        } else if (mode == "traintest") {
          # prefer split module if available
          train_df <- if (!is.null(get_split) && nrow(get_split$train()) > 0) {
            get_split$train()
          } else {
            req(input$split_col, input$split_col != "(none)")
            col_chr <- tolower(trimws(as.character(df[[input$split_col]])))
            df[col_chr %in% c("train", "1", "true"), , drop = FALSE]
          }
          
          test_df <- if (!is.null(get_split) && nrow(get_split$test()) > 0) {
            get_split$test()
          } else {
            req(input$split_col, input$split_col != "(none)")
            col_chr <- tolower(trimws(as.character(df[[input$split_col]])))
            df[col_chr %in% c("test", "0", "false"), , drop = FALSE]
          }
          
          req(nrow(train_df) > 0, nrow(test_df) > 0)
          r_train <- run_recipe(train_df)
          r_test  <- list(imputed = bake(r_train$trained, new_data = test_df))
          list(imputed_train = r_train$imputed,
               imputed_test  = r_test$imputed,
               recipe        = r_train$trained)
          
        } else {
          # future: train on all, ready to bake new data
          r <- run_recipe(df)
          list(imputed_train = r$imputed, imputed_test = NULL, recipe = r$trained)
        }
        
        elapsed <- (proc.time() - start_time)[["elapsed"]]
        
        # count imputed cells
        n_imputed <- sum(is.na(df[, names(result$imputed_train), drop = FALSE]) &
                           !is.na(result$imputed_train))
        
        impute_result(list(
          result    = result,
          algo      = algo,
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
          tags$p("Configure options and click Run Imputation.",
                 style = "font-size:15px;")
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
                           traintest = "Train on trainset, predict testset",
                           future    = "Predict future unseen data"
      )
      
      # ── stat cards ───────────────────────────────────────────────────────
      
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
        card("Cells Imputed", res$n_imputed,           "#185FA5"),
        card("Algorithm",     algo_label,               "#0F6E56"),
        card("Mode",          mode_label,               "#534AB7"),
        card("Time (sec)",    res$elapsed,              "#BA7517")
      )
      
      # ── per-column imputation summary ─────────────────────────────────────
      
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
    
    # ── per-column summary table ──────────────────────────────────────────
    
    output$col_summary_tbl <- DT::renderDataTable({
      req(impute_result(), is.null(impute_result()$error))
      res     <- impute_result()
      df_orig <- res$df_orig
      df_imp  <- res$result$imputed_train
      
      tbl <- data.frame(
        Column       = names(df_orig),
        Type         = sapply(df_orig, function(x) class(x)[1]),
        Missing_Before = colSums(is.na(df_orig)),
        Missing_After  = colSums(is.na(df_imp[, names(df_orig), drop = FALSE])),
        Imputed        = colSums(is.na(df_orig)) - colSums(is.na(df_imp[, names(df_orig), drop = FALSE])),
        stringsAsFactors = FALSE
      )
      tbl <- tbl[order(-tbl$Imputed), ]
      
      DT::datatable(tbl, options = list(pageLength = 15, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Imputed",
                        color = DT::styleInterval(0, c("#adb5bd", "#185FA5")),
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