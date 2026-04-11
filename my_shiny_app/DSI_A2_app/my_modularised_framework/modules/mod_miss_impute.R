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
      
      # ── Tab note ──────────────────────────────────────────────────────────
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Imputation Strategy</b><br><br>
              Imputer is <b>fitted on train only</b> and applied to both
              train and test splits. This prevents data leakage while
              ensuring no NAs reach the model.")
      ),
      hr(),
      
      # ── Response variable ─────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom: 10px;",
        tags$label("Response (Y) variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("y_var"), label = NULL, choices = c("(none)"), width = "100%")
      ),
      
      # ── Split variable ────────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom: 4px;",
        tags$label("Split variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        selectInput(ns("split_var"), label = NULL, choices = c("(none)"), width = "100%")
      ),
      
      # Train / Test level selectors
      conditionalPanel(
        condition = sprintf("input['%s'] != '(none)'", ns("split_var")),
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
      
      # ── Ignore variables ──────────────────────────────────────────────────
      tags$div(
        style = "margin-top: 10px; margin-bottom: 10px;",
        tags$label("Ignore variables:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        div(style = "font-size:11px; color:#6c757d; margin-bottom:4px;",
            "Auto-filled from roles: Sensitive, Case Weight, Stratifier, Ignore"),
        selectizeInput(ns("ignore_vars"), label = NULL,
                       choices  = NULL,
                       multiple = TRUE,
                       options  = list(placeholder = "Select vars to exclude from imputation..."),
                       width    = "100%")
      ),
      hr(),
      
      # ── Algorithm choice ──────────────────────────────────────────────────
      tags$label("Imputation algorithm:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(
        ns("algorithm"),
        label    = NULL,
        choices  = c(
          "KNN (step_impute_knn)"         = "knn",
          "Bagged Tree (step_impute_bag)" = "bag",
          "Mean / Median / Mode"          = "mmm"
        ),
        selected = "knn"
      ),
      hr(),
      
      # ── KNN parameters ────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'knn'", ns("algorithm")),
        tags$label("KNN parameters:", style = "font-weight:600; font-size:13px; color:#343a40;"),
        sliderInput(ns("knn_neighbors"), "Neighbours (k):",
                    min = 1, max = 25, value = 5, step = 1, width = "100%")
      ),
      
      # ── Bag parameters ────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bag'", ns("algorithm")),
        tags$label("Bag parameters:", style = "font-weight:600; font-size:13px; color:#343a40;"),
        sliderInput(ns("bag_trees"), "Number of trees:",
                    min = 5, max = 50, value = 25, step = 5, width = "100%"),
        sliderInput(ns("bag_neighbors"), "Imputation neighbours:",
                    min = 1, max = 10, value = 5, step = 1, width = "100%")
      ),
      
      # ── MMM parameters ────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'mmm'", ns("algorithm")),
        tags$label("MMM parameters:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        div(style = "font-size:12px; color:#6c757d; margin-bottom:8px;",
            "Categorical columns are always imputed with mode."),
        
        tags$label("Impute with Mean:",
                   style = "font-size:12px; font-weight:600; color:#343a40; display:block; margin-bottom:2px;"),
        selectizeInput(ns("mmm_mean_cols"), label = NULL,
                       choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Select numeric columns..."),
                       width = "100%"),
        
        tags$label("Impute with Median:",
                   style = "font-size:12px; font-weight:600; color:#343a40; display:block; margin-top:6px; margin-bottom:2px;"),
        selectizeInput(ns("mmm_median_cols"), label = NULL,
                       choices = NULL, multiple = TRUE,
                       options = list(placeholder = "Select numeric columns..."),
                       width = "100%")
      ),
      hr(),
      
      # ── Bag warning ───────────────────────────────────────────────────────
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bag'", ns("algorithm")),
        div(
          style = "background:#fff3cd; border:1px solid #ffc107; border-radius:6px;
                   padding:8px 10px; font-size:12px; color:#856404; margin-bottom:10px;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML("&nbsp; Bagged tree imputation can be slow on large datasets.
                Click <b>Run Imputation</b> when ready.")
        )
      ),
      
      # ── Run button ────────────────────────────────────────────────────────
      actionButton(
        ns("run"),
        label = "Run Imputation",
        icon  = icon("play"),
        width = "100%",
        style = "background-color:#185FA5; color:white; border:none; margin-bottom:8px;"
      ),
      
      # ── Reset button ──────────────────────────────────────────────────────
      actionButton(
        ns("reset"),
        label = "Reset",
        icon  = icon("rotate-left"),
        width = "100%"
      )
    ),
    
    # ── Main panel ────────────────────────────────────────────────────────────
    mainPanel(
      width = 9,
      uiOutput(ns("result_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_impute_server <- function(id, get_data, split, roles) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Helpers ──────────────────────────────────────────────────────────────
    
    make_card <- function(..., bg = "white", border_color = "#dee2e6") {
      div(style = paste0(
        "background:", bg, "; border:1px solid ", border_color, ";",
        "border-radius:10px; padding:16px 20px; margin-bottom:12px;",
        "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
      ), ...)
    }
    
    stat_box <- function(label, value, icon_name, color = "#0d6efd") {
      div(
        style = paste0(
          "display:flex; align-items:center; gap:14px;",
          "background:white; border:1px solid #dee2e6; border-radius:10px;",
          "padding:14px 18px; flex:1; min-width:160px;",
          "box-shadow:0 1px 2px rgba(0,0,0,0.05);"
        ),
        div(
          style = paste0(
            "width:42px; height:42px; border-radius:50%;",
            "background:", color, "22;",
            "display:flex; align-items:center; justify-content:center;",
            "flex-shrink:0;"
          ),
          icon(icon_name, style = paste0("color:", color, "; font-size:18px;"))
        ),
        div(
          div(style = "font-size:22px; font-weight:700; color:#212529; line-height:1.1;", value),
          div(style = "font-size:12px; color:#6c757d; margin-top:2px;", label)
        )
      )
    }
    
    # ── Populate selectors from roles ────────────────────────────────────────
    
    observe({
      req(get_data(), roles())
      r    <- roles()
      vars <- names(get_data())
      updateSelectInput(session, "y_var",
                        choices  = c("(none)", vars),
                        selected = {
                          v <- names(r)[r == "outcome"]
                          if (length(v)) v[1] else "(none)"
                        })
      updateSelectInput(session, "split_var",
                        choices  = c("(none)", vars),
                        selected = {
                          v <- names(r)[r == "split"]
                          if (length(v)) v[1] else "(none)"
                        })
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
    
    # ── Auto-populate ignore vars from roles ─────────────────────────────────
    
    observe({
      req(get_data(), roles())
      r    <- roles()
      vars <- names(get_data())
      
      # roles that should be ignored by the imputer
      # adjust these strings to match exactly what your data_roles module returns
      ignore_role_names <- c("sensitive", "weight", "stratifier", "ignore")
      auto_ignored      <- names(r)[tolower(as.character(r)) %in% ignore_role_names]
      
      updateSelectizeInput(session, "ignore_vars",
                           choices  = vars,
                           selected = auto_ignored,
                           server   = TRUE)
    })
    
    # ── Populate MMM selectize boxes ─────────────────────────────────────────
    
    observe({
      req(get_data())
      df       <- get_data()
      excl     <- unique(c(input$y_var, input$split_var, input$ignore_vars))
      num_cols <- setdiff(names(df)[sapply(df, is.numeric)], excl)
      updateSelectizeInput(session, "mmm_mean_cols",
                           choices  = num_cols,
                           selected = num_cols,
                           server   = TRUE)
      updateSelectizeInput(session, "mmm_median_cols",
                           choices  = num_cols,
                           selected = NULL,
                           server   = TRUE)
    }) |> bindEvent(get_data(), input$y_var, input$split_var, input$ignore_vars)
    
    # ── State ─────────────────────────────────────────────────────────────────
    
    impute_result <- reactiveVal(NULL)
    
    # ── Run ───────────────────────────────────────────────────────────────────
    
    observeEvent(input$run, {
      df   <- get_data(); req(df)
      algo <- input$algorithm
      
      start_time <- proc.time()[["elapsed"]]
      
      tryCatch({
        
        # excluded columns
        role_vals <- if (!is.null(roles)) roles() else NULL
        id_cols   <- if (!is.null(role_vals)) names(role_vals[role_vals == "obs_id"]) else character(0)
        
        excl <- unique(c(
          if (!is.null(input$y_var)      && input$y_var      != "(none)") input$y_var,
          if (!is.null(input$split_var)  && input$split_var  != "(none)") input$split_var,
          id_cols,
          if (length(input$ignore_vars) > 0) input$ignore_vars   # ← user-selected / auto-filled ignore vars
        ))
        pred_cols <- setdiff(names(df), excl)
        
        # train / test subsets
        sv        <- input$split_var
        tl        <- input$train_level
        ts        <- input$test_level
        splits_ok <- !is.null(sv) && sv != "(none)" &&
          !is.null(tl) && tl != "(none)" &&
          !is.null(ts) && ts != "(none)"
        
        train_df  <- if (splits_ok) df[as.character(df[[sv]]) == tl, , drop = FALSE] else df
        test_df   <- if (splits_ok) df[as.character(df[[sv]]) == ts, , drop = FALSE] else NULL
        
        train_pred <- train_df[, pred_cols, drop = FALSE]
        test_pred  <- if (!is.null(test_df)) test_df[, pred_cols, drop = FALSE] else NULL
        
        # ── algorithms ────────────────────────────────────────────────────
        
        if (algo == "mmm") {
          
          library(recipes)
          
          num_cols <- names(train_pred)[sapply(train_pred, is.numeric)]
          fac_cols <- setdiff(pred_cols, num_cols)
          
          mean_cols   <- intersect(input$mmm_mean_cols,   num_cols)
          median_cols <- intersect(input$mmm_median_cols, num_cols)
          
          rec <- recipe(~ ., data = train_pred)
          if (length(mean_cols)   > 0) rec <- rec |> step_impute_mean(all_of(mean_cols))
          if (length(median_cols) > 0) rec <- rec |> step_impute_median(all_of(median_cols))
          if (length(fac_cols)    > 0) rec <- rec |> step_impute_mode(all_of(fac_cols))
          
          trained     <- prep(rec, training = train_pred, verbose = FALSE)
          train_baked <- bake(trained, new_data = NULL)
          test_baked  <- if (!is.null(test_pred)) bake(trained, new_data = test_pred) else NULL
          
        } else if (algo == "knn") {
          
          library(recipes)
          
          y_var <- input$y_var
          
          # include y in the data so recipe knows the role, but only impute predictors
          if (!is.null(y_var) && y_var != "(none)" && y_var %in% names(train_df)) {
            train_with_y <- train_df[, c(pred_cols, y_var), drop = FALSE]
            test_with_y  <- if (!is.null(test_df)) test_df[, c(pred_cols, y_var), drop = FALSE] else NULL
            fml <- as.formula(paste(y_var, "~ ."))
          } else {
            train_with_y <- train_pred
            test_with_y  <- test_pred
            fml <- ~ .
          }
          
          rec <- recipe(fml, data = train_with_y) |>
            step_impute_knn(all_predictors(), neighbors = input$knn_neighbors)
          
          trained     <- prep(rec, training = train_with_y, verbose = FALSE)
          train_baked <- bake(trained, new_data = NULL)
          test_baked  <- if (!is.null(test_with_y)) bake(trained, new_data = test_with_y) else NULL
          
          # strip y back out so rebuild works on pred_cols only
          train_baked <- train_baked[, intersect(pred_cols, names(train_baked)), drop = FALSE]
          if (!is.null(test_baked))
            test_baked <- test_baked[, intersect(pred_cols, names(test_baked)), drop = FALSE]
          
        } else if (algo == "bag") {
          
          library(recipes)
          
          y_var <- input$y_var
          
          if (!is.null(y_var) && y_var != "(none)" && y_var %in% names(train_df)) {
            train_with_y <- train_df[, c(pred_cols, y_var), drop = FALSE]
            test_with_y  <- if (!is.null(test_df)) test_df[, c(pred_cols, y_var), drop = FALSE] else NULL
            fml <- as.formula(paste(y_var, "~ ."))
          } else {
            train_with_y <- train_pred
            test_with_y  <- test_pred
            fml <- ~ .
          }
          
          rec <- recipe(fml, data = train_with_y) |>
            step_impute_bag(all_predictors(),
                            trees     = input$bag_trees,
                            neighbors = input$bag_neighbors)
          
          trained     <- prep(rec, training = train_with_y, verbose = FALSE)
          train_baked <- bake(trained, new_data = NULL)
          test_baked  <- if (!is.null(test_with_y)) bake(trained, new_data = test_with_y) else NULL
          
          # strip y back out so rebuild works on pred_cols only
          train_baked <- train_baked[, intersect(pred_cols, names(train_baked)), drop = FALSE]
          if (!is.null(test_baked))
            test_baked <- test_baked[, intersect(pred_cols, names(test_baked)), drop = FALSE]
          
        }
        
        # rebuild: put imputed pred cols back into original full-row frames
        rebuild <- function(original, baked, p_cols) {
          out <- original
          for (col in intersect(p_cols, names(baked))) out[[col]] <- baked[[col]]
          out
        }
        
        train_out <- rebuild(train_df, train_baked, pred_cols)
        test_out  <- if (!is.null(test_baked)) rebuild(test_df, test_baked, pred_cols) else NULL
        
        elapsed <- round(proc.time()[["elapsed"]] - start_time, 2)
        
        impute_result(list(
          train_out     = train_out,
          test_out      = test_out,
          algo          = algo,
          elapsed       = elapsed,
          pred_cols     = pred_cols,
          excl          = excl,
          na_before     = list(
            train = sum(is.na(train_pred)),
            test  = if (!is.null(test_pred)) sum(is.na(test_pred)) else NA
          ),
          na_after      = list(
            train = sum(is.na(train_out[, pred_cols, drop = FALSE])),
            test  = if (!is.null(test_out))
              sum(is.na(test_out[, pred_cols, drop = FALSE])) else NA
          ),
          col_na_before = colSums(is.na(train_pred)),
          col_na_after  = colSums(is.na(train_out[, pred_cols, drop = FALSE])),
          params        = list(
            knn_neighbors = input$knn_neighbors,
            bag_trees     = input$bag_trees,
            bag_neighbors = input$bag_neighbors
          )
        ))
        
      }, error = function(e) {
        impute_result(list(error = conditionMessage(e)))
      })
    })
    
    # ── Reset ─────────────────────────────────────────────────────────────────
    
    observeEvent(input$reset, {
      impute_result(NULL)
      updateRadioButtons(session, "algorithm",     selected = "knn")
      updateSliderInput(session,  "knn_neighbors",  value = 5)
      updateSliderInput(session,  "bag_trees",      value = 25)
      updateSliderInput(session,  "bag_neighbors",  value = 5)
      # re-derive ignore vars from roles instead of clearing
      r                 <- roles()
      vars              <- names(get_data())
      ignore_role_names <- c("sensitive", "weight", "stratifier", "ignore")
      auto_ignored      <- names(r)[tolower(as.character(r)) %in% ignore_role_names]
      updateSelectizeInput(session, "ignore_vars",
                           choices  = vars,
                           selected = auto_ignored,
                           server   = TRUE)
    })
    
    # ── Result UI ─────────────────────────────────────────────────────────────
    
    output$result_ui <- renderUI({
      res <- impute_result()
      
      # idle
      if (is.null(res)) {
        return(make_card(
          div(style = "text-align:center; color:#6c757d; padding:30px 0;",
              icon("circle-info", style = "font-size:32px; margin-bottom:10px; color:#adb5bd;"),
              br(),
              tags$span("No imputation run yet.", style = "font-size:15px;"),
              br(),
              tags$span("Configure settings and click Run Imputation.",
                        style = "font-size:12px; color:#adb5bd;")
          )
        ))
      }
      
      # error
      if (!is.null(res$error)) {
        return(make_card(
          bg = "#fff5f5", border_color = "#f5c2c7",
          div(
            icon("circle-xmark", style = "color:#dc3545; font-size:20px;"),
            tags$span(" Imputation error", style = "font-weight:600; color:#dc3545;"),
            br(), br(),
            tags$code(res$error, style = "font-size:12px;")
          )
        ))
      }
      
      # success
      algo_label <- switch(res$algo,
                           "knn" = paste0("KNN (k = ", res$params$knn_neighbors, ")"),
                           "bag" = paste0("Bagged Trees (", res$params$bag_trees,
                                          " trees, k = ", res$params$bag_neighbors, ")"),
                           "mmm" = "Mean / Median / Mode"
      )
      
      total_imputed <- (res$na_before$train - res$na_after$train) +
        ifelse(!is.na(res$na_before$test),
               res$na_before$test - res$na_after$test, 0)
      cols_affected <- sum(res$col_na_before > 0)
      
      tagList(
        
        # stat boxes
        div(
          style = "display:flex; gap:12px; flex-wrap:wrap; margin-bottom:14px;",
          stat_box("Cells Imputed",   format(total_imputed, big.mark = ","), "fill-drip",     "#0d6efd"),
          stat_box("Cols Affected",   cols_affected,                         "table-columns", "#6610f2"),
          stat_box("Train NAs After", res$na_after$train,                    "check-circle",  "#198754"),
          stat_box("Test NAs After",
                   if (is.na(res$na_after$test)) "—" else res$na_after$test, "check-circle",  "#198754"),
          stat_box("Time (s)",        res$elapsed,                           "stopwatch",     "#fd7e14")
        ),
        
        # train / test summary table
        make_card(
          tags$h6("Imputation Summary",
                  style = "font-weight:600; margin-bottom:12px; color:#343a40;"),
          tags$table(
            class = "table table-sm table-bordered",
            style = "font-size:13px; margin-bottom:0;",
            tags$thead(tags$tr(
              tags$th(""), tags$th("NAs Before"), tags$th("NAs After"), tags$th("Imputed")
            )),
            tags$tbody(
              tags$tr(
                tags$td(tags$b("Train")),
                tags$td(format(res$na_before$train, big.mark = ",")),
                tags$td(res$na_after$train),
                tags$td(style = "color:#198754; font-weight:600;",
                        format(res$na_before$train - res$na_after$train, big.mark = ","))
              ),
              tags$tr(
                tags$td(tags$b("Test")),
                tags$td(if (is.na(res$na_before$test)) "—"
                        else format(res$na_before$test, big.mark = ",")),
                tags$td(if (is.na(res$na_after$test)) "—" else res$na_after$test),
                tags$td(style = "color:#198754; font-weight:600;",
                        if (is.na(res$na_before$test)) "—"
                        else format(res$na_before$test - res$na_after$test, big.mark = ","))
              )
            )
          )
        ),
        
        # settings recap pills
        make_card(
          bg = "#f8f9fa",
          tags$h6("Settings Used",
                  style = "font-weight:600; margin-bottom:10px; color:#343a40;"),
          div(style = "display:flex; gap:8px; flex-wrap:wrap; font-size:12px;",
              span(style = "background:#e7f0ff; color:#0d6efd; padding:3px 10px; border-radius:999px;",
                   icon("wand-magic-sparkles"), paste0(" ", algo_label)),
              span(style = "background:#e8f5e9; color:#198754; padding:3px 10px; border-radius:999px;",
                   icon("circle-check"), " Fitted on train only"),
              if (length(res$excl) > 0)
                span(style = "background:#fff3e0; color:#e65100; padding:3px 10px; border-radius:999px;",
                     icon("eye-slash"),
                     paste0(" Excluded: ", paste(res$excl, collapse = ", ")))
          )
        ),
        
        # per-column DT
        make_card(
          tags$h6(icon("table", style = "color:#185FA5; margin-right:6px;"),
                  "Per-Column Imputation Detail",
                  style = "font-weight:600; margin-bottom:12px; color:#343a40;"),
          DT::dataTableOutput(ns("col_tbl"))
        )
      )
    })
    
    # ── Per-column DT ─────────────────────────────────────────────────────────
    
    output$col_tbl <- DT::renderDataTable({
      res <- impute_result()
      req(res, is.null(res$error))
      
      tbl <- data.frame(
        Column         = names(res$col_na_before),
        Type           = sapply(get_data()[, names(res$col_na_before), drop = FALSE],
                                function(x) class(x)[1]),
        Missing_Before = as.integer(res$col_na_before),
        Missing_After  = as.integer(res$col_na_after),
        Imputed        = as.integer(res$col_na_before - res$col_na_after),
        stringsAsFactors = FALSE
      )
      tbl <- tbl[order(-tbl$Imputed), ]
      
      DT::datatable(tbl,
                    options  = list(pageLength = 15, dom = "tip"),
                    rownames = FALSE) |>
        DT::formatStyle("Imputed",
                        color      = DT::styleInterval(0, c("#adb5bd", "#185FA5")),
                        fontWeight = "bold")
    })
    
    # ── Return ────────────────────────────────────────────────────────────────
    
    return(list(
      data = reactive({
        res <- impute_result()
        if (is.null(res) || !is.null(res$error)) return(get_data())
        
        df  <- get_data()
        sv  <- input$split_var
        tl  <- input$train_level
        ts  <- input$test_level
        out <- df
        
        if (!is.null(sv) && sv != "(none)" && !is.null(tl) && tl != "(none)") {
          train_rows <- which(as.character(df[[sv]]) == tl)
          for (col in intersect(names(res$train_out), names(out)))
            if (length(train_rows) == nrow(res$train_out))
              out[train_rows, col] <- res$train_out[[col]]
        }
        
        if (!is.null(res$test_out) && !is.null(ts) && ts != "(none)") {
          test_rows <- which(as.character(df[[sv]]) == ts)
          for (col in intersect(names(res$test_out), names(out)))
            if (length(test_rows) == nrow(res$test_out))
              out[test_rows, col] <- res$test_out[[col]]
        }
        
        out
      })
    ))
    
  })
}