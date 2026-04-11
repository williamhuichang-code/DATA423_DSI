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
      
      # ── tab note ──────────────────────────────────────────────────────
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <strong>Tab Note:</strong><br><br>
          Choose a training <em>mode</em>, pick an algorithm, configure its
          parameters, then click <strong>Run Imputation</strong>.
          The recipe is trained on the appropriate data slice and applied
          to the target rows. The imputed data flows downstream.")
      ),
      hr(),
      
      # ── training mode ─────────────────────────────────────────────────
      tags$label("Training Mode:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(
        ns("mode"), label = NULL,
        choices = c(
          "All observations (diagnose)"          = "all",
          "Train on trainset, predict on testset" = "split",
          "Predict on future unseen data"         = "unseen"
        ),
        selected = "all"
      ),
      
      # ── split-mode extras ─────────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("mode"), "'] == 'split'"),
        hr(),
        tags$div(
          style = "margin-bottom:10px;",
          tags$label("Response Variable:",
                     style = "font-weight:600; font-size:13px; color:#343a40;
                               display:block; margin-bottom:4px;"),
          selectInput(ns("response_var"), label = NULL,
                      choices = "(loading...)", width = "100%")
        ),
        tags$div(
          tags$label("Split Column:",
                     style = "font-weight:600; font-size:13px; color:#343a40;
                               display:block; margin-bottom:4px;"),
          selectInput(ns("split_col"), label = NULL,
                      choices = "(loading...)", width = "100%"),
          tags$div(
            style = "font-size:11px; color:#6c757d; margin-top:2px;",
            "Expected values: 'Train'/'Test', '1'/'0', or 'TRUE'/'FALSE'."
          )
        )
      ),
      
      # ── unseen-mode extras ────────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("mode"), "'] == 'unseen'"),
        hr(),
        tags$div(
          style = "margin-bottom:10px;",
          tags$label("Unseen Indicator Column:",
                     style = "font-weight:600; font-size:13px; color:#343a40;
                               display:block; margin-bottom:4px;"),
          selectInput(ns("unseen_col"), label = NULL,
                      choices = "(loading...)", width = "100%")
        ),
        tags$div(
          tags$label("Level that represents unseen data:",
                     style = "font-weight:600; font-size:13px; color:#343a40;
                               display:block; margin-bottom:4px;"),
          selectInput(ns("unseen_level"), label = NULL,
                      choices = "(loading...)", width = "100%")
        )
      ),
      
      hr(),
      
      # ── algorithm ─────────────────────────────────────────────────────
      tags$label("Algorithm:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(
        ns("algorithm"), label = NULL,
        choices = c(
          "Mean / Median / Mode (MMM)" = "mmm",
          "KNN Imputation"             = "knn",
          "Bagged Trees Imputation"    = "bag"
        ),
        selected = "knn"
      ),
      
      # ── mmm params ────────────────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("algorithm"), "'] == 'mmm'"),
        hr(),
        tags$label("MMM Parameters:",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                             display:block; margin-bottom:6px;"),
        tags$div(
          style = "font-size:12px; color:#6c757d; margin-bottom:8px;",
          "Numeric → mean or median. Factor / character → mode."
        ),
        radioButtons(
          ns("mmm_num_fn"), label = "Numeric strategy:",
          choices  = c("Mean" = "mean", "Median" = "median"),
          selected = "mean"
        )
      ),
      
      # ── knn params ────────────────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("algorithm"), "'] == 'knn'"),
        hr(),
        tags$label("KNN Parameters:",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                             display:block; margin-bottom:6px;"),
        tags$div(
          style = "margin-bottom:8px;",
          tags$label("Neighbours (K):",
                     style = "font-size:12px; color:#343a40;"),
          sliderInput(ns("knn_k"), label = NULL,
                      min = 1, max = 25, value = 5, step = 1, width = "100%")
        )
      ),
      
      # ── bag params ────────────────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("algorithm"), "'] == 'bag'"),
        hr(),
        tags$label("Bag Parameters:",
                   style = "font-weight:600; font-size:13px; color:#343a40;
                             display:block; margin-bottom:6px;"),
        tags$div(
          style = "margin-bottom:8px;",
          tags$label("Trees per imputation model:",
                     style = "font-size:12px; color:#343a40;"),
          sliderInput(ns("bag_trees"), label = NULL,
                      min = 5, max = 50, value = 25, step = 5, width = "100%")
        ),
        tags$div(
          style = "margin-bottom:8px;",
          tags$label("Random seed (optional):",
                     style = "font-size:12px; color:#343a40;"),
          numericInput(ns("bag_seed"), label = NULL,
                       value = NA, min = 0, step = 1, width = "100%")
        ),
        tags$div(
          style = "font-size:11px; color:#856404; background:#fff3cd;
                   border:0.5px solid #ffc107; border-radius:4px;
                   padding:6px 8px; margin-top:2px;",
          icon("triangle-exclamation"),
          " Bagged imputation can be slow on large datasets."
        )
      ),
      
      hr(),
      
      # ── action buttons ────────────────────────────────────────────────
      actionButton(
        ns("run"), "Run Imputation",
        icon  = icon("play"),
        width = "100%",
        style = "background-color:#185FA5; color:white; font-weight:600;
                 border:none; border-radius:6px; margin-bottom:8px;
                 padding:8px 0;"
      ),
      actionButton(
        ns("reset"), "Reset",
        icon  = icon("rotate-left"),
        width = "100%"
      )
    ),
    
    # ── main panel ────────────────────────────────────────────────────────
    mainPanel(
      width = 9,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

miss_impute_server <- function(id, get_data, split = NULL, roles = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── populate selectors ──────────────────────────────────────────────
    
    # fires when data changes (roles may or may not be ready)
    observe({
      df       <- get_data()
      req(df)
      all_vars <- names(df)
      
      updateSelectInput(session, "split_col",
                        choices  = c("(none)", all_vars),
                        selected = "(none)")
      updateSelectInput(session, "response_var",
                        choices  = c("(none)", all_vars),
                        selected = "(none)")
      updateSelectInput(session, "unseen_col",
                        choices  = c("(none)", all_vars),
                        selected = "(none)")
    }) |> bindEvent(get_data())
    
    # fires when roles change — overrides defaults above with role-aware selections
    # uses observe() + req() guard instead of bindEvent(roles()) to avoid
    # crashing when roles is NULL (not passed in)
    observe({
      if (is.null(roles)) return()
      role_vals <- roles()
      req(!is.null(role_vals), length(role_vals) > 0)
      df       <- get_data()
      req(df)
      all_vars <- names(df)
      
      default_resp <- {
        v <- names(role_vals[role_vals == "outcome"])
        if (length(v) > 0) v[1] else "(none)"
      }
      default_split <- {
        v <- names(role_vals[role_vals == "split"])
        if (length(v) > 0) v[1] else "(none)"
      }
      
      updateSelectInput(session, "response_var",
                        choices  = c("(none)", all_vars),
                        selected = default_resp)
      updateSelectInput(session, "split_col",
                        choices  = c("(none)", all_vars),
                        selected = default_split)
    })
    
    # populate unseen level choices when unseen_col changes
    observe({
      df  <- get_data()
      col <- input$unseen_col
      req(df, col, col != "(none)", col %in% names(df))
      lvls <- sort(unique(as.character(df[[col]])))
      lvls <- lvls[!is.na(lvls)]
      updateSelectInput(session, "unseen_level",
                        choices  = lvls,
                        selected = if (length(lvls) > 0) lvls[1] else NULL)
    }) |> bindEvent(input$unseen_col, get_data())
    
    # ── reset ────────────────────────────────────────────────────────────
    
    observeEvent(input$reset, {
      updateRadioButtons(session,  "mode",        selected = "all")
      updateRadioButtons(session,  "algorithm",   selected = "knn")
      updateSliderInput(session,   "knn_k",       value    = 5)
      updateSliderInput(session,   "bag_trees",   value    = 25)
      updateNumericInput(session,  "bag_seed",    value    = NA)
      updateRadioButtons(session,  "mmm_num_fn",  selected = "mean")
      impute_result(NULL)
    })
    
    # ── impute result store ──────────────────────────────────────────────
    
    impute_result <- reactiveVal(NULL)
    
    # ── run imputation ───────────────────────────────────────────────────
    
    observeEvent(input$run, {
      
      df <- get_data()
      req(df)
      
      tryCatch({
        
        mode <- input$mode
        
        # ── slice train / apply sets ───────────────────────────────────
        
        if (mode == "all") {
          train_df <- df
          apply_df <- df
          
        } else if (mode == "split") {
          req(input$split_col,
              input$split_col != "(none)",
              input$split_col %in% names(df))
          
          col_chr  <- tolower(trimws(as.character(df[[input$split_col]])))
          tr_mask  <- col_chr %in% c("train", "1", "true")
          te_mask  <- col_chr %in% c("test",  "0", "false")
          
          if (sum(tr_mask) == 0) {
            showNotification(
              "No Train rows found in split column. Expected 'Train'/'Test', '1'/'0', or 'TRUE'/'FALSE'.",
              type = "error", duration = 8
            )
            return()
          }
          train_df <- df[tr_mask, , drop = FALSE]
          apply_df <- df[te_mask, , drop = FALSE]
          
        } else {   # unseen
          req(input$unseen_col,
              input$unseen_col != "(none)",
              input$unseen_col %in% names(df),
              input$unseen_level)
          
          col_chr  <- as.character(df[[input$unseen_col]])
          seen_mask <- col_chr != input$unseen_level | is.na(col_chr)
          train_df  <- df[seen_mask,  , drop = FALSE]
          apply_df  <- df[!seen_mask, , drop = FALSE]
          
          if (nrow(train_df) == 0 || nrow(apply_df) == 0) {
            showNotification(
              "Unseen indicator produced an empty train or apply set. Check column / level selection.",
              type = "error", duration = 8
            )
            return()
          }
        }
        
        # ── build recipe & impute ──────────────────────────────────────
        
        alg     <- input$algorithm
        t_start <- proc.time()[["elapsed"]]
        
        rec <- recipes::recipe(~ ., data = train_df)
        
        if (alg == "mmm") {
          num_cols <- names(train_df)[sapply(train_df, is.numeric)]
          fac_cols <- names(train_df)[sapply(train_df,
                                             function(x) is.factor(x) || is.character(x))]
          
          if (input$mmm_num_fn == "mean") {
            if (length(num_cols) > 0)
              rec <- rec |> recipes::step_impute_mean(recipes::all_of(num_cols))
          } else {
            if (length(num_cols) > 0)
              rec <- rec |> recipes::step_impute_median(recipes::all_of(num_cols))
          }
          if (length(fac_cols) > 0)
            rec <- rec |> recipes::step_impute_mode(recipes::all_of(fac_cols))
          
        } else if (alg == "knn") {
          rec <- rec |> recipes::step_impute_knn(
            recipes::all_predictors(),
            neighbors = as.integer(input$knn_k)
          )
          
        } else if (alg == "bag") {
          seed_val <- input$bag_seed
          if (!is.null(seed_val) && !is.na(seed_val))
            set.seed(as.integer(seed_val))
          rec <- rec |> recipes::step_impute_bag(
            recipes::all_predictors(),
            trees = as.integer(input$bag_trees)
          )
        }
        
        prep_rec <- recipes::prep(rec, training = train_df)
        baked    <- recipes::bake(prep_rec, new_data = apply_df)
        
        t_elapsed <- round(proc.time()[["elapsed"]] - t_start, 2)
        
        # ── per-column summary ─────────────────────────────────────────
        
        col_summary <- do.call(rbind, lapply(names(apply_df), function(cn) {
          before <- sum(is.na(apply_df[[cn]]))
          after  <- if (cn %in% names(baked)) sum(is.na(baked[[cn]])) else NA_integer_
          data.frame(
            Column         = cn,
            Type           = class(apply_df[[cn]])[1],
            Missing_Before = before,
            Missing_After  = as.integer(after),
            Imputed        = as.integer(before - after),
            stringsAsFactors = FALSE
          )
        }))
        
        impute_result(list(
          baked       = baked,
          col_summary = col_summary,
          n_imputed   = sum(col_summary$Imputed, na.rm = TRUE),
          elapsed     = t_elapsed,
          alg         = alg,
          mode        = mode,
          n_train     = nrow(train_df),
          n_apply     = nrow(apply_df),
          knn_k       = if (alg == "knn") input$knn_k       else NULL,
          bag_trees   = if (alg == "bag") input$bag_trees   else NULL,
          mmm_fn      = if (alg == "mmm") input$mmm_num_fn  else NULL
        ))
        
      }, error = function(e) {
        showNotification(
          paste("Imputation error:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })
    
    # ── main UI ──────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      
      res <- impute_result()
      
      # idle placeholder
      if (is.null(res)) {
        return(
          tags$div(
            style = "display:flex; flex-direction:column; align-items:center;
                     justify-content:center; min-height:340px;",
            tags$div(
              style = "font-size:52px; color:#dee2e6; margin-bottom:16px;",
              HTML("&#9654;")   # plain HTML play symbol — no FA dependency needed
            ),
            tags$div(
              style = "font-size:14px; color:#6c757d;",
              "Configure options on the right, then click ",
              tags$strong("Run Imputation", style = "color:#185FA5;"), "."
            )
          )
        )
      }
      
      # stat-card helper
      card <- function(label, value, color) {
        tags$div(
          style = paste0(
            "flex:1; min-width:130px; background:white; border-radius:10px;",
            "border:0.5px solid #dee2e6; padding:14px 18px;",
            "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
          ),
          tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                            text-transform:uppercase; letter-spacing:.5px;", label),
          tags$div(style = paste0("font-size:26px; font-weight:700; color:", color, ";"),
                   value)
        )
      }
      
      alg_label <- switch(res$alg,
                          mmm = paste0("MMM (", toupper(res$mmm_fn), ")"),
                          knn = paste0("KNN (K = ", res$knn_k, ")"),
                          bag = paste0("Bagged Trees (", res$bag_trees, " trees)")
      )
      
      mode_label <- switch(res$mode,
                           all    = "All observations\n(diagnose)",
                           split  = "Train \u2192 Testset",
                           unseen = "Train \u2192 Unseen rows"
      )
      mode_color <- switch(res$mode,
                           all    = "#534AB7",
                           split  = "#185FA5",
                           unseen = "#0F6E56"
      )
      
      cards <- tags$div(
        style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
        card("Cells Imputed",  format(res$n_imputed, big.mark = ","), "#185FA5"),
        card("Algorithm",      alg_label,                              "#0F6E56"),
        card("Mode",           mode_label,                             mode_color),
        card("Time (sec)",     res$elapsed,                            "#BA7517")
      )
      
      tbl_section <- tags$div(
        style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                 padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$h5(
          icon("table-cells", style = "color:#185FA5; margin-right:6px;"),
          "Per-Column Imputation Summary",
          style = "font-weight:600; color:#343a40; margin-bottom:14px;"
        ),
        DT::dataTableOutput(ns("col_tbl"))
      )
      
      tagList(cards, tbl_section)
    })
    
    # ── column table ──────────────────────────────────────────────────────
    
    output$col_tbl <- DT::renderDataTable({
      res <- impute_result()
      req(res)
      
      DT::datatable(
        res$col_summary,
        options  = list(pageLength = 15, dom = "tip"),
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "Imputed",
          color      = DT::styleInterval(0, c("#adb5bd", "#185FA5")),
          fontWeight = DT::styleInterval(0, c("normal", "bold"))
        ) |>
        DT::formatStyle(
          "Missing_Before",
          color = DT::styleInterval(0, c("#adb5bd", "#343a40"))
        )
    })
    
    # ── return ────────────────────────────────────────────────────────────
    
    return(list(
      data = reactive({
        res <- impute_result()
        if (is.null(res)) get_data() else res$baked
      })
    ))
    
  })
}