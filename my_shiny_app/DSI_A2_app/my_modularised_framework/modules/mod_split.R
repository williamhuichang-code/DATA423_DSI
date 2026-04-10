# =================================================================================
# mod_split.R
# =================================================================================

# ── HELPER ───────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── UI ───────────────────────────────────────────────────────────────────────

split_ui <- function(id) {
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
        HTML("&nbsp; Tab Note: <br><br> Placeholder for now.")
      ),
      hr(),
      
      tags$div(
        style = "margin-bottom:12px;",
        tags$label("Response Variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
        selectInput(ns("response_var"), label = NULL, choices = NULL, width = "100%")
      ),
      hr(),
      
      tags$label("Split Method:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("split_method"), label = NULL,
                   choices = c("Use a split column" = "column",
                               "Use ratio split"    = "ratio"),
                   selected = "column"),
      
      conditionalPanel(
        condition = paste0("input['", ns("split_method"), "'] == 'column'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Split Column:",
                     style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
          selectInput(ns("split_col"), label = NULL, choices = NULL, width = "100%")
        )
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("split_method"), "'] == 'ratio'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Train %:",
                     style = "font-weight:600; font-size:13px; color:#343a40;"),
          sliderInput(ns("train_ratio"), label = NULL,
                      min = 50, max = 95, value = 80, step = 5, width = "100%",
                      post = "%")
        ),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Seed:",
                     style = "font-weight:600; font-size:13px; color:#343a40;"),
          numericInput(ns("ratio_seed"), label = NULL,
                       value = NA, min = 0, step = 1, width = "100%"),
          uiOutput(ns("seed_warning"))
        )
      ),
      hr(),
      
      actionButton(ns("reset"), label = "Reset", icon = icon("rotate-left"), width = "100%")
    ),
    
    mainPanel(
      width = 9,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

split_server <- function(id, get_data, get_roles = NULL, global_seed = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── populate selectors from data + roles ──────────────────────────────
    
    observe({
      df <- get_data()
      req(df)
      
      all_vars <- names(df)
      role_vals <- if (!is.null(get_roles)) get_roles() else NULL
      
      default_response <- if (!is.null(role_vals)) {
        vars <- names(role_vals[role_vals == "outcome"])
        if (length(vars) > 0) vars[1] else all_vars[1]
      } else all_vars[1]
      
      default_split_col <- if (!is.null(role_vals)) {
        vars <- names(role_vals[role_vals == "split"])
        if (length(vars) > 0) vars[1] else all_vars[1]
      } else all_vars[1]
      
      updateSelectInput(session, "response_var",
                        choices  = all_vars,
                        selected = default_response)
      
      updateSelectInput(session, "split_col",
                        choices  = all_vars,
                        selected = default_split_col)
      
    }) |> bindEvent(get_data(), if (!is.null(get_roles)) get_roles() else NULL)
    
    observeEvent(input$reset, {
      updateRadioButtons(session, "split_method", selected = "ratio")
      updateSliderInput(session, "train_ratio", value = 80)
    })
    
    # sync ratio_seed from global_seed when global_seed changes
    observe({
      req(!is.null(global_seed))
      updateNumericInput(session, "ratio_seed", value = global_seed())
    }) |> bindEvent(if (!is.null(global_seed)) global_seed() else NULL)
    
    # ── split reactive ────────────────────────────────────────────────────
    
    split_result <- reactive({
      df <- get_data()
      req(df, input$split_method)
      
      if (input$split_method == "column") {
        req(input$split_col, input$split_col %in% names(df))
        col        <- df[[input$split_col]]
        col_chr    <- tolower(trimws(as.character(col)))
        train_mask <- col_chr %in% c("train", "1", "true")
        test_mask  <- col_chr %in% c("test",  "0", "false")
        
        if (sum(train_mask) == 0 && sum(test_mask) == 0) {
          n         <- nrow(df)
          train_idx <- sample(seq_len(n), floor(n * 0.8))
          return(list(train = df[train_idx, , drop = FALSE],
                      test  = df[-train_idx, , drop = FALSE],
                      method = "column_fallback"))
        }
        
        list(train  = df[train_mask, , drop = FALSE],
             test   = df[test_mask,  , drop = FALSE],
             method = "column")
        
      } else {
        req(input$train_ratio)
        ratio     <- input$train_ratio / 100
        n         <- nrow(df)
        seed_val <- input$ratio_seed
        if (!is.null(seed_val) && !is.na(seed_val)) set.seed(as.integer(seed_val))
        train_idx <- sample(seq_len(n), floor(n * ratio))
        list(train  = df[ train_idx, , drop = FALSE],
             test   = df[-train_idx, , drop = FALSE],
             method = "ratio")
      }
    })
    
    # ── main UI ───────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      req(split_result(), input$response_var)
      
      res    <- split_result()
      train  <- res$train
      test   <- res$test
      resp   <- input$response_var
      
      n_train <- nrow(train)
      n_test  <- nrow(test)
      n_total <- n_train + n_test
      
      resp_in_data <- resp %in% names(get_data())
      is_cat <- resp_in_data && (is.factor(get_data()[[resp]]) || is.character(get_data()[[resp]]))
      
      # ── stat cards ───────────────────────────────────────────────────────
      
      card <- function(label, value, color) {
        tags$div(
          style = paste0(
            "flex:1; min-width:140px; background:white; border-radius:10px;",
            "border:0.5px solid #dee2e6; padding:14px 18px;",
            "box-shadow:0 1px 3px rgba(0,0,0,0.06);"
          ),
          tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                            text-transform:uppercase; letter-spacing:.5px;", label),
          tags$div(style = paste0("font-size:28px; font-weight:700; color:", color, ";"), value)
        )
      }
      
      cards <- tags$div(
        style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
        card("Total Obs",  n_total,  "#343a40"),
        card("Train",      n_train,  "#185FA5"),
        card("Test",       n_test,   "#0F6E56"),
        card("Train %",    paste0(round(n_train / n_total * 100), "%"), "#534AB7")
      )
      
      # ── class imbalance (categorical response only) ───────────────────
      
      imbalance_section <- if (is_cat && resp %in% names(train) && resp %in% names(test)) {
        
        make_imbalance_tbl <- function(df_split, label) {
          tbl <- as.data.frame(table(as.character(df_split[[resp]])),
                               stringsAsFactors = FALSE)
          names(tbl) <- c("Class", "N")
          tbl$Pct    <- paste0(round(tbl$N / sum(tbl$N) * 100, 1), "%")
          tbl$Split  <- label
          tbl[, c("Split", "Class", "N", "Pct")]
        }
        
        tbl_train <- make_imbalance_tbl(train, "Train")
        tbl_test  <- make_imbalance_tbl(test,  "Test")
        combined  <- rbind(tbl_train, tbl_test)
        
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-top:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(
            icon("scale-balanced", style = "color:#BA7517; margin-right:6px;"),
            paste0("Class Imbalance — ", resp),
            style = "font-weight:600; color:#343a40; margin-bottom:12px;"
          ),
          DT::dataTableOutput(ns("imbalance_tbl"))
        )
        
      } else if (!is_cat && resp_in_data) {
        
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-top:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$p(
            icon("circle-info", style = "color:#6c757d;"),
            paste0(" ", resp, " is numeric — class imbalance table not applicable."),
            style = "color:#6c757d; font-size:13px; margin:0;"
          )
        )
        
      } else NULL
      
      fallback_warn <- if (res$method == "column_fallback") {
        tags$div(
          style = "background:#fff3cd; border:0.5px solid #ffc107; border-radius:8px;
             padding:10px 14px; margin-bottom:16px; font-size:13px; color:#856404;",
          icon("triangle-exclamation"),
          " Selected split column has no recognised Train/Test values.
      Expected: 'Train'/'Test', '1'/'0', or 'TRUE'/'FALSE'. Showing 80/20 fallback."
        )
      } else NULL
      
      tagList(fallback_warn, cards, imbalance_section)
    })
    
    # ── seed warning ──────────────────────────────────────────────────────────
    output$seed_warning <- renderUI({
      seed_val <- input$ratio_seed
      if (is.na(seed_val) || is.null(seed_val)) {
        tags$div(
          style = "font-size:11px; color:#856404; background:#fff3cd;
               border:0.5px solid #ffc107; border-radius:4px;
               padding:6px 8px; margin-top:4px;",
          icon("triangle-exclamation"),
          " No seed set — results will differ each run. Set a value here or in Config > Global Seed."
        )
      }
    })
    
    # ── imbalance table render ────────────────────────────────────────────
    
    output$imbalance_tbl <- DT::renderDataTable({
      req(split_result(), input$response_var)
      res  <- split_result()
      resp <- input$response_var
      req(resp %in% names(res$train))
      
      make_tbl <- function(df_split, label) {
        tbl       <- as.data.frame(table(as.character(df_split[[resp]])),
                                   stringsAsFactors = FALSE)
        names(tbl) <- c("Class", "N")
        tbl$Pct    <- paste0(round(tbl$N / sum(tbl$N) * 100, 1), "%")
        tbl$Split  <- label
        tbl[, c("Split", "Class", "N", "Pct")]
      }
      
      combined <- rbind(make_tbl(res$train, "Train"), make_tbl(res$test, "Test"))
      
      DT::datatable(combined,
                    options  = list(pageLength = 20, dom = "t"),
                    rownames = FALSE) |>
        DT::formatStyle("Split",
                        target          = "row",
                        backgroundColor = DT::styleEqual("Train", "#EBF3FB"),
                        fontWeight      = "bold")
    })
    
    # ── return ────────────────────────────────────────────────────────────
    
    return(list(
      train = reactive(split_result()$train),
      test  = reactive(split_result()$test)
    ))
    
  })
}