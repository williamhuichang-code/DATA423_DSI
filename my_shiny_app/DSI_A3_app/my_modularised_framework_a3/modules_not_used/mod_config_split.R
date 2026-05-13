# =================================================================================
# mod_config_split.R
# =================================================================================

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
        HTML("&nbsp; <b>Split Config</b><br><br>
              Define how the dataset is split into train and test sets.")
      ),
      hr(),
      
      # ── Response variable ─────────────────────────────────────────────────
      tags$div(
        style = "margin-bottom:12px;",
        tags$label("Response Variable:",
                   style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
        selectInput(ns("response_var"), label = NULL, choices = c("(none)"), width = "100%")
      ),
      hr(),
      
      # ── Split method ──────────────────────────────────────────────────────
      tags$label("Split Method:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      radioButtons(ns("split_method"), label = NULL,
                   choices = c("Use a split column" = "column",
                               "Use ratio split"    = "ratio"),
                   selected = "column"),
      
      # ── Column split options ──────────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("split_method"), "'] == 'column'"),
        tags$div(
          style = "margin-top:8px;",
          tags$label("Split Column:",
                     style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
          selectInput(ns("split_col"), label = NULL, choices = c("(none)"), width = "100%")
        ),
        conditionalPanel(
          condition = paste0("input['", ns("split_col"), "'] != '(none)'"),
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
        )
      ),
      
      # ── Ratio split options ───────────────────────────────────────────────
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
          tags$label("New split column name:",
                     style = "font-weight:600; font-size:13px; color:#343a40; display:block; margin-bottom:4px;"),
          textInput(ns("ratio_col_name"), label = NULL,
                    value = "ratio_split_col",
                    placeholder = "e.g. ratio_split_col")
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

split_server <- function(id, get_data, global_seed = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Populate column selectors from data ───────────────────────────────
    
    observe({
      df       <- get_data(); req(df)
      all_vars <- names(df)
      updateSelectInput(session, "response_var",
                        choices  = c("(none)", all_vars),
                        selected = "(none)")
      updateSelectInput(session, "split_col",
                        choices  = c("(none)", all_vars),
                        selected = "(none)")
    }) |> bindEvent(get_data())
    
    # ── Level selectors when split col is chosen ──────────────────────────
    
    observe({
      req(input$split_col, input$split_col != "(none)")
      df   <- get_data(); req(df)
      lvls <- as.character(unique(df[[input$split_col]]))
      lvls <- lvls[!is.na(lvls)]
      updateSelectInput(session, "train_level", choices = c("(none)", lvls), selected = "(none)")
      updateSelectInput(session, "test_level",  choices = c("(none)", lvls), selected = "(none)")
    })
    
    # ── Sync ratio_seed from global_seed ──────────────────────────────────
    
    observe({
      req(!is.null(global_seed))
      updateNumericInput(session, "ratio_seed", value = global_seed())
    }) |> bindEvent(if (!is.null(global_seed)) global_seed() else NULL)
    
    observeEvent(input$reset, {
      updateRadioButtons(session, "split_method",   selected = "column")
      updateSliderInput(session,  "train_ratio",    value = 80)
      updateTextInput(session,    "ratio_col_name", value = "ratio_split_col")
      updateSelectInput(session,  "split_col",      selected = "(none)")
      updateSelectInput(session,  "response_var",   selected = "(none)")
    })
    
    # ── Split reactive ────────────────────────────────────────────────────
    
    split_result <- reactive({
      df <- get_data(); req(df, input$split_method)
      
      if (input$split_method == "column") {
        req(input$split_col, input$split_col != "(none)",
            input$split_col %in% names(df),
            input$train_level, input$train_level != "(none)",
            input$test_level,  input$test_level  != "(none)")
        
        tl  <- input$train_level
        ts  <- input$test_level
        col <- as.character(df[[input$split_col]])
        
        list(
          train       = df[col == tl, , drop = FALSE],
          test        = df[col == ts, , drop = FALSE],
          method      = "column",
          split_col   = input$split_col,
          train_level = tl,
          test_level  = ts,
          full_df     = df
        )
        
      } else {
        req(input$train_ratio)
        ratio    <- input$train_ratio / 100
        n        <- nrow(df)
        seed_val <- input$ratio_seed
        if (!is.null(seed_val) && !is.na(seed_val)) set.seed(as.integer(seed_val))
        train_idx <- sample(seq_len(n), floor(n * ratio))
        
        col_name <- trimws(input$ratio_col_name)
        if (!nzchar(col_name)) col_name <- "ratio_split_col"
        
        df[[col_name]] <- factor(
          ifelse(seq_len(n) %in% train_idx, "Train", "Test"),
          levels = c("Train", "Test")
        )
        
        list(
          train       = df[ train_idx, , drop = FALSE],
          test        = df[-train_idx, , drop = FALSE],
          method      = "ratio",
          split_col   = col_name,
          train_level = "Train",
          test_level  = "Test",
          full_df     = df
        )
      }
    })
    
    # ── Main UI ───────────────────────────────────────────────────────────
    
    output$main_ui <- renderUI({
      res <- tryCatch(split_result(), error = function(e) NULL)
      
      if (is.null(res)) {
        return(div(
          style = "text-align:center; color:#6c757d; padding:60px 0;",
          icon("circle-info", style = "font-size:32px; color:#adb5bd; margin-bottom:10px;"),
          br(),
          tags$span("Configure split settings on the right.", style = "font-size:15px;")
        ))
      }
      
      train   <- res$train
      test    <- res$test
      resp    <- input$response_var
      n_train <- nrow(train)
      n_test  <- nrow(test)
      n_total <- n_train + n_test
      
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
        card("Total Obs", n_total, "#343a40"),
        card("Train",     n_train, "#185FA5"),
        card("Test",      n_test,  "#0F6E56"),
        card("Train %",   paste0(round(n_train / n_total * 100), "%"), "#534AB7")
      )
      
      ratio_info <- if (res$method == "ratio") {
        tags$div(
          style = "background:#e8f0fe; border:0.5px solid #a8c0fd; border-radius:8px;
                   padding:10px 14px; margin-bottom:16px; font-size:13px; color:#185FA5;",
          icon("info-circle"),
          HTML(paste0(" Ratio split created column <strong>", res$split_col,
                      "</strong> with Train/Test labels."))
        )
      } else NULL
      
      # class imbalance if response is categorical
      resp_in_data <- !is.null(resp) && resp != "(none)" && resp %in% names(get_data())
      is_cat <- resp_in_data &&
        (is.factor(get_data()[[resp]]) || is.character(get_data()[[resp]]))
      
      imbalance_section <- if (is_cat && resp %in% names(train) && resp %in% names(test)) {
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-top:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5(icon("scale-balanced", style = "color:#BA7517; margin-right:6px;"),
                  paste0("Class Imbalance — ", resp),
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          DT::dataTableOutput(ns("imbalance_tbl"))
        )
      } else if (!is_cat && resp_in_data) {
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-top:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$p(icon("circle-info", style = "color:#6c757d;"),
                 paste0(" ", resp, " is numeric — class imbalance table not applicable."),
                 style = "color:#6c757d; font-size:13px; margin:0;")
        )
      } else NULL
      
      tagList(ratio_info, cards, imbalance_section)
    })
    
    # ── Seed warning ──────────────────────────────────────────────────────
    
    output$seed_warning <- renderUI({
      if (is.na(input$ratio_seed) || is.null(input$ratio_seed)) {
        tags$div(
          style = "font-size:11px; color:#856404; background:#fff3cd;
                   border:0.5px solid #ffc107; border-radius:4px;
                   padding:6px 8px; margin-top:4px;",
          icon("triangle-exclamation"),
          " No seed set — results will differ each run."
        )
      }
    })
    
    # ── Imbalance table ───────────────────────────────────────────────────
    
    output$imbalance_tbl <- DT::renderDataTable({
      res  <- split_result(); req(res)
      resp <- input$response_var
      req(resp, resp != "(none)", resp %in% names(res$train))
      
      make_tbl <- function(df_split, label) {
        tbl        <- as.data.frame(table(as.character(df_split[[resp]])),
                                    stringsAsFactors = FALSE)
        names(tbl) <- c("Class", "N")
        tbl$Pct    <- paste0(round(tbl$N / sum(tbl$N) * 100, 1), "%")
        tbl$Split  <- label
        tbl[, c("Split", "Class", "N", "Pct")]
      }
      
      combined <- rbind(make_tbl(res$train, "Train"), make_tbl(res$test, "Test"))
      DT::datatable(combined, options = list(pageLength = 20, dom = "t"), rownames = FALSE) |>
        DT::formatStyle("Split",
                        target          = "row",
                        backgroundColor = DT::styleEqual("Train", "#EBF3FB"),
                        fontWeight      = "bold")
    })
    
    # ── Return ────────────────────────────────────────────────────────────
    
    return(list(
      train       = reactive(tryCatch(split_result()$train,       error = function(e) NULL)),
      test        = reactive(tryCatch(split_result()$test,        error = function(e) NULL)),
      split_col   = reactive(tryCatch(split_result()$split_col,   error = function(e) NULL)),
      train_level = reactive(tryCatch(split_result()$train_level, error = function(e) NULL)),
      test_level  = reactive(tryCatch(split_result()$test_level,  error = function(e) NULL)),
      data        = reactive(tryCatch(split_result()$full_df,     error = function(e) get_data()))
    ))
    
  })
}