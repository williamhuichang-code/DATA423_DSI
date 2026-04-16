# =================================================================================
# mod_out_response.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

out_response_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
               min-height:100vh; padding-left:20px;",
      
      div(
        style = "font-size:13px; color:#343a40; background:white; padding:10px;
                 border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;
                 box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Outlier Response</b><br><br>
              Statistical methods alone cannot justify treating an observation
              as an outlier. <b>Always consult a domain expert</b> before
              taking any action.")
      ),
      div(
        style = "font-size:12px; color:#842029; background:#f8d7da;
                 border-left:3px solid #f5c2c7; padding:8px 10px;
                 border-radius:4px; margin-bottom:12px;",
        icon("user-doctor", style = "color:#842029;"),
        HTML(" <b>Domain expert confirmation required</b> before modifying
              or omitting any observation.")
      ),
      hr(),
      
      selectInput(ns("id_col"), "ID / label column:", choices = NULL),
      hr(),
      
      # ── Global Rules ──────────────────────────────────────────────────
      tags$label("Global Rules:",
                 style = "font-size:13px; font-weight:600; color:#343a40;
                          display:block; margin-bottom:6px;"),
      helpText("Bad values here are replaced across ALL columns where they appear
               (numeric columns for numeric values, character columns for string values).
               Applied before column-specific rules."),
      tags$div(id = ns("global_rules_container")),
      actionButton(ns("add_global_rule"), "+ Add Global Rule", icon = icon("plus"),
                   width = "100%",
                   style = "margin-top:6px; margin-bottom:6px;"),
      hr(),
      
      # ── Column-Specific Rules ──────────────────────────────────────────
      tags$label("Column-Specific Rules:",
                 style = "font-size:13px; font-weight:600; color:#343a40;
                          display:block; margin-bottom:6px;"),
      helpText("For each rule: select a column, enter bad values (comma-sep),
               then choose how to replace them."),
      tags$div(id = ns("col_rules_container")),
      actionButton(ns("add_rule"), "+ Add Column Rule", icon = icon("plus"),
                   width = "100%",
                   style = "margin-top:6px; margin-bottom:6px;"),
      hr(),
      
      # ── Omit by ID ────────────────────────────────────────────────────
      tags$label("Omit Observations by ID:",
                 style = "font-size:13px; font-weight:600; color:#343a40;
                          display:block; margin-bottom:4px;"),
      div(
        style = "font-size:11px; color:#842029; background:#f8d7da;
                 border-left:3px solid #f5c2c7; padding:6px 8px;
                 border-radius:4px; margin-bottom:8px;",
        icon("triangle-exclamation", style = "color:#842029;"),
        HTML(" Only omit if not expected in future unseen data.")
      ),
      selectizeInput(ns("omit_ids"), label = NULL,
                     choices  = NULL, multiple = TRUE,
                     options  = list(placeholder = "Select IDs to omit...")),
      hr(),
      
      # ── Notes ─────────────────────────────────────────────────────────
      textAreaInput(ns("notes"), "Notes (optional):",
                    placeholder = "Domain knowledge justification...",
                    rows = 2, width = "100%"),
      hr(),
      
      actionButton(ns("reset"), "Reset All", icon = icon("rotate-left"),
                   width = "100%")
    ),
    
    mainPanel(
      width = 9,
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

out_response_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── state ──────────────────────────────────────────────────────────────
    n_global_rules <- reactiveVal(0)
    n_rules        <- reactiveVal(0)
    
    # ── helper: build one global rule block ─────────────────────────────────
    make_global_rule_ui <- function(i) {
      method_id <- ns(paste0("grule_method_", i))
      tags$div(
        id    = paste0("grule_block_", i),
        style = "background:#fff8e1; border-radius:6px; padding:10px;
                 margin-bottom:10px; border:1px solid #ffe082;",
        tags$label(paste("Global Rule", i),
                   style = "font-size:11px; font-weight:600;
                            color:#6c757d; margin-bottom:6px; display:block;"),
        textInput(ns(paste0("grule_bad_", i)),
                  "Bad values (comma-sep):",
                  placeholder = "e.g. -99, -999, 9999", width = "100%"),
        selectInput(ns(paste0("grule_method_", i)), "Replace with:",
                    choices  = c("Value" = "value", "Set to NA" = "set_na"),
                    selected = "set_na", width = "100%"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'value'", method_id),
          textInput(ns(paste0("grule_newval_", i)),
                    "New value:", placeholder = "domain-correct value",
                    width = "100%")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'set_na'", method_id),
          div(style = "font-size:11px; color:#856404; background:#fff3cd;
                       border-left:3px solid #ffc107; padding:5px 8px;
                       border-radius:4px;",
              icon("triangle-exclamation", style = "color:#ffc107;"),
              HTML(" Bad values set to <code>NA</code> across all columns.
                    Handle via Miss Strategy afterwards."))
        )
      )
    }
    
    # ── helper: build one column rule block ─────────────────────────────────
    make_col_rule_ui <- function(i, cols) {
      method_id <- ns(paste0("rule_method_", i))
      tags$div(
        id    = paste0("rule_block_", i),
        style = "background:#f8f9fa; border-radius:6px; padding:10px;
                 margin-bottom:10px; border:1px solid #dee2e6;",
        tags$label(paste("Rule", i),
                   style = "font-size:11px; font-weight:600;
                            color:#6c757d; margin-bottom:6px; display:block;"),
        selectInput(ns(paste0("rule_col_", i)), "Column:",
                    choices = cols, width = "100%"),
        textInput(ns(paste0("rule_bad_", i)),
                  "Bad values (comma-sep):",
                  placeholder = "e.g. -99, 14", width = "100%"),
        selectInput(ns(paste0("rule_method_", i)), "Replace with:",
                    choices  = c("Value"     = "value",
                                 "Set to NA" = "set_na",
                                 "Winsorise" = "winsorise"),
                    selected = "value", width = "100%"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'value'", method_id),
          textInput(ns(paste0("rule_newval_", i)),
                    "New value:", placeholder = "domain-correct value",
                    width = "100%")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'set_na'", method_id),
          div(style = "font-size:11px; color:#856404; background:#fff3cd;
                       border-left:3px solid #ffc107; padding:5px 8px;
                       border-radius:4px;",
              icon("triangle-exclamation", style = "color:#ffc107;"),
              HTML(" Bad values set to <code>NA</code>.
                    Handle via Miss Strategy afterwards."))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'winsorise'", method_id),
          selectInput(ns(paste0("rule_winmethod_", i)), "Cap by:",
                      choices  = c("IQR whisker"  = "iqr",
                                   "Percentile"   = "percentile",
                                   "Manual value" = "manual"),
                      width = "100%"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'iqr'",
                                ns(paste0("rule_winmethod_", i))),
            numericInput(ns(paste0("rule_iqrk_", i)), "IQR k:",
                         value = 1.5, min = 0.1, step = 0.5, width = "100%")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'percentile'",
                                ns(paste0("rule_winmethod_", i))),
            sliderInput(ns(paste0("rule_pct_", i)), "Tail %:",
                        min = 1, max = 20, value = 5, step = 1, width = "100%")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'",
                                ns(paste0("rule_winmethod_", i))),
            textInput(ns(paste0("rule_manval_", i)), "Cap value:",
                      placeholder = "e.g. 1.0", width = "100%")
          )
        )
      )
    }
    
    # ── add / reset observers ───────────────────────────────────────────────
    observeEvent(input$add_global_rule, {
      i <- n_global_rules() + 1
      n_global_rules(i)
      insertUI(
        selector = paste0("#", ns("global_rules_container")),
        where    = "beforeEnd",
        ui       = make_global_rule_ui(i)
      )
    })
    
    observeEvent(input$add_rule, {
      req(get_data())
      i    <- n_rules() + 1
      cols <- c("(select)" = "", names(get_data()))
      n_rules(i)
      insertUI(
        selector = paste0("#", ns("col_rules_container")),
        where    = "beforeEnd",
        ui       = make_col_rule_ui(i, cols)
      )
    })
    
    observeEvent(input$reset, {
      # remove all inserted rule blocks from the DOM
      for (i in seq_len(n_global_rules()))
        removeUI(selector = paste0("#grule_block_", i), immediate = TRUE)
      for (i in seq_len(n_rules()))
        removeUI(selector = paste0("#rule_block_",  i), immediate = TRUE)
      n_global_rules(0)
      n_rules(0)
      updateSelectizeInput(session, "omit_ids", selected = character(0))
      updateTextAreaInput(session, "notes", value = "")
    })
    
    # ── seed one rule of each type on load ──────────────────────────────────
    observe({
      req(get_data())
      # only seed once
      if (n_global_rules() == 0) {
        i <- 1; n_global_rules(i)
        insertUI(
          selector = paste0("#", ns("global_rules_container")),
          where    = "beforeEnd",
          ui       = make_global_rule_ui(i)
        )
      }
      if (n_rules() == 0) {
        i    <- 1
        cols <- c("(select)" = "", names(get_data()))
        n_rules(i)
        insertUI(
          selector = paste0("#", ns("col_rules_container")),
          where    = "beforeEnd",
          ui       = make_col_rule_ui(i, cols)
        )
      }
    }) |> bindEvent(get_data(), once = TRUE)
    
    # ── populate selectors ──────────────────────────────────────────────────
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      updateSelectInput(session, "id_col",
                        choices  = names(df),
                        selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    })
    
    observe({
      df <- get_data(); req(df)
      req(input$id_col %in% names(df))
      ids <- as.character(unique(df[[input$id_col]]))
      updateSelectizeInput(session, "omit_ids", choices = ids, server = TRUE)
    })
    
    # ── collect global rules ────────────────────────────────────────────────
    r_global_rules <- reactive({
      rules <- list()
      for (i in seq_len(n_global_rules())) {
        bad_str <- trimws(input[[paste0("grule_bad_", i)]] %||% "")
        method  <- input[[paste0("grule_method_", i)]] %||% "set_na"
        if (nchar(bad_str) == 0) next
        bad_vals <- trimws(strsplit(bad_str, ",")[[1]])
        rules[[length(rules) + 1]] <- list(
          bad_vals = bad_vals,
          method   = method,
          new_val  = trimws(input[[paste0("grule_newval_", i)]] %||% "")
        )
      }
      rules
    })
    
    # ── collect column rules ────────────────────────────────────────────────
    r_col_rules <- reactive({
      rules <- list()
      for (i in seq_len(n_rules())) {
        col      <- input[[paste0("rule_col_",    i)]] %||% ""
        bad_str  <- trimws(input[[paste0("rule_bad_",    i)]] %||% "")
        method   <- input[[paste0("rule_method_", i)]] %||% "value"
        if (col == "" || col == "(select)") next
        if (nchar(bad_str) == 0) next
        bad_vals <- trimws(strsplit(bad_str, ",")[[1]])
        rules[[length(rules) + 1]] <- list(
          col        = col,
          bad_vals   = bad_vals,
          method     = method,
          new_val    = trimws(input[[paste0("rule_newval_",   i)]] %||% ""),
          win_method = input[[paste0("rule_winmethod_", i)]] %||% "iqr",
          iqr_k      = as.numeric(input[[paste0("rule_iqrk_",    i)]] %||% 1.5),
          pct        = as.numeric(input[[paste0("rule_pct_",     i)]] %||% 5),
          manual_val = trimws(input[[paste0("rule_manval_",  i)]] %||% "")
        )
      }
      rules
    })
    
    # ── processed data ──────────────────────────────────────────────────────
    processed <- reactive({
      df     <- get_data(); req(df)
      g_rules <- r_global_rules()
      rules  <- r_col_rules()
      id_col <- input$id_col
      omit   <- input$omit_ids %||% character(0)
      
      # 1. omit rows
      if (length(omit) > 0 && id_col %in% names(df)) {
        df <- df[!as.character(df[[id_col]]) %in% omit, , drop = FALSE]
      }
      
      # 2. global rules — applied across all columns
      for (rule in g_rules) {
        bad_num <- suppressWarnings(as.numeric(rule$bad_vals))
        bad_num <- bad_num[!is.na(bad_num)]
        
        for (col in names(df)) {
          x <- df[[col]]
          
          target <- if (is.numeric(x)) {
            length(bad_num) > 0 & x %in% bad_num
          } else {
            as.character(x) %in% rule$bad_vals
          }
          if (!any(target, na.rm = TRUE)) next
          
          if (rule$method == "set_na") {
            df[[col]][target] <- NA
          } else if (rule$method == "value") {
            if (nchar(rule$new_val) == 0) next
            typed <- tryCatch(
              if (is.numeric(x)) as.numeric(rule$new_val) else rule$new_val,
              error = function(e) rule$new_val)
            df[[col]][target] <- typed
          }
        }
      }
      
      # 3. column-specific rules
      for (rule in rules) {
        col <- rule$col
        if (!col %in% names(df)) next
        x <- df[[col]]
        
        target <- if (is.numeric(x)) {
          bad_num <- suppressWarnings(as.numeric(rule$bad_vals))
          x %in% bad_num[!is.na(bad_num)]
        } else {
          as.character(x) %in% rule$bad_vals
        }
        if (!any(target, na.rm = TRUE)) next
        
        if (rule$method == "set_na") {
          df[[col]][target] <- NA
          
        } else if (rule$method == "value") {
          if (nchar(rule$new_val) == 0) next
          typed <- tryCatch(
            if (is.numeric(x)) as.numeric(rule$new_val) else rule$new_val,
            error = function(e) rule$new_val)
          df[[col]][target] <- typed
          
        } else if (rule$method == "winsorise") {
          x_ref <- x[!target & !is.na(x)]
          cap <- switch(rule$win_method,
                        "iqr" = {
                          lims <- boxplot.stats(x_ref, coef = rule$iqr_k)$stats
                          list(lo = lims[1], hi = lims[5])
                        },
                        "percentile" = {
                          p <- rule$pct / 100
                          list(lo = quantile(x_ref, p,     na.rm = TRUE),
                               hi = quantile(x_ref, 1 - p, na.rm = TRUE))
                        },
                        "manual" = {
                          v <- suppressWarnings(as.numeric(rule$manual_val))
                          list(lo = -Inf, hi = if (is.na(v)) max(x_ref, na.rm=TRUE) else v)
                        }
          )
          df[[col]][target] <- pmin(pmax(x[target], cap$lo), cap$hi)
        }
      }
      
      df
    })
    
    # ── change log ──────────────────────────────────────────────────────────
    change_log <- reactive({
      df_orig <- get_data(); req(df_orig)
      df_new  <- processed()
      
      shared_rows <- intersect(rownames(df_orig), rownames(df_new))
      rows <- list()
      for (col in names(df_orig)) {
        if (!col %in% names(df_new)) next
        orig <- df_orig[shared_rows, col]
        new  <- df_new[shared_rows,  col]
        changed <- which(
          (is.na(orig) != is.na(new)) |
            (!is.na(orig) & !is.na(new) &
               as.character(orig) != as.character(new))
        )
        if (length(changed) == 0) next
        rows[[length(rows) + 1]] <- data.frame(
          Row    = shared_rows[changed],
          Column = col,
          Before = as.character(orig[changed]),
          After  = as.character(new[changed]),
          stringsAsFactors = FALSE
        )
      }
      if (length(rows) == 0) return(data.frame())
      log_df <- do.call(rbind, rows)
      log_df[order(log_df$Row, log_df$Column), ]
    })
    
    # ── main UI ─────────────────────────────────────────────────────────────
    output$main_ui <- renderUI({
      req(get_data())
      log_df   <- change_log()
      n_change <- nrow(log_df)
      n_omit   <- length(input$omit_ids %||% character(0))
      
      card <- function(label, value, color) {
        tags$div(
          style = paste0("flex:1; min-width:130px; background:white;
                          border-radius:10px; border:0.5px solid #dee2e6;
                          padding:14px 18px; box-shadow:0 1px 3px rgba(0,0,0,0.06);"),
          tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                            text-transform:uppercase; letter-spacing:.5px;", label),
          tags$div(style = paste0("font-size:28px; font-weight:700; color:", color, ";"),
                   value)
        )
      }
      
      tagList(
        tags$div(
          style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
          card("Cells Modified", n_change,
               if (n_change > 0) "#185FA5" else "#6c757d"),
          card("Rows Affected", if (n_change > 0) length(unique(log_df$Row)) else 0,
               if (n_change > 0) "#0F6E56" else "#6c757d"),
          card("Cols Affected", if (n_change > 0) length(unique(log_df$Column)) else 0,
               if (n_change > 0) "#534AB7" else "#6c757d"),
          card("Rows Omitted", n_omit,
               if (n_omit > 0) "#C41E3A" else "#6c757d")
        ),
        
        if (!is.null(input$notes) && nchar(trimws(input$notes)) > 0)
          tags$div(
            style = "background:#fff3cd; border-left:3px solid #ffc107;
                     padding:8px 12px; border-radius:4px; margin-bottom:16px;
                     font-size:12px; color:#495057;",
            icon("note-sticky", style = "color:#856404;"),
            HTML(paste0(" <b>Note:</b> ", trimws(input$notes)))
          ),
        
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5("Change Log",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          if (n_change == 0)
            tags$p("No values modified yet.",
                   style = "color:#adb5bd; font-size:13px;")
          else
            DT::dataTableOutput(ns("change_log_tbl"))
        ),
        
        if (n_change > 0) tags$div(
          style = "display:grid; grid-template-columns:1fr 1fr; gap:16px;",
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#6c757d; margin-right:6px;"),
                    "Before",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            DT::dataTableOutput(ns("before_tbl"))
          ),
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"),
                    "After",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            DT::dataTableOutput(ns("after_tbl"))
          )
        )
      )
    })
    
    # ── tables ──────────────────────────────────────────────────────────────
    dt_opts <- list(pageLength = 10, scrollX = TRUE, dom = "tip")
    
    output$change_log_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      DT::datatable(change_log(), options = dt_opts, rownames = FALSE) |>
        DT::formatStyle("After", color = "#185FA5", fontWeight = "bold")
    })
    
    output$before_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      rows <- unique(change_log()$Row)
      raw  <- tryCatch(get_raw(), error = function(e) get_data())
      show <- intersect(rows, rownames(raw))
      DT::datatable(raw[show, , drop = FALSE], options = dt_opts, rownames = TRUE)
    })
    
    output$after_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      rows <- unique(change_log()$Row)
      proc <- processed()
      show <- intersect(rows, rownames(proc))
      DT::datatable(proc[show, , drop = FALSE], options = dt_opts, rownames = TRUE)
    })
    
    # ── return ──────────────────────────────────────────────────────────────
    return(list(data = processed))
  })
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b