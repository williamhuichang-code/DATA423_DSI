# =============================================================================
# server.R
# =============================================================================

server <- function(input, output, session) {
  
  # isolated R environment for the console (inheritance)
  #   so that I can read global objects
  #   assign while staying inside rconsole_env
  #   but my console code won't overwrite globalenv()
  
  rconsole_env <- new.env(parent = globalenv())
  
  
  # ── SERVER SECURITY REACTIVE ───────────────────────────────────────────
  
  # ·· LOCK STATE UPDATE ··············································
  
  # single reactive value drives everything security-related
  is_unlocked <- reactiveVal(FALSE)
  
  # toggle lock / unlock on button click
  observeEvent(input$privacy_unlock, {
    if (is_unlocked()) {
      # if already unlocked → lock again
      is_unlocked(FALSE)
      updateTextInput(session, "privacy_pass", value = "")
    } else {
      if (!is.null(input$privacy_pass) && input$privacy_pass == UNLOCK_PASSPHRASE) {
        is_unlocked(TRUE)
        updateTextInput(session, "privacy_pass", value = "")
      } else {
        showNotification("Incorrect passphrase.", type = "error", duration = 3)
      }
    }
  })
  
  # status badge next to the lock button (bigger and nice looking, this is intended, not a bug)
  output$privacy_status_ui <- renderUI({
    if (is_unlocked()) {
      tagList(
        actionButton("privacy_lock_btn", label = NULL, icon = icon("unlock"),
                     style = "color:#198754; background:none; border:none; font-size:18px; padding:0;",
                     title = "Click to lock"),
        span("Full access", style = "color:#198754; font-size:12px;")
      )
    } else {
      span(icon("lock"), " Private Mode",
           style = "color:#6c757d; font-size:12px;")
    }
  })
  
  
  # ·· DATASET SELECTOR UI ············································
  
  output$dataset_selector_ui <- renderUI({
    choices <- c("Raw Dataset", "EDA Dataset", "Enriched Dataset", "Model Dataset")
    
    # (injects "Debug Dataset" when unlocked)
    if (is_unlocked()) choices <- c(choices, "Debug Dataset")
    
    # preserve current selection
    current <- isolate(input$dataset_choice)
    if (is.null(current) || !current %in% choices) current <- "EDA Dataset"
    
    selectInput("dataset_choice", "Dataset Stage:",
                choices = choices, selected = current)
  })
  
  # info modal explaining each stage
  observeEvent(input$dataset_info, {
    showModal(modalDialog(
      title = "Dataset Stages",
      tags$dl(
        tags$dt("Raw Dataset"),
        tags$dd("Original data as loaded from disk. No changes."),
        tags$dt("EDA Dataset"),
        tags$dd("Raw + derived EDA helper columns for Exploratory Data Analysis 
                (e.g., grouping, pattern inspection, or data integrity checks) 
                without any preprocessing for modeling. 
                (fixed minor schema and naming inconsistencies so that 
                the dataset is correctly interpreted when loaded)"),
        tags$dt("Enriched Dataset"),
        tags$dd("Dataset with additional derived or contextual columns added 
                to enrich the information available for further modelling consideration."),
        tags$dt("Model Dataset"),
        tags$dd("Reduced dataset containing only the variables required for modelling, 
                with unnecessary or potentially leaky columns removed."),
        tags$dt("Debug Dataset", style = "color:#856404;"),
        tags$dd("Explore upon enriched dataset with diagnostic flags and intermediate values 
                for debugging or testing. Only visible when unlocked."),
        tags$dt("Note 1: ", style = "color:#856404;"),
        tags$dd("The Enriched/Model dataset stages are shown here as placeholders within 
                the overall framework. They are not meaningfully implemented at this stage."),
        tags$dt("Note 2: ", style = "color:#856404;"),
        tags$dd("The ID column is intentionally hidden across all dataset views 
                to demonstrate how the framework can protect sensitive attributes. 
                It becomes visible only when unlocked."),
      ),
      easyClose = TRUE,
      footer    = modalButton("Close")
    ))
  })
  
  
  # ── GLOBAL DATASET STAGE ───────────────────────────────────────────────
  
  # Note: everything downstream will read from display_data()
  
  # layer 1: selected_data()  —> picks the intended dataset stage to examine
  selected_data <- reactive({
    req(input$dataset_choice)
    switch(input$dataset_choice,
           "Raw Dataset"      = raw_dataset,
           "EDA Dataset"      = eda_dataset,
           "Enriched Dataset" = enriched_dataset,
           "Model Dataset"    = model_dataset,
           "Debug Dataset"    = debug_dataset,   # only reachable when unlocked
           enriched_dataset)                      # enriched dataset always as fallback
  })
  
  # layer 2: display_data()   —> additionally strips PRIVATE_COLS when locked
  display_data <- reactive({
    df   <- selected_data()
    drop <- intersect(PRIVATE_COLS, names(df))
    if (!is_unlocked() && length(drop) > 0) {
      df <- df[, !names(df) %in% drop, drop = FALSE]
    }
    df
  })
  
  
  # ── SERVER DATA TABLE ──────────────────────────────────────────────────
  
  # 1st block: preset dropdown initialisation only
  observe({
    df       <- display_data()
    all_vars <- names(df)
    
    valid_groups <- Filter(function(v) any(v %in% all_vars), presets_for("dtable"))
    choices_p    <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "dt_preset", choices = choices_p)
  })
  
  # 2nd block: column choices reset only when dataset stage changes
  observeEvent(input$dataset_choice, {
    df       <- display_data()
    all_vars <- names(df)
    updateSelectizeInput(session, "dt_cols", choices = all_vars)
    apply_preset_selection(session, "dtable", "dt_cols", all_vars)
  })
  
  # 3rd block: preset observer
  observeEvent(input$dt_preset, {
    req(input$dt_preset != "none")
    df       <- display_data()
    all_vars <- names(df)
    sel      <- intersect(VAR_PRESETS[[input$dt_preset]], all_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "dt_cols", selected = sel)
  })
  
  # 4th block: table output
  output$data_table <- DT::renderDataTable({
    
    df <- head(display_data(), input$dt_row_cap)
    
    if (!is.null(input$dt_cols) && length(input$dt_cols) > 0)
      df <- df[, intersect(input$dt_cols, names(df)), drop = FALSE]
    
    tbl_class <- paste(
      c("display", "nowrap",
        if (isTRUE(input$dt_compact)) "compact" else "cell-border"),
      collapse = " "
    )
    
    extensions <- c("Buttons", "ColReorder")
    freeze_n   <- max(0, as.integer(input$dt_freeze), na.rm = TRUE)
    if (freeze_n > 0) extensions <- c(extensions, "FixedColumns")
    
    opts <- list(
      pageLength = as.integer(input$dt_page_len),
      dom        = "Bfrtip",
      buttons    = c("copy", "csv", "excel", "pdf"),
      scrollX    = TRUE,
      autoWidth  = FALSE,
      colReorder = TRUE
    )
    
    if (freeze_n > 0)
      opts$fixedColumns <- list(leftColumns = freeze_n)
    
    DT::datatable(
      df,
      extensions = extensions,
      filter     = input$dt_filter,
      selection  = input$dt_selection,
      rownames   = FALSE,
      class      = tbl_class,
      caption    = paste0(input$dataset_choice, " — ",
                          nrow(df), " rows × ", ncol(df), " cols"),
      options    = opts
    )
  })
  
  
  # ── SERVER SUMMARY ─────────────────────────────────────────────────────
  
  output$summary_output <- renderUI({
    df <- display_data()
    if (input$summary_style == "base") {
      tags$pre(paste(capture.output(summary(df)), collapse = "\n"))
    } else if (input$summary_style == "glimpse") {
      tags$pre(paste(capture.output(tibble::glimpse(df)), collapse = "\n"))
    } else if (input$summary_style == "dfsummary") {
      summarytools::dfSummary(
        df
      ) |> print(method = "render")
    }
  })
  
  
  # ── SERVER WORD CLOUD ──────────────────────────────────────────────────
  
  # 1st block: categorical variable selector initialisation
  observe({
    df       <- display_data()
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "wc_var", choices = cat_vars, selected = cat_vars[1])
  })
  
  # 2nd block: plot output
  output$wc_plot <- renderPlot({
    df <- display_data()
    
    if (isTRUE(input$wc_varnames_mode)) {
      val <- names(df)
    } else {
      req(input$wc_var)
      val <- as.character(df[[input$wc_var]])
    }
    
    val <- val[!is.na(val) & nchar(trimws(val)) > 0]
    
    if (!input$wc_case) val <- tolower(val)
    
    validate(need(length(val) > 0, "No non-missing values to display."))
    
    tokens <- switch(input$wc_split_mode,
                     "none"     = val,
                     "chars"    = {
                       t <- unlist(strsplit(val, ""))
                       t[!grepl("\\s", t)]
                     },
                     "alphanum" = {
                       t <- unlist(lapply(val, function(tok) {
                         m <- gregexpr("[A-Za-z]+|[0-9]+", tok, perl = TRUE)
                         regmatches(tok, m)[[1]]
                       }))
                       t[nchar(t) > 0]
                     }
    )
    
    freq_table <- sort(table(tokens), decreasing = TRUE)
    freq_table <- freq_table[freq_table >= input$wc_min_freq]
    validate(need(length(freq_table) > 0,
                  paste0("No tokens appear at least ", input$wc_min_freq, " times.")))
    freq_table <- head(freq_table, input$wc_max_words)
    
    words <- names(freq_table)
    freqs <- as.integer(freq_table)
    n     <- length(words)
    
    pal    <- RColorBrewer::brewer.pal(max(3, min(8, n)), input$wc_palette)
    colors <- colorRampPalette(pal)(n)[rank(-freqs, ties.method = "first")]
    
    freq_ranks <- rank(-freqs, ties.method = "first")
    freq_ratio <- max(freqs) / max(median(freqs), 1)
    
    if (freq_ratio >= 3) {
      freqs_norm <- as.integer(20 + (sqrt(freqs) - sqrt(min(freqs))) /
                                 max(sqrt(max(freqs)) - sqrt(min(freqs)), 1) * 80)
    } else {
      freqs_norm <- as.integer(100 - (freq_ranks - 1) / max(freq_ranks - 1, 1) * 80)
    }
    
    n_eff      <- min(n, 30)
    base_scale <- max(3, min(9, 40 / sqrt(n_eff)))
    max_scale  <- min(12, base_scale * (1 + log10(max(freq_ratio, 1))))
    min_scale  <- max(0.3, max_scale * 0.15)
    max_scale  <- max_scale * input$wc_scale
    min_scale  <- min_scale * input$wc_scale
    
    par(mar = c(0, 0, 0, 0), bg = "white")
    
    wordcloud::wordcloud(
      words        = words,
      freq         = freqs_norm,
      max.words    = n,
      min.freq     = 1,
      random.order = FALSE,
      rot.per      = 0.15,
      colors       = colors,
      scale        = c(max_scale, min_scale),
      use.r.layout = FALSE
    )
  })
  
  
  # ── SERVER MISSINGNESS ─────────────────────────────────────────────────
  
  # 1st block: variable + group selectors initialisation
  observe({
    df       <- display_data()
    all_vars <- names(df)
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    # variable selector — default by preset
    updateSelectizeInput(session, "ms_vars", choices = all_vars)
    apply_preset_selection(session, "missing", "ms_vars", all_vars)
    
    # preset dropdown
    valid_groups <- Filter(function(v) any(v %in% all_vars), presets_for("missing"))
    choices_p    <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "ms_preset", choices = choices_p)
    
    apply_groupby_defaults(session, "missing", cat_vars, "ms_group_var")
    
    updateSelectInput(session, "ms_group_var2",
                      choices  = c("None" = "none", cat_vars),
                      selected = "none")
  })
  
  # preset observer for missingness
  observeEvent(input$ms_preset, {
    req(input$ms_preset != "none")
    df       <- display_data()
    all_vars <- names(df)
    sel      <- intersect(VAR_PRESETS[[input$ms_preset]], all_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "ms_vars", selected = sel)
  })
  
  # populate primary group levels when group var changes
  observeEvent(input$ms_group_var, {
    req(input$ms_group_var)
    df   <- display_data()
    lvls <- sort(unique(na.omit(as.character(df[[input$ms_group_var]]))))
    updateSelectizeInput(session, "ms_group_levels", choices = lvls, selected = lvls)
  })
  
  # populate secondary group levels when group var2 changes
  observeEvent(input$ms_group_var2, {
    req(input$ms_group_var2)
    if (input$ms_group_var2 == "none") {
      updateSelectizeInput(session, "ms_group_levels2", choices = character(0), selected = character(0))
      return()
    }
    df   <- display_data()
    lvls <- sort(unique(na.omit(as.character(df[[input$ms_group_var2]]))))
    updateSelectizeInput(session, "ms_group_levels2", choices = lvls, selected = lvls)
  })
  
  # 2nd block: vis_dat / vis_miss plot
  output$ms_output <- renderPlot({
    df <- display_data()
    req(input$ms_vars)
    
    sel_vars <- intersect(input$ms_vars, names(df))
    validate(need(length(sel_vars) >= 1, "Select at least 1 variable."))
    
    default_title <- paste0(
      if (input$ms_mode == "visdat") "Type-Wise Missingness Overview" else "Missingness Overview",
      if (input$ms_group_on && !is.null(input$ms_group_var) && input$ms_group_var %in% names(df)) {
        grp_label <- input$ms_group_var
        if (!is.null(input$ms_group_var2) && input$ms_group_var2 != "none" &&
            input$ms_group_var2 %in% names(df) && input$ms_group_var2 != input$ms_group_var)
          paste0(" | Grouped by ", grp_label, " & ", input$ms_group_var2)
        else
          paste0(" | Grouped by ", grp_label)
      } else ""
    )
    plot_title    <- if (nzchar(input$ms_title)) input$ms_title else default_title
    
    make_panel <- function(sub_df, subtitle = NULL) {
      sub_df <- sub_df[, sel_vars, drop = FALSE]
      
      # guard: empty subgroup, return a blank placeholder panel
      if (nrow(sub_df) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No data", size = 5, colour = "grey50") +
          ggplot2::theme_void()
        if (!is.null(subtitle))
          p <- p + ggplot2::labs(title = subtitle) +
            ggplot2::theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))
        return(p)
      }
      
      p <- if (input$ms_mode == "visdat") visdat::vis_dat(sub_df) else visdat::vis_miss(sub_df)
      p <- p + ggplot2::theme(
        axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12), # size for labels
        axis.text.y     = element_text(size = 11),
        axis.title      = element_text(size = 13),
        legend.text     = element_text(size = 11),
        legend.title    = element_text(size = 12),
        legend.position = "none"
      )
      if (!is.null(subtitle))
        p <- p + ggplot2::labs(title = subtitle) +
        ggplot2::theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5))  # or face = "plain"
      p
    }
    
    if (input$ms_group_on && !is.null(input$ms_group_var) && input$ms_group_var %in% names(df)) {
      
      grp1  <- input$ms_group_var
      lvls1 <- if (!is.null(input$ms_group_levels) && length(input$ms_group_levels) > 0)
        input$ms_group_levels else sort(unique(na.omit(as.character(df[[grp1]]))))
      
      grp2  <- if (!is.null(input$ms_group_var2) && input$ms_group_var2 != "none" &&
                   input$ms_group_var2 %in% names(df) && input$ms_group_var2 != grp1)
        input$ms_group_var2 else NULL
      lvls2 <- if (!is.null(grp2) && !is.null(input$ms_group_levels2) && length(input$ms_group_levels2) > 0)
        input$ms_group_levels2 else if (!is.null(grp2))
          sort(unique(na.omit(as.character(df[[grp2]])))) else NULL
      
      # filter df to selected levels
      df_filtered <- df[as.character(df[[grp1]]) %in% lvls1, , drop = FALSE]
      if (!is.null(grp2))
        df_filtered <- df_filtered[as.character(df_filtered[[grp2]]) %in% lvls2, , drop = FALSE]
      
      if (!is.null(grp2)) {
        plots <- list()
        for (l1 in lvls1) {
          for (l2 in lvls2) {
            sub <- df_filtered[as.character(df_filtered[[grp1]]) == l1 &
                                 as.character(df_filtered[[grp2]]) == l2, , drop = FALSE]
            plots[[length(plots) + 1]] <- make_panel(sub, paste0(grp1, "=", l1, " & ", grp2, "=", l2))
          }
        }
        ncols <- length(lvls2)
      } else {
        plots <- lapply(lvls1, function(l1) {
          sub <- df_filtered[as.character(df_filtered[[grp1]]) == l1, , drop = FALSE]
          make_panel(sub, paste0(grp1, " = ", l1))
        })
        ncols <- length(lvls1)
      }
      
      combined <- patchwork::wrap_plots(plots, ncol = ncols) +
        patchwork::plot_layout(guides = "collect")
      print(combined + patchwork::plot_annotation(
        title = plot_title,
        theme = ggplot2::theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
      ))
      
    } else {
      print(make_panel(df) +
              ggplot2::labs(title = plot_title) +
              ggplot2::theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)))
    }
  })
  
  # 3rd block: Little's MCAR test — on selected variables only
  output$ms_mcar_output <- renderPrint({
    req(input$ms_mcar)
    df       <- display_data()
    sel_vars <- intersect(input$ms_vars, names(df))
    
    # apply same level filtering as the plot
    if (input$ms_group_on && !is.null(input$ms_group_var) && input$ms_group_var %in% names(df)) {
      lvls1 <- if (!is.null(input$ms_group_levels) && length(input$ms_group_levels) > 0)
        input$ms_group_levels else sort(unique(na.omit(as.character(df[[input$ms_group_var]]))))
      df <- df[as.character(df[[input$ms_group_var]]) %in% lvls1, , drop = FALSE]
      
      if (!is.null(input$ms_group_var2) && input$ms_group_var2 != "none" &&
          input$ms_group_var2 %in% names(df) && input$ms_group_var2 != input$ms_group_var) {
        lvls2 <- if (!is.null(input$ms_group_levels2) && length(input$ms_group_levels2) > 0)
          input$ms_group_levels2 else sort(unique(na.omit(as.character(df[[input$ms_group_var2]]))))
        df <- df[as.character(df[[input$ms_group_var2]]) %in% lvls2, , drop = FALSE]
      }
    }
    
    num_df <- df[, sel_vars, drop = FALSE]
    num_df <- num_df[, sapply(num_df, is.numeric), drop = FALSE]
    
    validate(need(ncol(num_df) >= 2, "Select at least 2 numeric variables for the MCAR test."))
    
    has_na <- sapply(num_df, anyNA)
    validate(need(any(has_na), "No missing values in the selected numeric variables — MCAR test not applicable."))
    
    result <- tryCatch(
      naniar::mcar_test(num_df),
      error = function(e) paste("Test failed:", conditionMessage(e))
    )
    
    if (is.character(result)) {
      cat(result, "\n")
    } else {
      cat("Little's MCAR Test\n")
      cat(sprintf("Variables tested     : %d numeric columns\n", ncol(num_df)))
      cat(sprintf("Observation included : %d rows\n", nrow(num_df)))
      cat("──────────────────────────────\n")
      cat(sprintf("Chi-square statistic : %.4f\n", result$statistic))
      cat(sprintf("Degrees of freedom   : %d\n",   result$df))
      cat(sprintf("p-value              : %.4f\n",  result$p.value))
      cat("──────────────────────────────\n")
      if (result$p.value < 0.05) {
        cat("Conclusion: Reject H0 — missingness is NOT MCAR (likely MAR or MNAR).\n")
      } else {
        cat("Conclusion: Fail to reject H0 — data is consistent with MCAR.\n")
      }
    }
  })
  
  
  # ── SERVER RISING VALUE ────────────────────────────────────────────────
  
  # 1st block: variable selector and preset dropdown initialisation
  observe({
    df <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "rv_vars", choices = num_vars)
    apply_preset_selection(session, "rising", "rv_vars", num_vars)
    # dropdown only shows presets valid for this tab
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("rising"))
    choices <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "rv_preset", choices = choices)
  })
  
  # 2nd block: preset observer — apply selected preset to variable selector
  observeEvent(input$rv_preset, {
    # do nothing if "None" is selected
    req(input$rv_preset != "none")
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    # look up preset vars by name, keep only those that exist in current dataset
    sel <- intersect(VAR_PRESETS[[input$rv_preset]], num_vars)
    # guard: skip update if no matching vars found
    if (length(sel) > 0)
      updateSelectizeInput(session, "rv_vars", selected = sel)
  })
  
  # 3rd block: plot output
  output$rv_output <- renderPlotly({
    req(input$rv_vars)
    df       <- display_data()
    num_vars <- input$rv_vars
    colours  <- theme_colours_for(num_vars)
    # build long-format df
    plot_df <- do.call(rbind, lapply(num_vars, function(v) {
      y <- df[[v]]
      if (input$rv_omit_na) y <- na.omit(y)
      
      y <- switch(input$rv_transform,
                  "center"      = y - mean(y, na.rm = TRUE),
                  "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
                  "normalise"   = (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)),
                  y
      )
      
      y   <- sort(y, na.last = TRUE)
      pct <- seq_along(y) / length(y) * 100
      data.frame(Percentile = pct, Value = y, Variable = v)
    }))
    
    p <- ggplot(plot_df, aes(x = Percentile, y = Value,
                             colour = Variable,
                             group  = Variable,
                             text   = paste0(Variable,
                                             "<br>Percentile: ", round(Percentile, 1),
                                             "<br>Value: ",      round(Value, 3)))) +
      geom_line(linewidth = input$rv_lwd / 3, linetype = input$rv_lty) +
      scale_colour_manual(values = colours) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "right")
    
    default_title <- paste0(
      "Rising Value Graph for Continuity",
      if (input$rv_omit_na) " | NAs Omitted" else "",
      if (input$rv_transform != "none") paste0(" | ", tools::toTitleCase(input$rv_transform)) else ""
    )
    
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(
        text = if (nzchar(input$rv_title)) paste0("<b>", input$rv_title, "</b>") else 
          paste0("<b>", default_title, "</b>"),
        font = list(size = 28, color = "black"), x = 0.5, y = 0.95),
        margin = list(t = 90),
        legend = list(y = 0.8, yanchor = "middle", font  = list(size = 16),
                      title = list(font = list(size = 16, color = "black"), text = "<b>Variable</b>")),
        xaxis  = list(title    = list(text = "<b>Percentile</b>", font = list(size = 20)), 
                      tickfont = list(size = 18)),
        yaxis  = list(title    = list(text = "<b>Value</b>", font = list(size = 20)),
                      tickfont = list(size = 18))
      )
  })
  
  
  # ── SERVER TABPLOT ─────────────────────────────────────────────────────
  
  # 1st block: variable selector and preset dropdown initialisation
  observe({
    df       <- display_data()
    all_vars <- names(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    updateSelectizeInput(session, "tp_vars", choices = all_vars)
    apply_preset_selection(session, "tabplot", "tp_vars", all_vars)
    
    # preset dropdown — only presets valid for this tab
    valid_groups <- Filter(function(v) any(v %in% all_vars), presets_for("tabplot"))
    choices      <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "tp_preset", choices = choices)
    
    # sort-by selector — numeric vars + any Date cols, default to Y if present
    date_vars    <- names(df)[sapply(df, inherits, what = "Date")]
    sortable_vars <- c(date_vars, num_vars)
    sort_default  <- if (length(date_vars) > 0) date_vars[1] else sortable_vars[1]
    updateSelectInput(session, "tp_sortvar", choices = sortable_vars, selected = sort_default)
  })
  
  # 2nd block: preset observer
  observeEvent(input$tp_preset, {
    req(input$tp_preset != "none")
    df       <- display_data()
    all_vars <- names(df)
    sel      <- intersect(VAR_PRESETS[[input$tp_preset]], all_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "tp_vars", selected = sel)
  })
  
  # 3rd block: plot output
  output$tp_output <- renderPlot({
    req(input$tp_vars)
    df <- display_data()
    
    sel_vars <- intersect(input$tp_vars, names(df))
    validate(need(length(sel_vars) >= 1, "Select at least 1 variable."))
    
    # apply transform to numeric columns in the plot subset
    df_plot <- df[, sel_vars, drop = FALSE]
    if (input$tp_transform != "none") {
      num_cols <- names(df_plot)[sapply(df_plot, is.numeric)]
      df_plot[num_cols] <- lapply(df_plot[num_cols], function(y) {
        switch(input$tp_transform,
               "center"      = y - mean(y, na.rm = TRUE),
               "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
               "normalise"   = (y - min(y, na.rm = TRUE)) /
                 (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
        )
      })
    }
    
    # sorting logic
    #   sort_on = FALSE → inject a natural row-index col and sort by it (preserves original order visually)
    #   sort_on = TRUE  → sort by the chosen numeric variable
    using_index <- !isTRUE(input$tp_sort_on)
    
    if (using_index) {
      df_plot$.row_index <- seq_len(nrow(df_plot))
      sort_col           <- ".row_index"
      df_plot <- df_plot[, c(".row_index", setdiff(names(df_plot), ".row_index")), drop = FALSE]
    } else {
      req(input$tp_sortvar)
      validate(need(input$tp_sortvar %in% names(df), "Sort variable not found in dataset."))
      sort_col <- input$tp_sortvar
      
      if (!sort_col %in% names(df_plot))
        df_plot[[sort_col]] <- df[[sort_col]]
      
      # if sorting by a Date col, coerce it to numeric for tableplot's sort key
      # (the display version was already converted to factor above — this only
      #  affects the sort col copy that tableplot reads internally)
      if (inherits(df_plot[[sort_col]], "Date"))
        df_plot[[sort_col]] <- as.numeric(df_plot[[sort_col]])
      
      df_plot <- df_plot[, c(sort_col, setdiff(names(df_plot), sort_col)), drop = FALSE]
    }
    
    sort_label <- if (using_index) "Original Row Order" else input$tp_sortvar
    transform_label <- if (input$tp_transform != "none")
      paste0(" | ", tools::toTitleCase(input$tp_transform)) else ""
    
    default_title <- paste0("Tabplot | Sorted by ", sort_label, transform_label)
    plot_title    <- if (nzchar(input$tp_title)) input$tp_title else default_title
    
    tabplot::tableplot(
      df_plot,
      sortCol    = sort_col,
      decreasing = if (using_index) FALSE else isTRUE(input$tp_decreasing),
      nBins      = input$tp_nbin,
      title      = plot_title,
      fontsize   = 14,
      fontsize.title  = 22
    )
  })
  
  
  # ── SERVER BOXPLOT 1 ───────────────────────────────────────────────────
  
  # 1st block: variable selector, preset dropdown, group var initialisation
  observe({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    updateSelectizeInput(session, "bx_vars", choices = num_vars)
    apply_preset_selection(session, "boxplot", "bx_vars", num_vars)
    
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("boxplot"))
    choices      <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "bx_preset", choices = choices)
    
    apply_groupby_defaults(session, "boxplot", cat_vars, "bx_group_var")
  })
  
  # 2nd block: preset observer
  observeEvent(input$bx_preset, {
    req(input$bx_preset != "none")
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    sel      <- intersect(VAR_PRESETS[[input$bx_preset]], num_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "bx_vars", selected = sel)
  })
  
  # 3rd block: group level selector
  observeEvent(input$bx_group_var, {
    req(input$bx_group_var)
    df   <- display_data()
    lvls <- sort(unique(na.omit(as.character(df[[input$bx_group_var]]))))
    updateSelectizeInput(session, "bx_group_levels", choices = lvls, selected = lvls)
  })
  
  # 4th block: plot output
  output$bx_output <- renderPlot({
    req(input$bx_vars)
    df       <- display_data()
    sel_vars <- intersect(input$bx_vars, names(df))
    validate(need(length(sel_vars) >= 1, "Select at least 1 numeric variable."))
    
    # apply transform to a local copy only
    df_plot <- df[, sel_vars, drop = FALSE]
    if (input$bx_transform != "none") {
      df_plot[] <- lapply(df_plot, function(y) {
        switch(input$bx_transform,
               "center"      = y - mean(y, na.rm = TRUE),
               "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
               "normalise"   = (y - min(y, na.rm = TRUE)) /
                 (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
        )
      })
    }
    
    # resolve grouping
    using_group <- isTRUE(input$bx_group_on) &&
      !is.null(input$bx_group_var) &&
      input$bx_group_var %in% names(df)
    
    if (using_group) {
      grp  <- input$bx_group_var
      lvls <- if (!is.null(input$bx_group_levels) && length(input$bx_group_levels) > 0)
        input$bx_group_levels else sort(unique(na.omit(as.character(df[[grp]]))))
      df_plot[[grp]] <- df[[grp]]
      df_plot <- df_plot[as.character(df_plot[[grp]]) %in% lvls, , drop = FALSE]
      df_plot[[grp]] <- droplevels(as.factor(df_plot[[grp]]))
    }
    
    # build auto title
    transform_label <- if (input$bx_transform != "none")
      paste0(" | ", tools::toTitleCase(input$bx_transform)) else ""
    group_label   <- if (using_group) paste0(" | Grouped by ", input$bx_group_var) else ""
    default_title <- paste0("Boxplot | IQR ×", input$bx_coef, transform_label, group_label)
    plot_title    <- if (nzchar(input$bx_title)) input$bx_title else default_title
    
    colours <- theme_colours_for(sel_vars)
    
    # pivot to long format
    df_long <- tidyr::pivot_longer(df_plot,
                                   cols      = all_of(sel_vars),
                                   names_to  = "Variable",
                                   values_to = "Value")
    df_long$Variable <- factor(df_long$Variable, levels = sel_vars)
    
    n_vars    <- length(sel_vars)
    angle_val <- if (n_vars > 8) 45 else 0
    hjust_val <- if (n_vars > 8) 1  else 0.5
    
    p <- if (using_group) {
      ggplot2::ggplot(df_long,
                      aes(x = Variable, y = Value, fill = Variable)) +
        ggplot2::geom_boxplot(
          coef          = input$bx_coef,
          outlier.shape = 21,
          outlier.size  = 1.5,
          outlier.alpha = 0.5,
          alpha         = 0.75,
          colour        = "grey25"
        ) +
        ggplot2::facet_wrap(as.formula(paste("~", grp)), scales = "free_y") +
        ggplot2::scale_fill_manual(values = colours, guide = "none")
    } else {
      ggplot2::ggplot(df_long,
                      aes(x = Variable, y = Value, fill = Variable)) +
        ggplot2::geom_boxplot(
          coef          = input$bx_coef,
          outlier.shape = 21,
          outlier.size  = 1.5,
          outlier.alpha = 0.5,
          alpha         = 0.75,
          colour        = "grey25"
        ) +
        ggplot2::scale_fill_manual(values = colours, guide = "none")
    }
    
    print(
      p +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          plot.title         = element_text(size = 24, face = "bold", hjust = 0.5),
          strip.text         = element_text(size = 18, face = "bold"),
          panel.grid.major.x = element_blank(),
          axis.text.x        = element_text(angle = angle_val, hjust = hjust_val,
                                            size  = if (n_vars > 20) 14 else 18),
          axis.text.y        = element_text(size = 16),
          plot.margin        = margin(t = 15)
        ) +
        ggplot2::labs(title = plot_title, x = NULL, y = "Value") +
        ggplot2::theme(
          axis.title.y = element_text(size = 16, face = "bold")
        )
    )
  })
  
  
  # ── SERVER BOXPLOT 2 ───────────────────────────────────────────────────
  
  # 1st block: variable selector, preset dropdown, group var initialisation
  observe({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    updateSelectizeInput(session, "box_vars", choices = num_vars)
    apply_preset_selection(session, "boxplot", "box_vars", num_vars)
    
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("boxplot"))
    choices      <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "box_preset", choices = choices)
    
    apply_groupby_defaults(session, "boxplot", cat_vars, "box_group_var")
  })
  
  # 2nd block: preset observer
  observeEvent(input$box_preset, {
    req(input$box_preset != "none")
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    sel      <- intersect(VAR_PRESETS[[input$box_preset]], num_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "box_vars", selected = sel)
  })
  
  # 3rd block: group level selector
  observeEvent(input$box_group_var, {
    req(input$box_group_var)
    df   <- display_data()
    lvls <- sort(unique(na.omit(as.character(df[[input$box_group_var]]))))
    updateSelectizeInput(session, "box_group_levels", choices = lvls, selected = lvls)
  })
  
  # 4th block: plot output
  output$box_plot <- renderPlotly({
    req(input$box_vars)
    df       <- display_data()
    sel_vars <- intersect(input$box_vars, names(df))
    validate(need(length(sel_vars) >= 1, "Select at least 1 numeric variable."))
    
    # apply transform to a local copy only
    df_plot <- df[, sel_vars, drop = FALSE]
    if (input$box_transform != "none") {
      df_plot[] <- lapply(df_plot, function(y) {
        switch(input$box_transform,
               "center"      = y - mean(y, na.rm = TRUE),
               "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
               "normalise"   = (y - min(y, na.rm = TRUE)) /
                 (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
        )
      })
    }
    
    # resolve grouping
    using_group <- isTRUE(input$box_group_on) &&
      !is.null(input$box_group_var) &&
      input$box_group_var %in% names(df)
    
    if (using_group) {
      grp  <- input$box_group_var
      lvls <- if (!is.null(input$box_group_levels) && length(input$box_group_levels) > 0)
        input$box_group_levels else sort(unique(na.omit(as.character(df[[grp]]))))
      df_plot[[grp]] <- df[[grp]]
      df_plot <- df_plot[as.character(df_plot[[grp]]) %in% lvls, , drop = FALSE]
      df_plot[[grp]] <- droplevels(as.factor(df_plot[[grp]]))
    }
    
    # pivot to long format
    df_long <- tidyr::pivot_longer(df_plot,
                                   cols      = all_of(sel_vars),
                                   names_to  = "Variable",
                                   values_to = "Value")
    df_long$Variable <- factor(df_long$Variable, levels = sel_vars)
    
    # build auto title
    mode_label      <- if (isTRUE(input$box_violin)) "Violin" else "Boxplot"
    transform_label <- if (input$box_transform != "none")
      paste0(" | ", tools::toTitleCase(input$box_transform)) else ""
    group_label     <- if (using_group) paste0(" | Grouped by ", input$box_group_var) else ""
    # iqr_label       <- if (!isTRUE(input$box_violin)) paste0(" | IQR ×", input$box_iqr) else ""
    default_title   <- paste0(mode_label, transform_label, group_label) # iqr_label, 
    plot_title      <- if (nzchar(input$box_title)) input$box_title else default_title
    
    # x aesthetic: Variable alone, or Variable × group level
    if (using_group) {
      p <- ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
        facet_wrap(as.formula(paste("~", grp)), scales = "free_y")
    } else {
      p <- ggplot(df_long, aes(x = Variable, y = Value, fill = Variable))
    }
    
    if (isTRUE(input$box_violin)) {
      p <- p +
        ggplot2::geom_violin(
          alpha    = 0.7,
          trim     = FALSE
        )
    } else {
      p <- p +
        geom_boxplot(
          coef = 1.5,  # input$box_iqr
          alpha = 0.7,
          outlier.size = 0.8
        )
    }
    
    p <- p +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y     = element_text(size = 13),
        legend.position = "none",
        axis.title.y    = element_blank(),
        strip.text      = element_text(size = 16, face = "bold")
      ) +
      ggplot2::labs(title = plot_title, x = NULL, y = NULL)
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        title  = list(
          text = paste0("<b>", plot_title, "</b>"),
          font = list(size = 22, color = "black"),
          x = 0.5, y = 0.97
        ),
        margin = list(t = 78),
        legend = list(
          font  = list(size = 16),
          title = list(
            text = if (using_group) paste0("<b>", input$box_group_var, "</b>") else "<b>Variable</b>",
            font = list(size = 16)
          )
        ),
        xaxis  = list(title = list(text = "", font = list(size = 16)), tickfont = list(size = 16)),
        xaxis2 = list(title = list(text = "", font = list(size = 16)), tickfont = list(size = 16)),
        yaxis  = list(title = list(text = "<b>Value</b>", font = list(size = 16)), tickfont = list(size = 16)),
        yaxis2 = list(tickfont = list(size = 16))
      )
  })
  
  
  # ── SERVER MOSAIC ──────────────────────────────────────────────────────
  
  # 1st block: dynamic variable selectors (categorical only)
  cat_cols <- reactive({
    df   <- display_data()
    cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    max_lvls <- input$mosaic_max_levels
    req(max_lvls)
    # keep only cols whose cardinality is within the limit
    cols[sapply(cols, function(v) length(unique(na.omit(df[[v]]))) <= max_lvls)]
  })
  
  output$mosaic_x_ui <- renderUI({
    cols <- cat_cols()
    selectInput("mosaic_x", "Variable 1 (columns):", choices = cols, selected = cols[1])
  })
  
  output$mosaic_y_ui <- renderUI({
    cols <- cat_cols()
    selectInput("mosaic_y", "Variable 2 (rows):", choices = cols,
                selected = if (length(cols) >= 2) cols[2] else cols[1])
  })
  
  output$mosaic_z_ui <- renderUI({
    cols <- cat_cols()
    selectInput("mosaic_z", "Variable 3 — optional (sub-rows):",
                choices = c("None", cols), selected = "None")
  })
  
  # 2nd block: mosaic plot output
  output$mosaic_plot <- renderPlot({
    req(input$mosaic_x, input$mosaic_y, input$mosaic_z)
    df <- display_data()
    
    # build contingency table
    if (input$mosaic_z == "None") {
      tbl <- table(df[[input$mosaic_x]], df[[input$mosaic_y]])
      names(dimnames(tbl)) <- c(input$mosaic_x, input$mosaic_y)
    } else {
      tbl <- table(df[[input$mosaic_x]], df[[input$mosaic_y]], df[[input$mosaic_z]])
      names(dimnames(tbl)) <- c(input$mosaic_x, input$mosaic_y, input$mosaic_z)
    }
    
    vars_used     <- c(input$mosaic_x, input$mosaic_y,
                       if (input$mosaic_z != "None") input$mosaic_z)
    default_title <- paste0("Mosaic | ", paste(vars_used, collapse = " × "))
    plot_title    <- if (nzchar(input$mosaic_title)) input$mosaic_title else default_title
    
    vcd::mosaic(tbl,
                shade    = input$mosaic_shade,
                legend   = input$mosaic_shade,
                main     = plot_title,
                labeling = vcd::labeling_border(
                  rot_labels = c(input$mosaic_rot_labels, 0, 0, 0),
                  gp_labels  = grid::gpar(fontsize = input$mosaic_fontsize),
                  abbreviate = input$mosaic_abbreviate
                ))
  })
  
  # 3rd block: pair advisor — compute Cramér's V for all variable combos
  get_cramer <- function(df, v1, v2) {
    tbl   <- table(df[[v1]], df[[v2]])
    stats <- tryCatch(vcd::assocstats(tbl), error = function(e) NULL)
    if (is.null(stats)) return(NA)
    stats$cramer
  }
  
  pairs_result <- eventReactive(input$pairs_search, {
    df   <- display_data()
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    if (input$pairs_way == "2-way") {
      
      combos  <- combn(cats, 2, simplify = FALSE)
      results <- lapply(combos, function(pair) {
        tbl   <- table(df[[pair[1]]], df[[pair[2]]])
        stats <- tryCatch(vcd::assocstats(tbl), error = function(e) NULL)
        if (is.null(stats)) return(NULL)
        data.frame(
          Var1    = pair[1],
          Var2    = pair[2],
          Var3    = NA_character_,
          CramerV = round(stats$cramer, 4),
          Chi_sq  = round(stats$chisq_tests["Pearson", "X^2"], 2),
          p_value = round(stats$chisq_tests["Pearson", "P(> X^2)"], 6)
        )
      })
      
    } else {
      
      combos  <- combn(cats, 3, simplify = FALSE)
      results <- lapply(combos, function(trio) {
        v12 <- get_cramer(df, trio[1], trio[2])
        v13 <- get_cramer(df, trio[1], trio[3])
        v23 <- get_cramer(df, trio[2], trio[3])
        data.frame(
          Var1    = trio[1],
          Var2    = trio[2],
          Var3    = trio[3],
          CramerV = round(mean(c(v12, v13, v23), na.rm = TRUE), 4),
          V_1_2   = round(v12, 4),
          V_1_3   = round(v13, 4),
          V_2_3   = round(v23, 4),
          Chi_sq  = NA,
          p_value = NA
        )
      })
    }
    
    out <- do.call(rbind, Filter(Negate(is.null), results))
    out <- out[order(-out$CramerV), ]
    out$Strength <- ifelse(out$CramerV >= 0.5, "Strong",
                           ifelse(out$CramerV >= 0.3, "Moderate",
                                  ifelse(out$CramerV >= 0.1, "Weak", "Negligible")))
    head(out, input$pairs_top)
  })
  
  # 4th block: pair advisor table output
  output$pairs_table <- renderDT({
    datatable(pairs_result(),
              rownames  = FALSE,
              selection = "single",
              options   = list(pageLength = 15, dom = "tip")) %>%
      formatStyle("CramerV",
                  background         = styleColorBar(c(0, 1), "#a8d5a2"),
                  backgroundSize     = "100% 80%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Strength",
                  color = styleEqual(
                    c("Strong", "Moderate", "Weak", "Negligible"),
                    c("#155724", "#856404", "#856404", "#6c757d")
                  ),
                  fontWeight = "bold")
  })
  
  # 5th block: clicking a row loads variables into mosaic dropdowns
  observeEvent(input$pairs_table_rows_selected, {
    res <- pairs_result()
    sel <- input$pairs_table_rows_selected
    updateSelectInput(session, "mosaic_x", selected = res$Var1[sel])
    updateSelectInput(session, "mosaic_y", selected = res$Var2[sel])
    updateSelectInput(session, "mosaic_z",
                      selected = if (!is.na(res$Var3[sel])) res$Var3[sel] else "None")
  })
  
  
  # ── SERVER GGPAIRS ─────────────────────────────────────────────────────
  
  # 1st block: variable selector (initialise selectors when data changes)
  observe({
    # detect variable types
    df       <- display_data()
    all_vars <- names(df)
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    num_vars <- names(df)[sapply(df, is.numeric)]
    # updates the variable selection widget in the UI
    updateSelectizeInput(session, "gg_vars", choices = all_vars)
    # selection driven by preset (from custom helper function)
    apply_preset_selection(session, "ggpairs", "gg_vars", all_vars)
    # applly group-by defaults (from custom helper function)
    apply_groupby_defaults(session, "ggpairs", cat_vars, "gg_group_var")
    # build the preset dropdown, construct dropdown choices, then update preset dropdown
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("ggpairs"))
    choices      <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "gg_preset", choices = choices)
  })
  
  # 2nd block: apply preset selection, triggered when user chooses a preset
  observeEvent(input$gg_preset, {
    req(input$gg_preset != "none")
    df       <- display_data()
    all_vars <- names(df)
    sel      <- intersect(VAR_PRESETS[[input$gg_preset]], all_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "gg_vars", selected = sel)
  })
  
  # 3rd block: groupby level selector
  observeEvent(input$gg_group_var, {
    req(input$gg_group_var)
    df   <- display_data()
    lvls <- sort(unique(as.character(df[[input$gg_group_var]])))
    lvls <- lvls[!is.na(lvls)]
    updateSelectizeInput(session, "gg_group_levels",
                         choices  = lvls,
                         selected = resolve_group_levels("ggpairs", lvls))
  })
  
  # 4th block: output plot
  output$gg_output <- renderPlot({
    input$gg_run
    isolate({
      req(input$gg_vars)
      df <- display_data()
      validate(need(length(input$gg_vars) >= 2, "Please select at least 2 variables."))
      
      default_title <- paste0(
        "GGPairs",
        if (input$gg_group_on && !is.null(input$gg_group_var)) paste0(" | Grouped by ", input$gg_group_var) else ""
      )
      plot_title <- if (nzchar(input$gg_title)) input$gg_title else default_title
      
      if (input$gg_group_on && !is.null(input$gg_group_var)) {
        
        grp <- input$gg_group_var
        
        # build df with selected vars + group col, drop NAs in group
        plot_df <- df[, c(input$gg_vars, grp), drop = FALSE]
        plot_df <- plot_df[!is.na(plot_df[[grp]]), ]
        
        # filter to selected levels only
        if (!is.null(input$gg_group_levels) && length(input$gg_group_levels) > 0) {
          plot_df <- plot_df[as.character(plot_df[[grp]]) %in% input$gg_group_levels, ]
          plot_df[[grp]] <- droplevels(as.factor(plot_df[[grp]]))  # drop unused levels from legend
        }
        
        # columns to plot = only the selected vars
        col_idx <- seq_along(input$gg_vars)
        
        GGally::ggpairs(
          plot_df,
          progress = FALSE,
          columns  = col_idx,
          mapping  = aes(colour = .data[[grp]], alpha = 0.6),
          # displaying logic above the diagonal
          upper    = list(
            continuous = GGally::wrap("cor", size = 4),                      # corr
            combo      = GGally::wrap("box_no_facet", alpha = 0.5),          # box plot
            discrete   = GGally::wrap("facetbar", alpha = 0.5)               # bar chart
          ),
          # logic below the diagonal
          lower    = list(
            continuous = GGally::wrap("points", alpha = 0.3, size = 0.8),    # scatter plot
            combo      = GGally::wrap("facethist", bins = 20, alpha = 0.5),  # histogram
            discrete   = GGally::wrap("facetbar", alpha = 0.5)               # bar chart
          ),
          # the diagonal (each variable against itself)
          diag     = list(
            continuous = GGally::wrap("densityDiag", alpha = 0.5),           # density curve
            discrete   = GGally::wrap("barDiag", alpha = 0.5)                # bar chart
          ),
          # takes the legend from panel 1 and displays it for the whole plot
          legend   = 1
        ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title  = element_text(size = 20, face = "bold", hjust = 0.5),
            strip.text  = element_text(size = 14, face = "bold"),
            axis.text   = element_text(size = 14),
            legend.title  = element_text(face = "bold"),
            legend.text   = element_text(size = 12),
            plot.margin = margin(t = 20)
          ) +
          ggplot2::labs(title = plot_title)
        
      } else {
        
        plot_df <- df[, input$gg_vars, drop = FALSE]
        
        GGally::ggpairs(
          plot_df,
          progress = FALSE,
          upper = list(continuous = GGally::wrap("cor", size = 4)),
          lower = list(continuous = GGally::wrap("points", alpha = 0.4, size = 0.8)),
          diag  = list(continuous = GGally::wrap("densityDiag"))
        ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title  = element_text(size = 20, face = "bold", hjust = 0.5),
            strip.text  = element_text(size = 14, face = "bold"),
            axis.text   = element_text(size = 14),
            plot.margin = margin(t = 20)
          ) +
          ggplot2::labs(title = plot_title)
      }
    })
  })
  
  
  # ── SERVER HEATMAP ─────────────────────────────────────────────────────
  
  # 1st block: variable selector and preset dropdown initialisation
  observe({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "hm_vars", choices = num_vars)
    apply_preset_selection(session, "heatmap", "hm_vars", num_vars)
    # dropdown only shows presets valid for this tab
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("heatmap"))
    choices      <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "hm_preset", choices = choices)
  })
  
  # 2nd block: preset observer, apply selected preset to variable selector
  observeEvent(input$hm_preset, {
    req(input$hm_preset != "none")
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    sel      <- intersect(VAR_PRESETS[[input$hm_preset]], num_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "hm_vars", selected = sel)
  })
  
  # 3rd block: corrgram plot output
  output$hm_output <- renderPlot({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    selected <- if (!is.null(input$hm_vars) && length(input$hm_vars) >= 2)
      input$hm_vars else num_vars
    selected <- intersect(selected, num_vars)
    validate(need(length(selected) >= 2, "Select at least 2 numeric variables."))
    
    df_num <- df[, selected, drop = FALSE]
    
    # compute correlation matrix first (values/NA), then apply abs manually if checked
    if (isTRUE(input$hm_missing)) {
      na_mat  <- is.na(df_num) * 1L                  # 1 = missing, 0 = observed
      keep    <- apply(na_mat, 2, var) > 0            # drop zero-variance cols (never/always missing)
      na_mat  <- na_mat[, keep, drop = FALSE]
      validate(need(ncol(na_mat) >= 2, "Not enough variables with missingness to correlate."))
      cor_mat <- cor(na_mat, use = "pairwise.complete.obs", method = input$hm_cor)
    } else {
      cor_mat <- cor(df_num, use = "pairwise.complete.obs", method = input$hm_cor)
    }
    if (isTRUE(input$hm_abs)) cor_mat <- abs(cor_mat)
    
    # build order label
    order_label <- switch(input$hm_order,
                          "FALSE" = "Original Order",
                          "TRUE"  = "AOE (Eigenvector)",
                          "HC"    = "HC (Hierarchical)",
                          "OLO"   = "OLO (Optimal Leaf)"
    )
    
    default_title <- paste0(
      if (isTRUE(input$hm_missing)) "Missingness Correlation Heatmap" else "Correlation Heatmap",
      " | ", tools::toTitleCase(input$hm_cor),
      " | ", order_label,
      if (isTRUE(input$hm_abs)) " | Absolute" else ""
    )
    
    # outer top margin, pushes title up, 3 lines of space
    par(oma = c(0, 0, 3, 0))
    
    corrgram::corrgram(
      cor_mat,
      order      = if (input$hm_order == "FALSE") FALSE else input$hm_order,
      abs        = FALSE, # already handled above
      cor.method = input$hm_cor,
      cex.var    = 1.2
    )
    
    # overlay larger title (base graphics workaround — corrgram uses base plot)
    plot_title <- if (nzchar(input$hm_title)) input$hm_title else default_title
    title(main = plot_title, cex.main = 2, font.main = 2, outer = TRUE, line = 1)
  })
  
  
  # ── SERVER R CONSOLE ───────────────────────────────────────────────────
  
  # the entire UI is replaced with a lock screen when not authenticated
  
  # ·· CONDITIONAL UI (LOCK GATE) ·····································
  
  output$rconsole_body_ui <- renderUI({
    
    if (!is_unlocked()) {
      # ~~ locked state: show a friendly gate ~~
      div(style = "text-align:center; margin-top:120px; color:#6c757d;",
          icon("lock", style = "font-size:48px;"),
          h4("R Console is locked for developer only."),
          p("Enter the passphrase above to unlock.")
      )
      
    } else {
      # ~~ unlocked state: render full console ~~
      sidebarLayout(
        sidebarPanel(width = 3,
                     sidebar_note("Note: 
                                  <br><br>R console occupied when running shiny app, 
                                  thus build the console in app as a debugging helper. 
                                  <br><br>Run R expressions against the current dataset. 
                                  <br><br>The dataset is available as df."),
                     hr(),
                     actionButton("rconsole_run",   "Run",   icon = icon("play"),  width = "100%"),
                     br(), br(),
                     actionButton("rconsole_clear", "Clear", icon = icon("trash"), width = "100%")
        ),
        mainPanel(width = 9,
                  # code editor (ACE)
                  shinyAce::aceEditor(
                    "rconsole_input",
                    mode     = "r",
                    theme    = "tomorrow",
                    height   = "400px",
                    value    = "# type R code here\nhead(df)",
                    fontSize = 14
                  ),
                  hr(),
                  verbatimTextOutput("rconsole_output")
        )
      )
    }
  })
  
  
  # ·· CONTROLLED CODE EXECUTION ······································
  
  output$rconsole_output <- renderPrint({
    input$rconsole_run
    # isolate here means code only runs on button click
    isolate({
      code <- input$rconsole_input
      if (is.null(code) || trimws(code) == "") {
        cat("# Type R code above and click Run\n")
        return(invisible(NULL))
      }
      
      # normalise line endings and common unicode punctuation
      code <- gsub("\r\n|\r", "\n", code)
      code <- gsub("\u2018|\u2019", "'",  code)
      code <- gsub("\u201c|\u201d", '"',  code)
      code <- gsub("\u2013|\u2014", "-",  code)
      code <- iconv(code, from = "UTF-8", to = "ASCII", sub = "")
      
      # inject current dataset into the console environment
      assign("df", display_data(), envir = rconsole_env)
      
      # parse once, eval expression-by-expression so every value prints
      exprs <- tryCatch(parse(text = code),
                        error = function(e) { cat("Parse error:", conditionMessage(e), "\n"); NULL })
      if (is.null(exprs)) return(invisible(NULL))
      
      for (expr in exprs) {
        tryCatch({
          res <- capture.output(
            eval(expr, envir = rconsole_env)
          )
          if (length(res) > 0) cat(paste(res, collapse = "\n"), "\n")
        },
        error = function(e) cat("Error:", conditionMessage(e), "\n"),
        warning = function(w) cat("Warning:", conditionMessage(w), "\n"))
      }
    })
  })
  
  # resets editor content, clean UX
  observeEvent(input$rconsole_clear, {
    shinyAce::updateAceEditor(session, "rconsole_input", value = "")
  })
  
  
} # end server