# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(tidyverse)
library(DT)

# ── DATA INITIALISATION ──────────────────────────────────────────────────────

raw_dataset <- read.csv('Ass2Data.csv', header = TRUE, na.strings = c('NA', 'N/A'), stringsAsFactors = TRUE)

# ── GENERAL HELPER ───────────────────────────────────────────────────────────

sidebar_note <- function(text) {
  div(
    style = "
      font-size: 13px;
      font-weight: 500;
      color: #343a40;
      background-color: white;
      padding: 10px;
      border-left: 4px solid #0d6efd;
      border-radius: 6px;
      margin-bottom: 12px;
      box-shadow: 0 1px 2px rgba(0,0,0,0.05);
    ",
    icon("info-circle", style = "color:#0d6efd;"),
    HTML(paste("&nbsp;", text))
  )
}

# ── PLOT FUNCTIONS ────────────────────────────────────────────────────────────

# plot vis_dat and vis_mis graphs
plot_missingness <- function(df, full_df, mode, group_on, group_var, group_levels) {
  
  make_plot <- function(df_sub, title = NULL) {
    p <- if (mode == "visdat") {
      visdat::vis_dat(df_sub)
    } else if (mode == "vismiss_orig") {
      visdat::vis_miss(df_sub, sort_miss = FALSE)
    } else {
      visdat::vis_miss(df_sub, sort_miss = TRUE)
    }
    if (!is.null(title)) p <- p + ggplot2::ggtitle(title)
    p
  }
  
  if (!group_on) {
    make_plot(df)
    
  } else {
    lvls <- if (length(group_levels) > 0) group_levels else levels(full_df[[group_var]])
    
    if (length(lvls) > 6) {
      plot.new()
      text(0.5, 0.5,
           paste0("Too many levels to display (", length(lvls), ").\nPlease select 6 or fewer levels."),
           cex = 1.2, col = "#C41E3A", adj = 0.5)
      return()
    }
    
    plots <- lapply(lvls, function(lvl) {
      df_sub <- df[full_df[[group_var]] == lvl, , drop = FALSE]
      make_plot(df_sub, paste(group_var, "=", lvl))
    })
    
    n     <- length(plots)
    ncols <- min(n, 2)
    nrows <- ceiling(n / ncols)
    gridExtra::grid.arrange(grobs = plots, ncol = ncols, nrow = nrows)
  }
}

# plot boxplot graph
plot_boxplot <- function(df, var, label_col, iqr_k) {
  n_missing <- sum(is.na(df[[var]]))
  if (n_missing > 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("Cannot draw boxplot: ", n_missing, " rows have NA in ", 
                                var, ".\nApply a missingness strategy first."),
                 colour = "#C41E3A", size = 5) +
        theme_void()
    )
  }
  x       <- df[[var]]
  limits  <- boxplot.stats(x = x, coef = iqr_k)$stats
  df$label <- ifelse(x < limits[1] | x > limits[5], as.character(df[[label_col]]), NA)
  
  ggplot(df, aes(x = .data[[var]], y = 1, label = label)) +
    geom_boxplot(coef = iqr_k, outlier.colour = "#C41E3A") +
    ggrepel::geom_text_repel(max.overlaps = 20, na.rm = TRUE) +
    labs(
      title = paste("Boxplot of", var, "at IQR multiplier k =", iqr_k),
      x     = var
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )
}

get_boxplot_outliers <- function(df, var, iqr_k) {
  if (any(is.na(df[[var]]))) return(df[0, ])
  x      <- df[[var]]
  limits <- boxplot.stats(x = x, coef = iqr_k)$stats
  df[x < limits[1] | x > limits[5], , drop = FALSE]
}


# plot bagplot graph
plot_bagplot <- function(df, x_var, y_var, label_col, bag_k) {
  
  n_missing <- sum(is.na(df[[x_var]]) | is.na(df[[y_var]]))
  if (n_missing > 0) {
    plot.new()
    text(0.5, 0.5,
         paste0("Cannot draw bagplot: ", n_missing, " rows have NA in ", 
                x_var, " or ", y_var, ".\nApply a missingness strategy first."),
         cex = 1.2, col = "#C41E3A", adj = 0.5)
    return()
  }
  
  # draw the bags natively
  aplpack::bagplot(x = df[[x_var]], y = df[[y_var]],
                   factor = bag_k, show.bagpoints = FALSE,
                   main  = paste("Bagplot of", y_var, "vs", x_var, "at factor k =", bag_k),
                   xlab  = x_var, ylab = y_var)
  
  # extract outliers silently
  bag <- aplpack::bagplot(x = df[[x_var]], y = df[[y_var]],
                          factor = bag_k, show.bagpoints = FALSE,
                          create.plot = FALSE)
  
  if (!is.null(bag$pxy.outlier) && nrow(bag$pxy.outlier) > 0) {
    values       <- as.data.frame(bag$pxy.outlier)
    outlier_mask <- df[[x_var]] %in% values$x & df[[y_var]] %in% values$y
    outlier_df   <- df[outlier_mask, ]
    text(
      x      = outlier_df[[x_var]],
      y      = outlier_df[[y_var]],
      labels = as.character(outlier_df[[label_col]]),
      pos    = 3,
      cex    = 0.7,
      col    = "#C41E3A"
    )
  }
}

get_bagplot_outliers <- function(df, x_var, y_var, bag_k) {
  bag    <- aplpack::bagplot(x = df[[x_var]], y = df[[y_var]],
                             factor = bag_k, show.bagpoints = FALSE,
                             create.plot = FALSE)
  values <- as.data.frame(bag$pxy.outlier)
  df[df[[x_var]] %in% values$x & df[[y_var]] %in% values$y, , drop = FALSE]
}


# ── STRATEGY FUNCTIONS ────────────────────────────────────────────────────────

# for false negative NAs
apply_missingness_strategy <- function(df, raw_vals) {
  if (is.null(raw_vals) || trimws(raw_vals) == "") return(df)
  
  na_vals <- trimws(strsplit(raw_vals, ",")[[1]])
  na_nums <- suppressWarnings(as.numeric(na_vals))
  na_nums <- na_nums[!is.na(na_nums)]
  na_strs <- na_vals
  
  if (length(na_nums) > 0)
    df <- df |> mutate(across(where(is.numeric), ~ ifelse(.x %in% na_nums, NA_real_, .x)))
  
  df <- df |> mutate(across(where(is.factor), ~ {
    x <- as.character(.x)
    x[x %in% na_strs] <- NA
    as.factor(x)
  }))
  
  df |> mutate(across(where(is.character), ~ ifelse(.x %in% na_strs, NA_character_, .x)))
}

# for false positive NAs
apply_not_applicable_strategy <- function(df, rules) {
  for (rule in rules) {
    if (is.null(rule$target_col) || rule$target_col == "(none)") next
    if (is.null(rule$cond_col)   || rule$cond_col   == "(none)") next
    if (is.null(rule$cond_val)   || rule$cond_val   == "")       next
    
    mask <- !is.na(df[[rule$cond_col]]) &
      as.character(df[[rule$cond_col]]) == rule$cond_val &
      is.na(df[[rule$target_col]])
    
    impute_val <- switch(rule$impute,
                         "not_applicable" = "Not Applicable",
                         "zero"           = 0,
                         "mean"           = mean(df[[rule$target_col]], na.rm = TRUE),
                         "median"         = median(df[[rule$target_col]], na.rm = TRUE)
    )
    
    if (rule$impute == "not_applicable") {
      df[[rule$target_col]] <- as.character(df[[rule$target_col]])
      df[[rule$target_col]][mask] <- impute_val
      df[[rule$target_col]] <- as.factor(df[[rule$target_col]])
    } else {
      df[[rule$target_col]][mask] <- impute_val
    }
  }
  df
}

# for complete deletion

apply_threshold_strategy <- function(df, target, type, thresh) {
  if (is.null(thresh) || is.na(thresh)) return(df)
  
  miss_rate <- function(x) sum(is.na(x)) / length(x)
  miss_count <- function(x) sum(is.na(x))
  
  if (target == "cols") {
    keep <- if (type == "prop") {
      sapply(df, miss_rate)  <= thresh
    } else {
      sapply(df, miss_count) <= thresh
    }
    df[, keep, drop = FALSE]
    
  } else {
    keep <- if (type == "prop") {
      apply(df, 1, miss_rate)  <= thresh
    } else {
      apply(df, 1, miss_count) <= thresh
    }
    df[keep, , drop = FALSE]
  }
}


# for mmm imputation
apply_mmm_imputation <- function(df, mean_cols, median_cols, mode_cols) {
  
  stat_mode <- function(x) {
    tbl <- table(x, useNA = "no")
    if (length(tbl) == 0) return(NA)
    names(tbl)[which.max(tbl)]
  }
  
  for (col in mean_cols) {
    if (!col %in% names(df)) next
    df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
  }
  
  for (col in median_cols) {
    if (!col %in% names(df)) next
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
  
  for (col in mode_cols) {
    if (!col %in% names(df)) next
    df[[col]][is.na(df[[col]])] <- stat_mode(df[[col]])
  }
  
  df
}





# =================================================================================
# ui.R
# =================================================================================

ui <- fluidPage(
  
  # ·· HEADER ·································································
  
  fluidRow(
    column(12,
           div(
             style = "margin-top:20px; text-align:center; font-size:24px; font-weight:600; color:#495057;",
             "DATA423-26S1 Assignment 2 (EDA, Strategy, Model) \u2003 | \u2003 William Hui Chang (69051925)"
           )
    )
  ),
  
  hr(),
  
  
  # ·· TABS ···································································
  
  tabsetPanel(
    
    # ══ EDA ══════════════════════════════════════════════════════════════════
    
    tabPanel("EDA",
             tabsetPanel(
               
               # ── UI DATATABLE ──────────────────────────────────────────────────────
               
               tabPanel("Data Table",
                        DT::dataTableOutput("data_table")
               ), # end tab panel
               
               
               # ── UI SUMMARY ────────────────────────────────────────────────────────
               
               tabPanel("Summary",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Data Summary: <br><br>
                Select a style to inspect the dataset structure."),
                                       hr(),
                                       radioButtons("summary_style", "Style:",
                                                    choices = c("base R"    = "base",
                                                                "glimpse"   = "glimpse",
                                                                "dfSummary" = "dfsummary"),
                                                    selected = "glimpse")
                          ),
                          mainPanel(width = 9,
                                    uiOutput("summary_output")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI MISSINGNESS ────────────────────────────────────────────────────
               
               tabPanel("Missingness",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Missingness: <br><br>
                Visualise missing data patterns across the dataset.
                Group by a categorical variable to reveal whether
                missingness differs across subgroups."),
                                       hr(),
                                       selectizeInput("ms_vars", "Variables to plot:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       radioButtons("ms_mode", "View:",
                                                    choices = c("vis_dat (type + missingness)" = "visdat",
                                                                "vis_miss (Original Order)"    = "vismiss_orig",
                                                                "vis_miss (Rate Order)"        = "vismiss_rate"),
                                                    selected = "visdat"),
                                       hr(),
                                       checkboxInput("ms_group_on", "Group by categorical variable", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.ms_group_on == true",
                                         selectInput("ms_group_var", "Grouping variable:", choices = NULL),
                                         selectizeInput("ms_group_levels", "Levels to include:",
                                                        choices  = NULL,
                                                        multiple = TRUE,
                                                        options  = list(placeholder = "All levels included by default"))
                                       ),
                                       hr(),
                                       checkboxInput("ms_mcar", "Test for MCAR (Little's test)", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.ms_mcar == true",
                                         sidebar_note("Little's MCAR test: <br><br>
                                         H0: data is Missing Completely At Random.
                                         missingness is NOT random — i.e. MAR or MNAR.")
                                       )
                          ),
                          mainPanel(width = 9,
                                    plotOutput("ms_output", height = "70vh"),
                                    conditionalPanel(
                                      condition = "input.ms_mcar == true",
                                      hr(),
                                      h4("Little's MCAR Test Result"),
                                      verbatimTextOutput("ms_mcar_output")
                                    )
                          )
                        )
               ), # end tab panel
               
               
               # ── UI BOXPLOT ────────────────────────────────────────────────────────────────
               
               tabPanel("Box Plot",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Box Plot: <br><br>
                                       Univariate outlier detection using IQR multiplier.
                                       Points beyond the whiskers are flagged as outliers."),
                                       hr(),
                                       selectInput("bp_var", "Variable:", choices = NULL),
                                       hr(),
                                       selectInput("bp_label_col", "Label outliers by:", choices = NULL),
                                       hr(),
                                       numericInput("bp_iqr_k", "IQR multiplier (k):",
                                                    value = 1.5, min = 0.1, step = 0.5)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("bp_output", height = "40vh"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("bp_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI BAGPLOT ────────────────────────────────────────────────────────────────
               
               tabPanel("Bag Plot",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Bag Plot: <br><br>
                                       Bivariate outlier detection. Points outside the fence
                                       are flagged as outliers."),
                                       hr(),
                                       selectInput("bag_x_var", "X variable:", choices = NULL),
                                       selectInput("bag_y_var", "Y variable:", choices = NULL),
                                       hr(),
                                       selectInput("bag_label_col", "Label outliers by:", choices = NULL),
                                       hr(),
                                       numericInput("bag_k", "Bag factor (k):",
                                                    value = 3, min = 1, step = 0.5)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("bag_output", height = "60vh"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("bag_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI COMING SOON ────────────────────────────────────────────────────
               
               tabPanel("Coming Soon",
                        p("Under development.")
               ) # end tab panel
               
             ) # end eda tabsetPanel
    ), # end eda
    
    
    # ══ STRATEGY ══════════════════════════════════════════════════════════════
    
    tabPanel("Strategy",
             tabsetPanel(
               
               # ── UI MISSINGNESS STRATEGY ───────────────────────────────────────────
               
               tabPanel("MISS: Variants",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: Variants: <br><br>
                Define additional values to treat as missing.
                Changes will be reflected in the EDA Missingness tab."),
                                       hr(),
                                       textInput("strat_na_vals",
                                                 label       = "Treat as NA (comma separated, e.g. -99, --):",
                                                 value       = "",
                                                 placeholder = "e.g. -99, --, N/A, ?"
                                       )
                          ),
                          mainPanel(width = 9,
                                    p("Strategy applied. Check EDA > Missingness to see the effect.")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("MISS: Not Applicable",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: Not Applicable: <br><br>
                                       Define rules to recode NA values as 'Not Applicable'
                                       when a condition on another column is met."),
                          ),
                          mainPanel(width = 9,
                                    h5("Rule 1"),
                                    fluidRow(
                                      column(3, selectInput("strat_na_target_1",  "Target column (has NAs):", choices = NULL)),
                                      column(3, selectInput("strat_na_cond_col_1", "When column:", choices = NULL)),
                                      column(3, selectInput("strat_na_cond_val_1", "Equals:", choices = NULL)),
                                      column(3, selectInput("strat_na_impute_1",   "Impute with:",
                                                            choices = c("Not Applicable" = "not_applicable",
                                                                        "0"              = "zero",
                                                                        "Mean"           = "mean",
                                                                        "Median"         = "median")))
                                    )
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("MISS: Threshold",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: Threshold: <br><br>
                                                    Remove columns or rows where missingness exceeds a threshold."),
                                       hr(),
                                       radioButtons("strat_thresh_target", "Apply to:",
                                                    choices  = c("Columns" = "cols", "Rows" = "rows"),
                                                    selected = "cols"
                                       ),
                                       hr(),
                                       radioButtons("strat_thresh_type", "Threshold type:",
                                                    choices  = c("Proportion (0-1)" = "prop", "Count" = "count"),
                                                    selected = "prop"
                                       ),
                                       hr(),
                                       numericInput("strat_thresh_val", "Threshold:",
                                                    value = 1, min = 0, step = 0.01
                                       )
                          ),
                          mainPanel(width = 9,
                                    p("Strategy applied. Check EDA > Data Table or Missingness to see the effect.")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("MISS: MMM Imputation",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: MMM Imputation: <br><br>
                                       Impute missing values by column using
                                       mean, median, or mode.")
                          ),
                          mainPanel(width = 9,
                                    h5("Mean Imputation"),
                                    selectizeInput("strat_mmm_mean_cols", "Columns to impute with mean:",
                                                   choices  = NULL,
                                                   multiple = TRUE
                                    ),
                                    hr(),
                                    h5("Median Imputation"),
                                    selectizeInput("strat_mmm_median_cols", "Columns to impute with median:",
                                                   choices  = NULL,
                                                   multiple = TRUE
                                    ),
                                    hr(),
                                    h5("Mode Imputation"),
                                    selectizeInput("strat_mmm_mode_cols", "Columns to impute with mode:",
                                                   choices  = NULL,
                                                   multiple = TRUE
                                    )
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("Coming Soon", p("Under development."))
               
             ) # end strategy tabsetPanel
    ), # end strategy
    
    
    # ══ MODEL ═════════════════════════════════════════════════════════════════
    
    tabPanel("Model",
             tabsetPanel(
               tabPanel("Coming Soon", p("Under development."))
             ) # end model tabsetPanel
    ), # end model
    
    
    # ══ SELECTION ═════════════════════════════════════════════════════════════
    
    tabPanel("Selection",
             tabsetPanel(
               tabPanel("Coming Soon", p("Under development."))
             ) # end selection tabsetPanel
    ) # end selection
    
  ) # end tabsetPanel
) # end fluidPage


# =================================================================================
# server.R
# =================================================================================

server <- function(input, output, session) {
  
  onSessionEnded(function() { stopApp() })
  
  
  # ── REACTIVE DATA ──────────────────────────────────────────────────────────
  
  get_raw <- reactive({
    raw_dataset
  })
  
  get_data <- reactive({
    get_raw() |>
      apply_missingness_strategy(input$strat_na_vals)        |>
      apply_not_applicable_strategy(get_na_rules())          |>
      apply_threshold_strategy(
        target = input$strat_thresh_target,
        type   = input$strat_thresh_type,
        thresh = input$strat_thresh_val
      )                                                      |>
      apply_mmm_imputation(
        mean_cols   = input$strat_mmm_mean_cols,
        median_cols = input$strat_mmm_median_cols,
        mode_cols   = input$strat_mmm_mode_cols
      )
  })
  
  get_ms_data <- reactive({
    req(input$ms_vars)
    df <- get_data()
    df[, input$ms_vars, drop = FALSE]
  })
  
  
  # ── SERVER EDA DATATABLE ───────────────────────────────────────────────────
  
  output$data_table <- renderDT({
    get_data()
  }, options = list(pageLength = 20))
  
  
  # ── SERVER EDA SUMMARY ─────────────────────────────────────────────────────
  
  output$summary_output <- renderUI({
    df <- get_data()
    if (input$summary_style == "base") {
      tags$pre(paste(capture.output(summary(df)), collapse = "\n"))
    } else if (input$summary_style == "glimpse") {
      tags$pre(paste(capture.output(tibble::glimpse(df)), collapse = "\n"))
    } else if (input$summary_style == "dfsummary") {
      summarytools::dfSummary(df) |> print(method = "render")
    }
  })
  
  
  # ── SERVER EDA MISSINGNESS ─────────────────────────────────────────────────
  
  # populate variable selector
  observe({
    df <- get_data()
    updateSelectizeInput(session, "ms_vars",
                         choices  = names(df),
                         selected = names(df),
                         server   = TRUE
    )
  })
  
  # populate grouping variable dropdown with factor columns only
  observe({
    df          <- get_data()
    factor_cols <- names(df)[sapply(df, is.factor)]
    updateSelectInput(session, "ms_group_var", choices = c("(none)", factor_cols))
  })
  
  # populate level selector when grouping variable changes
  observeEvent(input$ms_group_var, {
    req(input$ms_group_var != "(none)")
    lvls <- levels(get_data()[[input$ms_group_var]])
    updateSelectizeInput(session, "ms_group_levels", choices = lvls, selected = lvls)
  })
  
  # missingness plot based on user choices
  output$ms_output <- renderPlot({
    req(input$ms_mode)
    plot_missingness(
      df           = get_ms_data(),
      full_df      = get_data(),
      mode         = input$ms_mode,
      group_on     = isTRUE(input$ms_group_on) && input$ms_group_var != "(none)",
      group_var    = input$ms_group_var,
      group_levels = input$ms_group_levels
    )
  })
  
  # Little's MCAR test
  output$ms_mcar_output <- renderPrint({
    req(input$ms_mcar)
    df     <- get_ms_data()
    df_num <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(df_num) < 2) {
      cat("Need at least 2 numeric variables for Little's MCAR test.\n")
      return(invisible(NULL))
    }
    naniar::mcar_test(df_num)
  })
  
  
  # ── SERVER EDA BOXPLOT ────────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    all_cols <- names(df)
    updateSelectInput(session, "bp_var",       choices = num_cols)
    updateSelectInput(session, "bp_label_col", choices = all_cols)
  })
  
  output$bp_output <- renderPlot({
    req(input$bp_var, input$bp_label_col, input$bp_iqr_k)
    plot_boxplot(get_data(), input$bp_var, input$bp_label_col, input$bp_iqr_k)
  })
  
  output$bp_outliers_table <- renderDT({
    req(input$bp_var, input$bp_iqr_k)
    get_boxplot_outliers(get_data(), input$bp_var, input$bp_iqr_k)
  }, options = list(pageLength = 10))
  
  
  # ── SERVER EDA BAGPLOT ────────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    all_cols <- names(df)
    updateSelectInput(session, "bag_x_var",    choices = num_cols)
    updateSelectInput(session, "bag_y_var",    choices = num_cols)
    updateSelectInput(session, "bag_label_col", choices = all_cols)
  })
  
  output$bag_output <- renderPlot({
    req(input$bag_x_var, input$bag_y_var, input$bag_label_col, input$bag_k)
    plot_bagplot(get_data(), input$bag_x_var, input$bag_y_var, input$bag_label_col, input$bag_k)
  })
  
  output$bag_outliers_table <- renderDT({
    req(input$bag_x_var, input$bag_y_var, input$bag_k)
    get_bagplot_outliers(get_data(), input$bag_x_var, input$bag_y_var, input$bag_k)
  }, options = list(pageLength = 10))
  
  
  # ── SERVER STRATEGY NOT APPLICABLE ────────────────────────────────────────
  
  # populate target and condition column dropdowns
  observe({
    df   <- get_raw()
    cols <- c("(none)", names(df))
    updateSelectInput(session, "strat_na_target_1",   choices = cols)
    updateSelectInput(session, "strat_na_cond_col_1", choices = cols)
  })
  
  observeEvent(input$strat_na_cond_col_1, {
    col <- input$strat_na_cond_col_1
    req(col != "(none)")
    vals <- sort(unique(as.character(get_raw()[[col]])))
    vals <- vals[!is.na(vals)]
    updateSelectInput(session, "strat_na_cond_val_1", choices = c("", vals))
  })
  
  # populate condition value dropdown when condition column changes
  lapply(1:3, function(i) {
    observeEvent(input[[paste0("strat_na_cond_col_", i)]], {
      col <- input[[paste0("strat_na_cond_col_", i)]]
      req(col != "(none)")
      vals <- unique(as.character(get_raw()[[col]]))
      vals <- sort(vals[!is.na(vals)])
      updateSelectInput(session, paste0("strat_na_cond_val_", i), choices = c("", vals))
    })
  })
  
  # reactive: collect rules from inputs
  get_na_rules <- reactive({
    list(list(
      target_col = input$strat_na_target_1,
      cond_col   = input$strat_na_cond_col_1,
      cond_val   = input$strat_na_cond_val_1,
      impute     = input$strat_na_impute_1
    ))
  })
  
  
  # ── SERVER STRATEGY MMM IMPUTATION ────────────────────────────────────────────
  
  observe({
    df        <- get_raw()
    num_cols  <- names(df)[sapply(df, is.numeric)]
    all_cols  <- names(df)
    
    updateSelectizeInput(session, "strat_mmm_mean_cols",   choices = num_cols, server = TRUE)
    updateSelectizeInput(session, "strat_mmm_median_cols", choices = num_cols, server = TRUE)
    updateSelectizeInput(session, "strat_mmm_mode_cols",   choices = all_cols, server = TRUE)
  })
  
  
} # end server







# =================================================================================
# Run
# =================================================================================

shinyApp(ui, server)