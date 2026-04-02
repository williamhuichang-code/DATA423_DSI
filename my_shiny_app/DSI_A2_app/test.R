# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(tidyverse)
library(DT)
library(rpart.plot)
library(recipes)

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
plot_missingness <- function(df, full_df, mode, group_on, group_var, group_levels, sort_cols, sort_rows) {
  
  make_plot <- function(df_sub, title = NULL) {
    
    if (sort_rows) {
      miss_pattern <- apply(df_sub, 1, function(x) paste(as.integer(is.na(x)), collapse = ""))
      df_sub <- df_sub[order(miss_pattern), , drop = FALSE]
    }
    
    p <- if (mode == "visdat") {
      visdat::vis_dat(df_sub)
    } else {
      visdat::vis_miss(df_sub, sort_miss = sort_cols)
    }
    
    p <- p + ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
      axis.text.y     = ggplot2::element_text(size = 11),
      legend.text     = ggplot2::element_text(size = 11),
      legend.title    = ggplot2::element_text(size = 12)
    )
    
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

# plot correlation corrgram (reusable for miss cor and variable heatmap)
plot_corrgram <- function(cor_mat, order, title = NULL, cex_var = 1.1) {
  
  par(oma = c(0, 0, 3, 0))
  
  corrgram::corrgram(
    cor_mat,
    order      = if (order == "FALSE") FALSE else order,
    abs        = FALSE,
    cex.var    = cex_var
  )
  
  if (!is.null(title)) {
    title(main = title, cex.main = 1.8, font.main = 2, outer = TRUE, line = 1)
  }
}

# plot missingness correlation
plot_miss_cor <- function(df, order, abs_cor) {
  
  m  <- is.na(df) + 0
  cm <- colMeans(m)
  m  <- m[, cm > 0 & cm < 1, drop = FALSE]
  
  if (ncol(m) < 2) {
    plot.new()
    text(0.5, 0.5,
         "Need at least 2 partially-missing variables\nto compute missingness correlation.",
         cex = 1.2, col = "#C41E3A", adj = 0.5)
    return()
  }
  
  cor_m <- cor(m)
  if (abs_cor) cor_m <- abs(cor_m)
  
  plot_corrgram(
    cor_mat = cor_m,
    order   = order,
    title   = "Variable Missingness Correlation"
  )
}

# plot missingness prediction tree
plot_miss_tree <- function(df, id_col) {
  
  # step 1, sort by id column and convert to numeric row sequence
  df <- df[order(df[[id_col]]), ]
  df[[id_col]] <- seq_len(nrow(df))
  
  # step 2, add missingness count as outcome column
  df$missingness <- apply(X = is.na(df), MARGIN = 1, FUN = sum)
  
  # step 3, train rpart tree to predict missingness from all other variables
  tree <- caret::train(
    missingness ~ .,
    data      = df,
    method    = "rpart",
    na.action = na.rpart,
  )
  
  # step 4, plot the tree
  rpart.plot::rpart.plot(
    tree$finalModel,
    main      = "Predicting the number of missing variables in an observation",
    sub       = "Check whether the outcome variable is an important variable",
    roundint  = TRUE,
    clip.facs = TRUE,
    cex       = 1.2,
    branch      = 0.5,
    box.palette = "Blues"
  )
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
# a helper p_miss utility (percentage of missing values in a vector)
p_miss <- function(x) sum(is.na(x)) / length(x)

# for complete deletion, handle variables first, then observations
apply_col_threshold <- function(df, thresh) {
  if (is.null(thresh) || is.na(thresh)) return(df)
  c_ratio <- apply(df, 2, p_miss)
  df[, c_ratio < thresh, drop = FALSE]
}

# for complete deletion, then handle observations
apply_row_threshold <- function(df, thresh) {
  if (is.null(thresh) || is.na(thresh)) return(df)
  r_ratio <- apply(df, 1, p_miss)
  df[r_ratio < thresh, , drop = FALSE]
}

# for informative missingness, create binary shadow columns
apply_shadow_variables <- function(df, shadow_cols) {
  if (is.null(shadow_cols) || length(shadow_cols) == 0) return(df)
  for (col in shadow_cols) {
    if (!col %in% names(df)) next
    df[[paste0(col, "_shadow")]] <- as.integer(is.na(df[[col]]))
  }
  df
}

# for knn imputation
apply_knn_imputation <- function(df, knn_response, knn_cols, knn_k) {
  if (is.null(knn_response) || knn_response == "" || knn_response == "(none)") return(df)
  if (is.null(knn_cols) || length(knn_cols) == 0) return(df)
  if (is.null(knn_k) || is.na(knn_k) || knn_k < 1) return(df)
  if (!knn_response %in% names(df)) return(df)
  
  cols_present <- intersect(knn_cols, setdiff(names(df), knn_response))
  if (length(cols_present) == 0) return(df)
  
  # exclude y entirely so it cannot influence neighbour distance calculation
  df_pred <- df[, setdiff(names(df), knn_response), drop = FALSE]
  
  imputed <- recipes::recipe(~ ., data = df_pred) |>
    recipes::step_impute_knn(tidyselect::all_of(cols_present), neighbors = knn_k) |>
    recipes::prep(training = df_pred) |>
    recipes::bake(new_data = df_pred)
  
  # put y back unchanged, preserving original column order
  df[, setdiff(names(df), knn_response)] <- imputed
  df
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
               
               
               # ── UI VISMISS ────────────────────────────────────────────────────────
               
               tabPanel("Vis Miss",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("VisMiss: <br><br>
                                       Visualise missing data patterns across the dataset.
                                       Group by a categorical variable to reveal whether
                                       missingness differs across subgroups."),
                                       hr(),
                                       selectizeInput("ms_vars", "Variables to plot:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       sliderInput("ms_downsample", "Downsample limit (observations):",
                                                   min = 100, max = 10000, value = 10000, step = 10, width = "100%"),
                                       hr(),
                                       radioButtons("ms_mode", "View:",
                                                    choices = c("vis_dat (type + missingness)" = "visdat",
                                                                "vis_miss (missingness only)"  = "vismiss"),
                                                    selected = "visdat"),
                                       hr(),
                                       tags$b("Order:"),
                                       checkboxInput("ms_sort_cols", "Sort columns by missingness rate",  value = FALSE),
                                       checkboxInput("ms_sort_rows", "Sort rows by missingness pattern",  value = FALSE),
                                       hr(),
                                       tags$b("Group by:"),
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
                                       tags$b("Little's MCAR test:"),
                                       checkboxInput("ms_mcar", "Test for MCAR (Little's test)", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.ms_mcar == true",
                                         sidebar_note("Little's MCAR test: <br><br>
                                         H0: data is Missing Completely At Random.
                                         A significant result suggests missingness is NOT random — i.e. MAR or MNAR.")
                                       )
                          ),
                          mainPanel(width = 9,
                                    plotOutput("ms_output", height = "100vh"),
                                    conditionalPanel(
                                      condition = "input.ms_mcar == true",
                                      hr(),
                                      h4("Little's MCAR Test Result"),
                                      verbatimTextOutput("ms_mcar_output")
                                    )
                          )
                        )
               ), # end tab panel
               
               
               # ── UI MISS COR ───────────────────────────────────────────────────────────────
               
               tabPanel("Miss Cor",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Miss Cor: <br><br>
                                       Visualise the correlation structure of missingness across variables.
                                       Variables are converted to binary (1 = missing, 0 = present),
                                       then correlated. Only variables with partial missingness are shown
                                       (fully present or fully missing variables are excluded)."),
                                       hr(),
                                       selectizeInput("mc_vars", "Variables to include:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       selectInput("mc_order", "Variable ordering:",
                                                   choices  = c("OLO (Optimal Leaf)" = "OLO",
                                                                "AOE (Eigenvector)"  = "TRUE",
                                                                "HC (Hierarchical)"  = "HC",
                                                                "Original"           = "FALSE"),
                                                   selected = "OLO"),
                                       hr(),
                                       checkboxInput("mc_abs", "Absolute correlation", value = TRUE)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("mc_output", height = "85vh")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI MISS UPSET ─────────────────────────────────────────────────────────────
               
               tabPanel("Miss Upset",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Miss Upset: <br><br>
                                       Visualise patterns of missingness across variables.
                                       Each bar represents a unique combination of missing variables,
                                       sorted by frequency. Reveals whether variables tend to be
                                       missing together."),
                                       hr(),
                                       selectizeInput("mu_vars", "Variables to include:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       numericInput("mu_nsets", "Max variable sets (nsets):",
                                                    value = 6, min = 2, max = 20, step = 1)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("mu_output", height = "85vh")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI MISS TREE ──────────────────────────────────────────────────────────────
               
               tabPanel("Miss Tree",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Miss Tree: <br><br>
                                       Predicts the number of missing values per observation using an rpart tree.
                                       If missingness can be predicted from other variables, it is NOT missing
                                       completely at random (MCAR). <br><br>
                                       A single-node tree suggests MCAR cannot be ruled out. <br><br>
                                       Multiple nodes rule out MCAR, suggesting MAR or MNAR is likely. <br><br>
                                       Need domain knowledge and data collecting process to understand issues. <br><br>
                                       When nodes include y response, it's informative missingness, need shadow cols. <br><br>
                                       When nodes include row index, it's sequential missingness, need domain explanation"),
                                       hr(),
                                       sidebar_note("Note: rpart handles missing predictors intrinsically —
                                       no cleaning is applied before this model."),
                                       hr(),
                                       selectInput("mt_id_col", "ID column (converted to numeric order):",
                                                   choices = NULL)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("mt_output", height = "85vh")
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
               
               
               tabPanel("MISS: Shadow Variables",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: Shadow Variables: <br><br>
                                       When missingness may be <b>informative</b> (i.e. the fact that a value 
                                       is missing is itself meaningful), create a binary shadow variable 
                                       (1 = missing, 0 = present) before losing NA info from imputing/removing. <br><br>
                                       This preserves the missingness signal even after imputation.
                                       Generate these <b>before</b> applying imputation/deletion strategies."),
                                       hr(),
                                       selectizeInput("strat_shadow_cols", "Columns to create shadow variables for:",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(placeholder = "Select columns with missingness"))
                          ),
                          mainPanel(width = 9,
                                    p("Strategy applied. Shadow columns are appended with '_shadow' suffix."),
                                    hr(),
                                    h5("Shadow Variables Created"),
                                    verbatimTextOutput("shadow_summary")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("MISS: Threshold",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: Threshold: <br><br>
                                       Remove variables (columns) with too much missingness first,
                                       then remove observations (rows). Order matters."),
                                       hr(),
                                       h6("Step 1 — Variable Threshold"),
                                       sliderInput("strat_var_thresh", "Max missingness per variable:",
                                                   min = 0, max = 1, value = 1, step = 0.01, width = "100%"),
                                       hr(),
                                       h6("Step 2 — Observation Threshold"),
                                       sliderInput("strat_obs_thresh", "Max missingness per observation:",
                                                   min = 0, max = 1, value = 1, step = 0.01, width = "100%")
                          ),
                          mainPanel(width = 9,
                                    p("Strategy applied. Check EDA > Data Table or Missingness to see the effect."),
                                    hr(),
                                    sidebar_note("Scenarios that prevent from deleting cols: <br><br>
                                    1. Important variable from domain concern. <br><br>
                                    2. Watch out for <b>Not Applicable</b>"),
                                    hr(),
                                    h5("Step 1 — Variables Removed"),
                                    verbatimTextOutput("thresh_col_summary"),
                                    DT::dataTableOutput("thresh_col_removed"),
                                    hr(),
                                    h5("Step 2 — Observations Removed"),
                                    verbatimTextOutput("thresh_row_summary"),
                                    DT::dataTableOutput("thresh_row_removed")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("MISS: KNN Imputation",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: KNN Imputation: <br><br>
                                       Impute selected predictor columns using K-Nearest Neighbours. <br><br>
                                       Select the <b>response variable (y)</b> first — it will never
                                       be used to impute. Then select which predictor columns to impute. <br><br>
                                       KNN uses the multivariate structure of all predictors as 
                                       neighbours to estimate missing values. <br><br>
                                       More robust than MMM when missingness is MAR. <br><br>
                                       Apply <b>after</b> shadow variables and <b>before</b> modelling."),
                                       hr(),
                                       selectInput("strat_knn_response", "Response variable (y):",
                                                   choices = NULL),
                                       hr(),
                                       numericInput("strat_knn_k", "Number of neighbours (k):",
                                                    value = 5, min = 1, max = 50, step = 1),
                                       hr(),
                                       selectizeInput("strat_knn_cols", "Columns to impute with KNN:",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(placeholder = "Select predictor columns"))
                          ),
                          mainPanel(width = 9,
                                    p("Strategy applied. Check EDA > Data Table or Missingness to see the effect."),
                                    hr(),
                                    h5("KNN Imputation Summary"),
                                    verbatimTextOutput("knn_summary")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("MISS: MMM Imputation",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: MMM Imputation: <br><br>
                                       Impute missing values by column using mean, median, or mode. <br><br>
                                       It could be useful when MCAR, eventhough MCAR is rarely confirmed.
                                       Sometimes you cannot rule out MCAR, but you cannot prove it either.")
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
    get_raw()                                                    |>
      apply_missingness_strategy(input$strat_na_vals)            |>
      apply_not_applicable_strategy(get_na_rules())              |>
      apply_shadow_variables(input$strat_shadow_cols)            |>
      apply_col_threshold(input$strat_var_thresh)                |>
      apply_row_threshold(input$strat_obs_thresh)                |>
      apply_mmm_imputation(
        mean_cols   = input$strat_mmm_mean_cols,
        median_cols = input$strat_mmm_median_cols,
        mode_cols   = input$strat_mmm_mode_cols
      )                                                          |>
      apply_knn_imputation(
        knn_response = input$strat_knn_response,
        knn_cols     = input$strat_knn_cols,
        knn_k        = input$strat_knn_k
      )
  })
  
  get_ms_data_plot <- reactive({
    req(input$ms_vars)
    df <- get_data()
    if (!is.null(input$ms_downsample) && input$ms_downsample < nrow(df)) {
      samp <- sort(sample(1:nrow(df), input$ms_downsample))
      df   <- df[samp, , drop = FALSE]
    }
    df[, input$ms_vars, drop = FALSE]
  })
  
  get_ms_data_full <- reactive({
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
  
  
  # ── SERVER EDA VISMISS ─────────────────────────────────────────────────────
  
  # populate variable selector
  observe({
    df <- get_data()
    updateSelectizeInput(session, "ms_vars",
                         choices  = names(df),
                         selected = names(df),
                         server   = TRUE
    )
  })
  
  # update downsample slider max to match current nrow
  observe({
    df <- get_data()
    updateSliderInput(session, "ms_downsample",
                      max   = nrow(df),
                      value = nrow(df))
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
      df           = get_ms_data_plot(),
      full_df      = get_data(),
      mode         = input$ms_mode,
      group_on     = isTRUE(input$ms_group_on) && input$ms_group_var != "(none)",
      group_var    = input$ms_group_var,
      group_levels = input$ms_group_levels,
      sort_cols    = isTRUE(input$ms_sort_cols),
      sort_rows    = isTRUE(input$ms_sort_rows)
    )
  })
  
  # Little's MCAR test
  output$ms_mcar_output <- renderPrint({
    req(input$ms_mcar)
    df     <- get_ms_data_full()
    df_num <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(df_num) < 2) {
      cat("Need at least 2 numeric variables for Little's MCAR test.\n")
      return(invisible(NULL))
    }
    result <- naniar::mcar_test(df_num)
    cat("Little's MCAR Test\n")
    cat(sprintf("Chi-square : %.2f\n", result$statistic))
    cat(sprintf("df         : %d\n",   result$df))
    cat(sprintf("p-value    : %.2e\n", result$p.value))
    cat(sprintf("Patterns   : %d\n",   result$missing.patterns))
    cat("──────────────────────────────\n")
    if (result$p.value < 0.05) {
      cat("Conclusion : Reject H0 — data is NOT MCAR (likely MAR or MNAR).\n")
    } else {
      cat("Conclusion : Fail to reject H0 — data is consistent with MCAR.\n")
    }
  })
  
  
  # ── SERVER EDA MISS UPSET ─────────────────────────────────────────────────────
  
  observe({
    df <- get_data()
    updateSelectizeInput(session, "mu_vars",
                         choices  = names(df),
                         selected = names(df),
                         server   = TRUE)
  })
  
  output$mu_output <- renderPlot({
    req(input$mu_vars)
    df <- get_data()
    df <- df[, intersect(input$mu_vars, names(df)), drop = FALSE]
    
    # need at least one variable with missingness
    has_miss <- sapply(df, anyNA)
    if (!any(has_miss)) {
      plot.new()
      text(0.5, 0.5, "No missing values in selected variables.",
           cex = 1.2, col = "#C41E3A", adj = 0.5)
      return()
    }
    
    naniar::gg_miss_upset(data = df, nsets = input$mu_nsets, text.scale = 2)
  })
  
  
  # ── SERVER EDA MISS COR ───────────────────────────────────────────────────────
  
  observe({
    df <- get_data()
    updateSelectizeInput(session, "mc_vars",
                         choices  = names(df),
                         selected = names(df),
                         server   = TRUE)
  })
  
  output$mc_output <- renderPlot({
    req(input$mc_vars)
    df <- get_data()
    df <- df[, intersect(input$mc_vars, names(df)), drop = FALSE]
    plot_miss_cor(df, input$mc_order, isTRUE(input$mc_abs))
  })
  
  
  # ── SERVER EDA MISS TREE ──────────────────────────────────────────────────────
  
  observe({
    df <- get_data()
    updateSelectInput(session, "mt_id_col", choices = names(df), selected = names(df)[1])
  })
  
  output$mt_output <- renderPlot({
    req(input$mt_id_col)
    df <- get_data()
    
    has_miss <- sapply(df, anyNA)
    if (!any(has_miss)) {
      plot.new()
      text(0.5, 0.5, "No missing values in dataset — tree not applicable.",
           cex = 1.2, col = "#C41E3A", adj = 0.5)
      return()
    }
    
    plot_miss_tree(df, input$mt_id_col)
  }, res = 96, height = 700)
  
  
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
  
  
  # ── SERVER STRATEGY SHADOW VARIABLES ──────────────────────────────────────────
  
  observe({
    df       <- get_raw()
    has_miss <- names(df)[sapply(df, anyNA)]
    updateSelectizeInput(session, "strat_shadow_cols",
                         choices = has_miss,
                         server  = TRUE)
  })
  
  output$shadow_summary <- renderPrint({
    cols <- input$strat_shadow_cols
    if (is.null(cols) || length(cols) == 0) {
      cat("No shadow variables created.\n")
      return(invisible(NULL))
    }
    cat(sprintf("Shadow variables created: %d\n", length(cols)))
    cat("──────────────────────────────\n")
    for (col in cols) {
      cat(sprintf("  %s  →  %s_shadow\n", col, col))
    }
  })
  
  
  # ── SERVER STRATEGY THRESHOLD ─────────────────────────────────────────────────
  
  output$thresh_col_summary <- renderPrint({
    df_before <- get_raw() |>
      apply_missingness_strategy(input$strat_na_vals) |>
      apply_not_applicable_strategy(get_na_rules())
    df_after <- apply_col_threshold(df_before, input$strat_var_thresh)
    removed  <- ncol(df_before) - ncol(df_after)
    cat(sprintf("Variables removed: %d  |  Remaining: %d / %d",
                removed, ncol(df_after), ncol(df_before)))
  })
  
  output$thresh_col_removed <- renderDT({
    df_before <- get_raw() |>
      apply_missingness_strategy(input$strat_na_vals) |>
      apply_not_applicable_strategy(get_na_rules())
    c_ratio <- apply(df_before, 2, p_miss)
    removed <- c_ratio[c_ratio >= input$strat_var_thresh]
    if (length(removed) == 0) return(data.frame(Message = "No variables removed."))
    data.frame(
      Variable     = names(removed),
      Missing_Prop = round(removed, 4),
      row.names    = NULL
    )
  }, options = list(pageLength = 10, dom = "tip"))
  
  output$thresh_row_summary <- renderPrint({
    df_before <- get_raw() |>
      apply_missingness_strategy(input$strat_na_vals) |>
      apply_not_applicable_strategy(get_na_rules())
    df_col   <- apply_col_threshold(df_before, input$strat_var_thresh)
    df_after <- apply_row_threshold(df_col, input$strat_obs_thresh)
    removed  <- nrow(df_col) - nrow(df_after)
    cat(sprintf("Observations removed: %d  |  Remaining: %d / %d",
                removed, nrow(df_after), nrow(df_col)))
  })
  
  output$thresh_row_removed <- renderDT({
    df_before <- get_raw() |>
      apply_missingness_strategy(input$strat_na_vals) |>
      apply_not_applicable_strategy(get_na_rules())
    df_col  <- apply_col_threshold(df_before, input$strat_var_thresh)
    r_ratio <- apply(df_col, 1, p_miss)
    mask    <- r_ratio >= input$strat_obs_thresh
    if (sum(mask) == 0) return(data.frame(Message = "No observations removed."))
    removed_df <- df_col[mask, , drop = FALSE]
    removed_df <- cbind(
      Row          = which(mask),
      Missing_Prop = round(r_ratio[mask], 4),
      removed_df
    )
    removed_df
  }, options = list(pageLength = 10, dom = "tip", scrollX = TRUE))
  
  
  # ── SERVER STRATEGY KNN IMPUTATION ────────────────────────────────────────────
  
  observe({
    df   <- get_raw()
    cols <- c("(none)", names(df))
    updateSelectInput(session, "strat_knn_response", choices = cols, selected = "(none)")
  })
  
  # when response changes, update imputable columns to exclude it
  observeEvent(input$strat_knn_response, {
    df   <- get_data()
    resp <- input$strat_knn_response
    eligible <- if (is.null(resp) || resp == "(none)") {
      names(df)[sapply(df, is.numeric)]
    } else {
      cols <- setdiff(names(df), resp)
      cols[sapply(df[cols], is.numeric)]
    }
    updateSelectizeInput(session, "strat_knn_cols",
                         choices  = eligible,
                         selected = NULL,
                         server   = TRUE)
  })
  
  output$knn_summary <- renderPrint({
    resp <- input$strat_knn_response
    cols <- input$strat_knn_cols
    k    <- input$strat_knn_k
    
    if (is.null(resp) || resp == "(none)" || resp == "") {
      cat("No response variable selected — KNN imputation not applied.\n")
      return(invisible(NULL))
    }
    if (is.null(cols) || length(cols) == 0) {
      cat("No predictor columns selected for KNN imputation.\n")
      return(invisible(NULL))
    }
    
    cat(sprintf("Response (y)   : %s\n", resp))
    cat(sprintf("Neighbours (k) : %d\n", k))
    cat(sprintf("Columns        : %d\n", length(cols)))
    cat("──────────────────────────────\n")
    for (col in cols) {
      cat(sprintf("  %s\n", col))
    }
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