# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(tidyverse)
library(sortable)
library(DT)
library(plotly)
library(tabplot)
library(GGally)
library(vcd)
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

# plot datatable with hints (yellow highlight for values changed from raw)
make_hints_dt <- function(df_raw, df_clean, page_length = 10) {
  shared_cols <- intersect(names(df_raw),    names(df_clean))
  shared_rows <- intersect(rownames(df_raw), rownames(df_clean))
  
  display <- df_clean[shared_rows, , drop = FALSE]
  display[shared_cols] <- lapply(shared_cols, function(col) as.character(df_clean[shared_rows, col]))
  
  mask <- matrix(FALSE, nrow = length(shared_rows), ncol = ncol(df_clean),
                 dimnames = list(shared_rows, names(df_clean)))
  
  for (col in shared_cols) {
    r_char  <- as.character(df_raw[shared_rows, col])
    c_char  <- as.character(df_clean[shared_rows, col])
    r_na    <- is.na(df_raw[shared_rows, col])
    c_na    <- is.na(df_clean[shared_rows, col])
    changed <- (r_na != c_na) | (!r_na & !c_na & r_char != c_char)
    mask[, col]    <- changed
    display[[col]] <- ifelse(changed, paste0(ifelse(r_na, "NA", r_char), " \u2192 ", ifelse(c_na, "NA", c_char)), c_char)
  }
  
  extra_cols <- setdiff(names(df_clean), shared_cols)
  if (length(extra_cols) > 0) {
    extra_mask <- matrix(FALSE, nrow = nrow(mask), ncol = length(extra_cols),
                         dimnames = list(rownames(mask), extra_cols))
    mask <- cbind(mask, extra_mask)
  }
  mask <- mask[, names(df_clean)[names(df_clean) %in% colnames(mask)], drop = FALSE]
  
  mask_by_row <- apply(mask, 1, function(row) paste(as.integer(row), collapse = ","))
  js_array    <- paste0("[", paste(sapply(mask_by_row, function(m) paste0("[", m, "]")), collapse = ","), "]")
  
  datatable(display,
            options = list(
              pageLength = page_length, scrollX = TRUE,
              rowCallback = JS(sprintf(
                "function(row, data, displayIndex, displayIndexFull) {
          var masks = %s;
          var mask  = masks[displayIndexFull];
          if (!mask) return;
          for (var i = 0; i < mask.length; i++) {
            if (mask[i] === 1) {
              $('td:eq(' + (i + 1) + ')', row).css({'background-color':'#fff3cd','font-weight':'600'});
            }
          }
        }", js_array))
            )
  )
}

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

# plot rising value graph
plot_rising_value <- function(df, vars, transform, omit_na, lwd, lty) {
  
  plot_df <- do.call(rbind, lapply(vars, function(v) {
    y <- df[[v]]
    if (omit_na) y <- na.omit(y)
    
    y <- switch(transform,
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
    geom_line(linewidth = lwd / 3, linetype = lty) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right") +
    labs(
      title = paste0(
        "Rising Value Graph",
        if (omit_na)        " | NAs Omitted" else "",
        if (transform != "none") paste0(" | ", tools::toTitleCase(transform)) else ""
      ),
      x = "Percentile",
      y = "Value"
    )
  
  plotly::ggplotly(p, tooltip = "text") |>
    plotly::layout(
      legend = list(font = list(size = 13)),
      xaxis  = list(title = list(text = "<b>Percentile</b>", font = list(size = 14))),
      yaxis  = list(title = list(text = "<b>Value</b>",      font = list(size = 14)))
    )
}

# plot correlation corrgram (reusable for miss cor and variable heatmap)
plot_corrgram <- function(cor_mat, order, title = NULL) {
  
  par(oma = c(0, 0, 3, 0))
  
  corrgram::corrgram(
    cor_mat,
    order = if (order == "FALSE") FALSE else order,
    abs   = FALSE
  )
  
  if (!is.null(title)) {
    title(main = title, cex.main = 1.8, font.main = 2, outer = TRUE, line = 1)
  }
}

# plot missingness correlation
plot_miss_cor <- function(df, order, abs_cor, method = "pearson") {
  
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
  
  cor_m <- cor(m, method = method)
  if (abs_cor) cor_m <- abs(cor_m)
  
  plot_corrgram(
    cor_mat = cor_m,
    order   = order,
    title   = paste0("Variable Missingness Correlation | ", tools::toTitleCase(method),
                     if (abs_cor) " | Absolute" else "")
  )
}

# plot missingness prediction tree
plot_miss_tree <- function(df, id_col, vars = NULL) {
  
  df <- df[order(df[[id_col]]), ]
  df[[id_col]] <- seq_len(nrow(df))
  df$missingness <- apply(X = is.na(df), MARGIN = 1, FUN = sum)
  
  if (!is.null(vars) && length(vars) > 0) {
    keep <- union(vars, c(id_col, "missingness"))
    df   <- df[, intersect(keep, names(df)), drop = FALSE]
  }
  
  tree <- caret::train(
    missingness ~ .,
    data      = df,
    method    = "rpart",
    na.action = na.rpart
  )
  
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

# plot missingness variable importance
plot_miss_importance <- function(df, id_col, response_col) {
  
  df <- df[order(df[[id_col]]), ]
  df[[id_col]] <- seq_len(nrow(df))
  df$missingness <- apply(X = is.na(df), MARGIN = 1, FUN = sum)
  
  tree <- caret::train(
    missingness ~ .,
    data      = df,
    method    = "rpart",
    na.action = na.rpart
  )
  
  imp <- caret::varImp(tree)$importance
  imp$variables  <- rownames(imp)
  imp$importance <- imp$Overall / sum(imp$Overall)
  imp <- imp[imp$importance > 0, ]
  imp <- imp[order(-imp$importance), ]
  
  interesting_vars <- c(id_col)
  if (!is.null(response_col) && response_col != "(none)") interesting_vars <- c(interesting_vars, response_col)
  imp$interesting  <- imp$variables %in% interesting_vars
  imp$variables    <- factor(imp$variables, levels = imp$variables)
  
  ggplot(imp, aes(x = variables, y = importance, fill = interesting)) +
    geom_col() +
    scale_fill_manual(values = c("FALSE" = "#e05c4b", "TRUE" = "#36b5a2")) +
    labs(
      title = "Variable Importance for predicting missing variables per observation",
      x     = "variables",
      y     = "Relative Importance",
      fill  = "interesting"
    ) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y  = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 11)
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
  
  ggplot(df, aes(x = .data[[var]], y = 1)) +
    geom_boxplot(coef = iqr_k, outlier.colour = "#C41E3A") +
    ggrepel::geom_text_repel(aes(label = label), max.overlaps = 20, na.rm = TRUE) +
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

# plot tabplot graph
plot_tabplot <- function(df, vars, sort_on, sort_var, sort_desc, transform, nbin) {
  
  sel <- intersect(vars, names(df))
  if (length(sel) == 0) return(invisible(NULL))
  
  df_plot <- df[, sel, drop = FALSE]
  
  if (transform != "none") {
    num_cols <- names(df_plot)[sapply(df_plot, is.numeric)]
    df_plot[num_cols] <- lapply(df_plot[num_cols], function(y) {
      switch(transform,
             "center"      = y - mean(y, na.rm = TRUE),
             "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
             "normalise"   = (y - min(y, na.rm = TRUE)) /
               (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
      )
    })
  }
  
  using_index <- !isTRUE(sort_on)
  
  if (using_index) {
    df_plot$.row_index <- seq_len(nrow(df_plot))
    sort_col <- ".row_index"
    df_plot  <- df_plot[, c(".row_index", setdiff(names(df_plot), ".row_index")), drop = FALSE]
  } else {
    sort_col <- sort_var
    if (!sort_col %in% names(df_plot)) df_plot[[sort_col]] <- df[[sort_col]]
    if (inherits(df_plot[[sort_col]], "Date")) df_plot[[sort_col]] <- as.numeric(df_plot[[sort_col]])
    df_plot <- df_plot[, c(sort_col, setdiff(names(df_plot), sort_col)), drop = FALSE]
  }
  
  tabplot::tableplot(
    df_plot,
    sortCol    = sort_col,
    decreasing = if (using_index) FALSE else isTRUE(sort_desc),
    nBins      = nbin,
    title      = paste0("Tabplot | Sorted by ", if (using_index) "Row Order" else sort_var,
                        if (transform != "none") paste0(" | ", tools::toTitleCase(transform)) else ""),
    fontsize       = 14,
    fontsize.title = 22
  )
}


# plot mosaic graph
plot_mosaic <- function(df, x_var, y_var, z_var, shade, rot_labels, abbreviate, fontsize) {
  
  if (z_var == "None") {
    tbl <- table(df[[x_var]], df[[y_var]])
    names(dimnames(tbl)) <- c(x_var, y_var)
  } else {
    tbl <- table(df[[x_var]], df[[y_var]], df[[z_var]])
    names(dimnames(tbl)) <- c(x_var, y_var, z_var)
  }
  
  vars_used  <- c(x_var, y_var, if (z_var != "None") z_var)
  plot_title <- paste0("Mosaic | ", paste(vars_used, collapse = " × "))
  
  vcd::mosaic(tbl,
              shade    = shade,
              legend   = shade,
              main     = plot_title,
              labeling = vcd::labeling_border(
                rot_labels = c(rot_labels, 0, 0, 0),
                gp_labels  = grid::gpar(fontsize = fontsize),
                abbreviate = abbreviate
              ))
}

# plot ggpairs graph
plot_ggpairs <- function(df, vars, group_on, group_var, group_levels, cardinality_threshold = 15) {
  
  sel <- intersect(vars, names(df))
  
  # drop factor cols with too many levels
  sel <- sel[sapply(sel, function(v) {
    x <- df[[v]]
    if (is.factor(x) || is.character(x)) length(unique(na.omit(x))) <= cardinality_threshold
    else TRUE
  })]
  
  if (length(sel) < 2) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "Not enough plottable variables.\nHigh-cardinality factors were excluded.",
               colour = "#C41E3A", size = 5) +
      theme_void()
    return()
  }
  
  if (group_on && !is.null(group_var) && group_var %in% names(df)) {
    plot_df  <- df[, c(sel, group_var), drop = FALSE]
    plot_df  <- plot_df[!is.na(plot_df[[group_var]]), ]
    if (!is.null(group_levels) && length(group_levels) > 0) {
      plot_df <- plot_df[as.character(plot_df[[group_var]]) %in% group_levels, ]
      plot_df[[group_var]] <- droplevels(as.factor(plot_df[[group_var]]))
    }
    col_idx <- seq_along(sel)
    GGally::ggpairs(
      plot_df, progress = FALSE, columns = col_idx,
      mapping = aes(colour = .data[[group_var]], alpha = 0.6),
      upper   = list(continuous = GGally::wrap("cor", size = 4),
                     combo      = GGally::wrap("box_no_facet", alpha = 0.5),
                     discrete   = GGally::wrap("facetbar", alpha = 0.5)),
      lower   = list(continuous = GGally::wrap("points", alpha = 0.3, size = 0.8),
                     combo      = GGally::wrap("facethist", bins = 20, alpha = 0.5),
                     discrete   = GGally::wrap("facetbar", alpha = 0.5)),
      diag    = list(continuous = GGally::wrap("densityDiag", alpha = 0.5),
                     discrete   = GGally::wrap("barDiag", alpha = 0.5)),
      legend  = 1
    ) +
      theme_minimal(base_size = 13) +
      theme(plot.title  = element_text(size = 20, face = "bold", hjust = 0.5),
            strip.text  = element_text(size = 14, face = "bold"),
            axis.text   = element_text(size = 14),
            legend.title = element_text(face = "bold"),
            legend.text  = element_text(size = 12)) +
      labs(title = paste0("GGPairs | Grouped by ", group_var))
    
  } else {
    plot_df <- df[, sel, drop = FALSE]
    GGally::ggpairs(
      plot_df, progress = FALSE,
      upper = list(continuous = GGally::wrap("cor", size = 4)),
      lower = list(continuous = GGally::wrap("points", alpha = 0.4, size = 0.8)),
      diag  = list(continuous = GGally::wrap("densityDiag"))
    ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            strip.text = element_text(size = 14, face = "bold"),
            axis.text  = element_text(size = 14)) +
      labs(title = "GGPairs")
  }
}

# plot variable correlation heatmap
plot_var_cor <- function(df, vars, method, order, abs_cor) {
  
  num_vars <- vars[sapply(vars, function(v) is.numeric(df[[v]]))]
  
  if (length(num_vars) < 2) {
    plot.new()
    text(0.5, 0.5,
         "Need at least 2 numeric variables\nto compute correlation.",
         cex = 1.2, col = "#C41E3A", adj = 0.5)
    return()
  }
  
  df_num  <- df[, num_vars, drop = FALSE]
  cor_mat <- cor(df_num, use = "pairwise.complete.obs", method = method)
  if (abs_cor) cor_mat <- abs(cor_mat)
  
  plot_corrgram(
    cor_mat = cor_mat,
    order   = order,
    title   = paste0("Variable Correlation Heatmap | ", tools::toTitleCase(method),
                     if (abs_cor) " | Absolute" else "")
  )
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
               
               # ── UI DATA ROLES ─────────────────────────────────────────────────────
               
               tabPanel("Data Roles",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Data Roles: <br><br>
                        Drag variables between role buckets. Only <b>Predictors</b> and the <b>Outcome</b>
                        are used in modelling."),
                                       hr(),
                                       actionButton("roles_reset", "Reset all to Predictor",
                                                    class = "btn-sm btn-outline-secondary")
                          ),
                          mainPanel(width = 9,
                                    uiOutput("roles_buckets_ui")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI DATATABLE ──────────────────────────────────────────────────────
               
               tabPanel("Data Table",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Data Table: <br><br>
                                       View the current dataset after all strategies are applied. <br><br>
                                       <b style='color:#856404;'>Yellow cells</b> indicate values
                                       changed by any strategy compared to raw."),
                                       hr(),
                                       radioButtons("dt_view_mode", "View mode:",
                                                    choices = c(
                                                      "Raw"     = "raw",
                                                      "Current" = "current",
                                                      "Current with Hints"   = "hints"
                                                    ),
                                                    selected = "hints")
                          ),
                          mainPanel(width = 9,
                                    DT::dataTableOutput("data_table")
                          )
                        )
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
                                       h6("Or"),
                                       checkboxInput("ms_sort_by_col_on", "Sort rows by a specific column value", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.ms_sort_by_col_on == true",
                                         selectInput("ms_sort_by_col", "Column to sort by:", choices = NULL)
                                       ),
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
               
               
               # ── UI RISING VALUE ───────────────────────────────────────────────────────────
               
               tabPanel("Rising Value",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Rising Value: <br><br>
                        Sorted values plotted against their percentile rank.
                        Smooth curves suggest a continuous variable; visible
                        steps or gaps may signal suspicious patterns."),
                                       hr(),
                                       selectizeInput("rv_vars", "Numeric variables to plot:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       checkboxInput("rv_omit_na", "Ignore NAs", value = FALSE),
                                       hr(),
                                       radioButtons("rv_transform", "Transform:",
                                                    choices = c("None"        = "none",
                                                                "Centre"      = "center",
                                                                "Standardise" = "standardise",
                                                                "Normalise"   = "normalise"),
                                                    selected = "normalise"),
                                       hr(),
                                       sliderInput("rv_lwd", "Line width:",
                                                   min = 0.2, max = 5, value = 1.6, step = 0.1, width = "100%"),
                                       hr(),
                                       radioButtons("rv_lty", "Line type:",
                                                    choices = c("Solid"    = "solid",
                                                                "Dashed"   = "dashed",
                                                                "Dotted"   = "dotted",
                                                                "Dotdash"  = "dotdash",
                                                                "Longdash" = "longdash"),
                                                    selected = "dotdash")
                          ),
                          mainPanel(width = 9,
                                    plotly::plotlyOutput("rv_output", height = "80vh")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI MISS COR ───────────────────────────────────────────────────────────────
               
               tabPanel("Correlation",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Correlation: <br><br>
                                     <b>Variable Correlation</b>: Pairwise correlations between numeric variables. <br><br>
                                     <b>Missingness Correlation</b>: Variables converted to binary (1 = missing, 0 = present),
                                     then correlated. Only partially-missing variables are shown."),
                                       hr(),
                                       radioButtons("cor_view", "View:",
                                                    choices  = c("Variable Correlation"   = "varcor",
                                                                 "Missingness Correlation" = "misscor"),
                                                    selected = "varcor"),
                                       hr(),
                                       selectizeInput("cor_vars", "Variables to include:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       selectInput("cor_method", "Correlation method:",
                                                   choices  = c("Pearson"  = "pearson",
                                                                "Spearman" = "spearman",
                                                                "Kendall"  = "kendall"),
                                                   selected = "spearman"),
                                       hr(),
                                       selectInput("cor_order", "Variable ordering:",
                                                   choices  = c("OLO (Optimal Leaf)" = "OLO",
                                                                "AOE (Eigenvector)"  = "TRUE",
                                                                "HC (Hierarchical)"  = "HC",
                                                                "Original"           = "FALSE"),
                                                   selected = "OLO"),
                                       hr(),
                                       checkboxInput("cor_abs", "Absolute correlation", value = TRUE)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("cor_output", height = "85vh")
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
                                                   choices = NULL),
                                       hr(),
                                       selectizeInput("mt_vars", "Variables to include as predictors:",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(placeholder = "All variables included by default"))
                          ),
                          mainPanel(width = 9,
                                    plotOutput("mt_output", height = "85vh")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI MISS IMPORTANCE ────────────────────────────────────────────────────────
               
               tabPanel("Miss Importance",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Miss Importance: <br><br>
                                       Variable importance for predicting the number of missing values per observation. <br><br>
                                       If <b>any bars appear</b>, MCAR is ruled out — missingness is likely MAR or MNAR. <br><br>
                                       <b style='color:#36b5a2;'>Teal</b> bars are interesting: <br><br>
                                       &nbsp;• <b>Response variable (y)</b> appearing suggests <b>informative missingness</b> —
                                       the fact that values are missing is predictive of y.
                                       Create shadow variables before imputing. <br><br>
                                       &nbsp;• <b>Row index</b> appearing suggests <b>sequential missingness</b> —
                                       needs domain knowledge to explain."),
                                       hr(),
                                       selectInput("mi_id_col", "ID column (converted to numeric order):",
                                                   choices = NULL),
                                       hr(),
                                       selectInput("mi_response_col", "Response variable (y):",
                                                   choices = NULL)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("mi_output", height = "85vh")
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
               
               
               # ── UI TABPLOT ────────────────────────────────────────────────────────────────
               
               tabPanel("Tabplot",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Tabplot: <br><br>
                        Visualises distributions of multiple variables simultaneously,
                        sorted by a target variable. Useful for spotting patterns
                        and how variables relate to the outcome."),
                                       hr(),
                                       selectizeInput("tp_vars", "Variables to plot:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       checkboxInput("tp_sort_on", "Sort by variable", value = TRUE),
                                       conditionalPanel(
                                         condition = "input.tp_sort_on == true",
                                         selectInput("tp_sortvar", "Sort by variable:", choices = NULL),
                                         checkboxInput("tp_decreasing", "Sort descending", value = FALSE)
                                       ),
                                       hr(),
                                       radioButtons("tp_transform", "Transform numeric columns:",
                                                    choices = c("None"        = "none",
                                                                "Centre"      = "center",
                                                                "Standardise" = "standardise",
                                                                "Normalise"   = "normalise"),
                                                    selected = "normalise"),
                                       hr(),
                                       sliderInput("tp_nbin", "Number of bins:",
                                                   min = 10, max = 500, value = 60, step = 10, width = "100%")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("tp_output", height = "80vh")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI MOSAIC ─────────────────────────────────────────────────────────────────
               
               tabPanel("Mosaic",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Mosaic: <br><br>
                        Visualise dependency between categorical variables.
                        Tile area reflects joint frequency; shading by residuals
                        reveals where observed counts deviate from independence."),
                                       hr(),
                                       sliderInput("mosaic_max_levels", "Max levels per variable:",
                                                   min = 2, max = 20, value = 8, step = 1, width = "100%"),
                                       hr(),
                                       uiOutput("mosaic_x_ui"),
                                       uiOutput("mosaic_y_ui"),
                                       uiOutput("mosaic_z_ui"),
                                       hr(),
                                       checkboxInput("mosaic_shade", "Shade by residuals", value = TRUE),
                                       hr(),
                                       sliderInput("mosaic_rot_labels", "Rotate Variable 1 labels:",
                                                   min = 0, max = 360, value = 0, step = 15, width = "100%"),
                                       checkboxInput("mosaic_abbreviate", "Abbreviate labels", value = TRUE),
                                       sliderInput("mosaic_fontsize", "Label font size:",
                                                   min = 6, max = 24, value = 10, step = 1, width = "100%")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("mosaic_output", height = "80vh")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI GGPAIRS ────────────────────────────────────────────────────────────────
               
               tabPanel("GGPairs",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("GGPairs: <br><br>
                        Pairwise relationships, correlations, and marginal distributions
                        across multiple variables. Useful for detecting suspicious
                        relationships and structural irregularities early."),
                                       hr(),
                                       selectizeInput("gg_vars", "Variables to plot:",
                                                      choices  = NULL,
                                                      multiple = TRUE),
                                       hr(),
                                       checkboxInput("gg_group_on", "Group by categorical variable", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.gg_group_on == true",
                                         selectInput("gg_group_var", "Grouping variable:", choices = NULL),
                                         selectizeInput("gg_group_levels", "Levels to include:",
                                                        choices  = NULL,
                                                        multiple = TRUE,
                                                        options  = list(placeholder = "All levels included by default"))
                                       ),
                                       hr(),
                                       actionButton("gg_run", "Plot", icon = icon("play"), width = "100%"),
                                       helpText("Large selections may be slow.")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("gg_output", height = "80vh")
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
      apply_shadow_variables(input$strat_shadow_cols)            |>
      apply_not_applicable_strategy(get_na_rules())              |>
      apply_col_threshold(input$strat_var_thresh)                |>
      apply_row_threshold(input$strat_obs_thresh)                |>
      apply_knn_imputation(
        knn_response = input$strat_knn_response,
        knn_cols     = input$strat_knn_cols,
        knn_k        = input$strat_knn_k
      )                                                          |>
      apply_mmm_imputation(
        mean_cols   = input$strat_mmm_mean_cols,
        median_cols = input$strat_mmm_median_cols,
        mode_cols   = input$strat_mmm_mode_cols
      )
  })
  
  get_ms_data_plot <- reactive({
    req(input$ms_vars)
    df <- get_data()
    
    if (isTRUE(input$ms_sort_by_col_on) && !is.null(input$ms_sort_by_col) &&
        input$ms_sort_by_col %in% names(df)) {
      df <- df[order(df[[input$ms_sort_by_col]]), , drop = FALSE]
    }
    
    if (!is.null(input$ms_downsample) && input$ms_downsample < nrow(df)) {
      samp <- sort(sample(1:nrow(df), input$ms_downsample))
      df   <- df[samp, , drop = FALSE]
    }
    
    df[, input$ms_vars, drop = FALSE]
  })
  
  
  # ── SERVER EDA DATA ROLES ──────────────────────────────────────────────────
  
  role_choices <- c("Predictor", "Outcome", "Observation ID",
                    "Train-Test Split", "Case Weight",
                    "Stratifier", "Sensitive", "Ignore")
  
  role_colors <- c(
    "Predictor"        = "#4a80d4",
    "Outcome"          = "#2a9d8f",
    "Observation ID"   = "#7c5cbf",
    "Train-Test Split" = "#e07b39",
    "Case Weight"      = "#3aaf85",
    "Stratifier"       = "#5a4fcf",
    "Sensitive"        = "#c2537a",
    "Ignore"           = "#868e96"
  )
  
  roles_rv <- reactiveVal(NULL)
  
  observe({
    df      <- get_raw()
    current <- roles_rv()
    new_roles <- setNames(rep("Predictor", ncol(df)), names(df))
    if (!is.null(current)) {
      shared <- intersect(names(new_roles), names(current))
      new_roles[shared] <- current[shared]
    }
    roles_rv(new_roles)
  })
  
  observeEvent(input$roles_reset, {
    df <- get_raw()
    roles_rv(setNames(rep("Predictor", ncol(df)), names(df)))
  })
  
  output$roles_buckets_ui <- renderUI({
    roles <- roles_rv()
    req(roles)
    
    buckets <- lapply(role_choices, function(role) {
      vars_in_role <- names(roles)[roles == role]
      color        <- role_colors[[role]]
      
      div(
        style = "margin-bottom: 16px;",
        div(
          style = paste0(
            "font-weight: 600; font-size: 13px; color: white;",
            "background-color:", color, ";",
            "padding: 6px 10px; border-radius: 6px 6px 0 0;"
          ),
          role
        ),
        sortable_js(paste0("bucket_", gsub(" ", "_", role)),
                    options = sortable_options(
                      group     = list(name = "roles", pull = TRUE, put = TRUE),
                      onEnd     = htmlwidgets::JS(sprintf("
                      function(evt) {
                        var allBuckets = {};
                        %s.forEach(function(r) {
                          var id = 'bucket_' + r.replace(/ /g, '_');
                          var el = document.getElementById(id);
                          if (el) {
                            var items = Array.from(el.children).map(function(c) {
                              return c.getAttribute('data-var');
                            });
                            allBuckets[r] = items;
                          }
                        });
                        Shiny.setInputValue('roles_drag_update', allBuckets, {priority: 'event'});
                      }
                    ", jsonlite::toJSON(role_choices)))
                    )
        ),
        div(
          id    = paste0("bucket_", gsub(" ", "_", role)),
          style = paste0(
            "min-height: 40px; padding: 6px;",
            "border: 2px dashed ", color, ";",
            "border-top: none;",
            "border-radius: 0 0 6px 6px;",
            "background: #f8f9fa;",
            "display: flex; flex-wrap: wrap; gap: 6px;"
          ),
          lapply(vars_in_role, function(v) {
            div(
              `data-var` = v,
              style = paste0(
                "padding: 4px 10px; border-radius: 4px; cursor: grab;",
                "background-color: ", color, ";",
                "color: white; font-size: 12px; font-weight: 500;",
                "user-select: none;"
              ),
              v
            )
          })
        )
      )
    })
    
    do.call(tagList, buckets)
  })
  
  observeEvent(input$roles_drag_update, {
    update <- input$roles_drag_update
    new_roles <- roles_rv()
    for (role in names(update)) {
      for (v in update[[role]]) {
        if (v %in% names(new_roles)) new_roles[[v]] <- role
      }
    }
    roles_rv(new_roles)
  })
  
  # role helper
  
  get_roles          <- reactive({ roles_rv() })
  get_outcome_col    <- reactive({ names(Filter(function(r) r == "Outcome",           get_roles())) })
  get_predictor_cols <- reactive({ names(Filter(function(r) r == "Predictor",         get_roles())) })
  get_id_col         <- reactive({ names(Filter(function(r) r == "Observation ID",    get_roles())) })
  get_split_col      <- reactive({ names(Filter(function(r) r == "Train-Test Split",  get_roles())) })
  
  
  # ── SERVER EDA DATATABLE ───────────────────────────────────────────────────
  
  output$data_table <- renderDT({
    df_raw   <- get_raw()
    df_clean <- get_data()
    mode     <- input$dt_view_mode
    
    if (mode == "raw") {
      return(datatable(df_raw, options = list(
        pageLength  = 20,
        scrollX     = TRUE,
        columnDefs  = list(list(className = "dt-left", targets = "_all"))
      )))
    }
    
    shared_cols <- intersect(names(df_raw),    names(df_clean))
    shared_rows <- intersect(rownames(df_raw), rownames(df_clean))
    
    mask <- matrix(FALSE, nrow = length(shared_rows), ncol = length(shared_cols),
                   dimnames = list(shared_rows, shared_cols))
    
    display <- df_clean[shared_rows, , drop = FALSE]
    display[shared_cols] <- lapply(shared_cols, function(col) {
      as.character(df_clean[shared_rows, col])
    })
    
    for (col in shared_cols) {
      r_char  <- as.character(df_raw[shared_rows,   col])
      c_char  <- as.character(df_clean[shared_rows, col])
      r_na    <- is.na(df_raw[shared_rows,   col])
      c_na    <- is.na(df_clean[shared_rows, col])
      changed <- (r_na != c_na) | (!r_na & !c_na & r_char != c_char)
      mask[, col]    <- changed
      display[[col]] <- if (mode == "hints") {
        ifelse(changed, paste0(ifelse(r_na, "NA", r_char), " \u2192 ", ifelse(c_na, "NA", c_char)), c_char)
      } else {
        c_char
      }
    }
    
    if (mode == "current") {
      return(datatable(display, options = list(pageLength = 20, scrollX = TRUE)))
    }
    make_hints_dt(df_raw, df_clean, page_length = 20)
  }, server = FALSE)
  
  
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
  
  # populate option to sort by a col
  observe({
    df <- get_data()
    updateSelectInput(session, "ms_sort_by_col", choices = names(df))
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
  
  
  # ── SERVER EDA RISING VALUE ───────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "rv_vars",
                         choices  = num_cols,
                         selected = num_cols,
                         server   = TRUE)
  })
  
  output$rv_output <- plotly::renderPlotly({
    req(input$rv_vars)
    df       <- get_data()
    sel_vars <- intersect(input$rv_vars, names(df)[sapply(df, is.numeric)])
    req(length(sel_vars) > 0)
    
    plot_rising_value(
      df        = df,
      vars      = sel_vars,
      transform = input$rv_transform,
      omit_na   = isTRUE(input$rv_omit_na),
      lwd       = input$rv_lwd,
      lty       = input$rv_lty
    )
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
  
  
  # ── SERVER EDA COR HEATMAP ────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    # var cor needs numeric only; miss cor can use all cols — default to all
    updateSelectizeInput(session, "cor_vars",
                         choices  = names(df),
                         selected = names(df),
                         server   = TRUE)
  })
  
  output$cor_output <- renderPlot({
    req(input$cor_vars)
    df  <- get_data()
    sel <- intersect(input$cor_vars, names(df))
    df  <- df[, sel, drop = FALSE]
    
    if (input$cor_view == "misscor") {
      plot_miss_cor(df, input$cor_order, isTRUE(input$cor_abs), input$cor_method)
    } else {
      plot_var_cor(df, names(df), input$cor_method, input$cor_order, isTRUE(input$cor_abs))
    }
  })
  
  
  # ── SERVER EDA MISS TREE ──────────────────────────────────────────────────────
  
  observe({
    df      <- get_data()
    id_col  <- get_id_col()
    updateSelectInput(session, "mt_id_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    updateSelectizeInput(session, "mt_vars",
                         choices  = names(df),
                         selected = names(df),
                         server   = TRUE)
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
    
    plot_miss_tree(df, input$mt_id_col, input$mt_vars)
  }, res = 96, height = 700)
  
  
  # ── SERVER EDA MISS IMPORTANCE ────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    id_col   <- get_id_col()
    resp_col <- get_outcome_col()
    updateSelectInput(session, "mi_id_col",
                      choices  = names(df),
                      selected = if (length(id_col)   > 0) id_col[1]   else names(df)[1])
    updateSelectInput(session, "mi_response_col",
                      choices  = c("(none)", names(df)),
                      selected = if (length(resp_col) > 0) resp_col[1] else "(none)")
  })
  
  output$mi_output <- renderPlot({
    req(input$mi_id_col)
    df <- get_data()
    
    has_miss <- sapply(df, anyNA)
    if (!any(has_miss)) {
      plot.new()
      text(0.5, 0.5, "No missing values in dataset — importance not applicable.",
           cex = 1.2, col = "#C41E3A", adj = 0.5)
      return()
    }
    
    plot_miss_importance(df, input$mi_id_col, input$mi_response_col)
  })
  
  
  # ── SERVER EDA BOXPLOT ────────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    id_col   <- get_id_col()
    updateSelectInput(session, "bp_var",       choices = num_cols)
    updateSelectInput(session, "bp_label_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
  })
  
  output$bp_output <- renderPlot({
    req(input$bp_var, input$bp_label_col, input$bp_iqr_k)
    plot_boxplot(get_data(), input$bp_var, input$bp_label_col, input$bp_iqr_k)
  })
  
  output$bp_outliers_table <- renderDT({
    req(input$bp_var, input$bp_iqr_k)
    make_hints_dt(get_raw(), get_boxplot_outliers(get_data(), input$bp_var, input$bp_iqr_k))
  }, server = FALSE)
  
  
  # ── SERVER EDA BAGPLOT ────────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    id_col   <- get_id_col()
    updateSelectInput(session, "bag_x_var",     choices = num_cols)
    updateSelectInput(session, "bag_y_var",     choices = num_cols)
    updateSelectInput(session, "bag_label_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
  })
  
  output$bag_output <- renderPlot({
    req(input$bag_x_var, input$bag_y_var, input$bag_label_col, input$bag_k)
    plot_bagplot(get_data(), input$bag_x_var, input$bag_y_var, input$bag_label_col, input$bag_k)
  })
  
  output$bag_outliers_table <- renderDT({
    req(input$bag_x_var, input$bag_y_var, input$bag_k)
    make_hints_dt(get_raw(), get_bagplot_outliers(get_data(), input$bag_x_var, input$bag_y_var, input$bag_k))
  }, server = FALSE)
  
  
  # ── SERVER EDA TABPLOT ────────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    all_cols <- names(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    date_cols <- names(df)[sapply(df, inherits, what = "Date")]
    
    updateSelectizeInput(session, "tp_vars",
                         choices  = all_cols,
                         selected = all_cols,
                         server   = TRUE)
    
    sortable_vars <- c(date_cols, num_cols)
    updateSelectInput(session, "tp_sortvar",
                      choices  = sortable_vars,
                      selected = if (length(sortable_vars) > 0) sortable_vars[1] else NULL)
  })
  
  output$tp_output <- renderPlot({
    req(input$tp_vars)
    plot_tabplot(
      df         = get_data(),
      vars       = input$tp_vars,
      sort_on    = isTRUE(input$tp_sort_on),
      sort_var   = input$tp_sortvar,
      sort_desc  = isTRUE(input$tp_decreasing),
      transform  = input$tp_transform,
      nbin       = input$tp_nbin
    )
  })
  
  
  # ── SERVER EDA MOSAIC ─────────────────────────────────────────────────────────
  
  mosaic_cat_cols <- reactive({
    df       <- get_data()
    max_lvls <- input$mosaic_max_levels
    req(max_lvls)
    cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    cols[sapply(cols, function(v) length(unique(na.omit(df[[v]]))) <= max_lvls)]
  })
  
  output$mosaic_x_ui <- renderUI({
    cols <- mosaic_cat_cols()
    selectInput("mosaic_x", "Variable 1 (columns):", choices = cols,
                selected = if (length(cols) >= 1) cols[1] else NULL)
  })
  
  output$mosaic_y_ui <- renderUI({
    cols <- mosaic_cat_cols()
    selectInput("mosaic_y", "Variable 2 (rows):", choices = cols,
                selected = if (length(cols) >= 2) cols[2] else cols[1])
  })
  
  output$mosaic_z_ui <- renderUI({
    cols <- mosaic_cat_cols()
    selectInput("mosaic_z", "Variable 3 — optional (sub-rows):",
                choices = c("None", cols), selected = "None")
  })
  
  output$mosaic_output <- renderPlot({
    req(input$mosaic_x, input$mosaic_y, input$mosaic_z)
    plot_mosaic(
      df          = get_data(),
      x_var       = input$mosaic_x,
      y_var       = input$mosaic_y,
      z_var       = input$mosaic_z,
      shade       = isTRUE(input$mosaic_shade),
      rot_labels  = input$mosaic_rot_labels,
      abbreviate  = isTRUE(input$mosaic_abbreviate),
      fontsize    = input$mosaic_fontsize
    )
  })
  
  
  # ── SERVER EDA GGPAIRS ────────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    all_cols <- names(df)
    cat_cols <- names(df)[sapply(df, is.factor)]
    updateSelectizeInput(session, "gg_vars",
                         choices  = all_cols,
                         selected = all_cols[1:min(5, length(all_cols))],
                         server   = TRUE)
    updateSelectInput(session, "gg_group_var", choices = cat_cols)
  })
  
  observeEvent(input$gg_group_var, {
    req(input$gg_group_var)
    lvls <- levels(get_data()[[input$gg_group_var]])
    updateSelectizeInput(session, "gg_group_levels", choices = lvls, selected = lvls)
  })
  
  output$gg_output <- renderPlot({
    input$gg_run
    isolate({
      req(input$gg_vars)
      validate(need(length(input$gg_vars) >= 2, "Select at least 2 variables."))
      plot_ggpairs(
        df           = get_data(),
        vars         = input$gg_vars,
        group_on     = isTRUE(input$gg_group_on),
        group_var    = input$gg_group_var,
        group_levels = input$gg_group_levels
      )
    })
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
    df       <- get_raw()
    resp_col <- get_outcome_col()
    cols     <- c("(none)", names(df))
    updateSelectInput(session, "strat_knn_response",
                      choices  = cols,
                      selected = if (length(resp_col) > 0) resp_col[1] else "(none)")
  })
  
  # when response changes, update imputable columns to exclude it
  observeEvent(input$strat_knn_response, {
    df   <- get_data()
    resp <- input$strat_knn_response
    eligible <- if (is.null(resp) || resp == "(none)") {
      names(df)
    } else {
      setdiff(names(df), resp)
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