# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(tidyverse)
library(sortable)
library(DT)
library(wordcloud)
library(RColorBrewer)
library(ipred)
library(scales)
library(plotly)
library(tabplot)
library(GGally)
library(vcd)
library(rpart.plot)
library(MASS)
library(bestNormalize)
library(recipes)
library(e1071)
library(isotree)
library(glmnet)
library(caret)


# ── DATA INITIALISATION ──────────────────────────────────────────────────────

raw_dataset <- read.csv('Ass2Data.csv', header = TRUE, na.strings = c('NA', 'N/A'), stringsAsFactors = TRUE)
# raw_dataset <- read.csv('Ass20Data.csv', header = TRUE, na.strings = c('NA', 'N/A'), stringsAsFactors = TRUE)

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

plot_wordcloud <- function(df, mode, wc_var, include_numeric, case_sensitive,
                           split_mode, max_words, min_freq, scale_val, palette) {
  val <- switch(mode,
                "varnames" = names(df),
                "allvals"  = unlist(lapply(names(df), function(col) as.character(df[[col]]))),
                "catvar" = unlist(lapply(wc_var, function(v) as.character(df[[v]])))
  )
  val <- val[!is.na(val) & nchar(trimws(val)) > 0]
  if (!case_sensitive) val <- tolower(val)
  if (length(val) == 0) stop("No non-missing values to display.")
  tokens <- switch(split_mode,
                   "none"     = val,
                   "chars"    = { t <- unlist(strsplit(val, "")); t[!grepl("\\s", t)] },
                   "alphanum" = {
                     t <- unlist(lapply(val, function(tok) {
                       m <- gregexpr("[A-Za-z]+|[0-9]+", tok, perl = TRUE)
                       regmatches(tok, m)[[1]]
                     }))
                     t[nchar(t) > 0]
                   }
  )
  freq_table <- sort(table(tokens), decreasing = TRUE)
  freq_table <- freq_table[freq_table >= min_freq]
  if (length(freq_table) == 0) stop(paste0("No tokens appear at least ", min_freq, " times."))
  freq_table <- head(freq_table, max_words)
  words      <- names(freq_table)
  freqs      <- as.integer(freq_table)
  n          <- length(words)
  pal        <- RColorBrewer::brewer.pal(max(3, min(8, n)), palette)
  colors     <- colorRampPalette(pal)(n)[rank(-freqs, ties.method = "first")]
  freq_ratio <- max(freqs) / max(median(freqs), 1)
  if (freq_ratio >= 3) {
    freqs_norm <- as.integer(20 + (sqrt(freqs) - sqrt(min(freqs))) /
                               max(sqrt(max(freqs)) - sqrt(min(freqs)), 1) * 80)
  } else {
    freq_ranks <- rank(-freqs, ties.method = "first")
    freqs_norm <- as.integer(100 - (freq_ranks - 1) / max(freq_ranks - 1, 1) * 80)
  }
  n_eff      <- min(n, 30)
  base_scale <- max(3, min(9, 40 / sqrt(n_eff)))
  max_scale  <- min(12, base_scale * (1 + log10(max(freq_ratio, 1)))) * scale_val
  min_scale  <- max(0.3, max_scale * 0.15)
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
}

# plot barchart graph
plot_barchart <- function(df, vars, top_n, include_na, group_on, group_var, stack_vars = FALSE, custom_title = NULL) {
  
  var_label <- if (length(vars) == 1) vars else paste0(length(vars), " variables")
  
  # build long-format df tracking which variable each value came from
  long_df <- do.call(rbind, lapply(vars, function(v) {
    data.frame(value = as.character(df[[v]]), variable = v, stringsAsFactors = FALSE)
  }))
  
  if (include_na) {
    long_df$value[is.na(long_df$value)] <- "(NA)"
  } else {
    long_df <- long_df[!is.na(long_df$value), ]
  }
  
  counts_all <- sort(table(long_df$value), decreasing = TRUE)
  top_levels <- names(head(counts_all, top_n))
  long_df    <- long_df[long_df$value %in% top_levels, ]
  long_df$value <- factor(long_df$value, levels = top_levels)
  
  if (stack_vars && length(vars) > 1) {
    long_df$variable <- factor(long_df$variable, levels = vars)
    fill_vals <- scales::hue_pal(l = 55, c = 40)(length(vars))
    
    default_title <- paste("Bar Chart for", var_label)
    plot_title <- if (!is.null(custom_title) && nzchar(custom_title)) custom_title else default_title
    
    ggplot(long_df, aes(x = value, fill = variable)) +
      geom_bar(position = "stack") +
      geom_text(
        stat = "count",
        aes(label = ..count..),
        position = position_stack(vjust = 0.5),
        size = 4.5,
        fontface = "bold",
        check_overlap = TRUE
      ) +
      scale_fill_manual(values = setNames(fill_vals, vars)) +
      labs(title = plot_title,
           x = "Value", y = "Count", fill = "Variable") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5,
                                  margin = margin(b = 12)),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text  = element_text(size = 12)
      )
    
  } else if (group_on) {
    grp_vec <- do.call(rbind, lapply(vars, function(v) {
      data.frame(value = as.character(df[[v]]), grp = as.character(df[[group_var]]),
                 stringsAsFactors = FALSE)
    }))
    if (!include_na) grp_vec <- grp_vec[!is.na(grp_vec$value), ]
    grp_vec <- grp_vec[grp_vec$value %in% top_levels, ]
    grp_vec$value <- factor(grp_vec$value, levels = top_levels)
    grp_vec <- grp_vec[!is.na(grp_vec$grp), ]
    n_levels  <- length(unique(grp_vec$grp))
    fill_vals <- viridis::viridis(length(vars), option = "C", begin = 0.1, end = 0.9)
    ggplot(grp_vec, aes(x = value, fill = grp)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = fill_vals) +
      labs(title = paste("Bar Chart for", var_label, "grouped by", group_var),
           x = "Value", y = "Count", fill = group_var) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    plot_df <- data.frame(
      level = factor(names(table(long_df$value)[top_levels]),  levels = top_levels),
      count = as.integer(table(long_df$value)[top_levels])
    )
    ggplot(plot_df, aes(x = level, y = count)) +
      geom_col() +
      geom_text(aes(label = count), vjust = -0.4, fontface = "bold", size = 4.5) +
      labs(title = paste("Bar Chart for", var_label), x = "Value", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
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
plot_corrgram <- function(cor_mat, order, title = NULL, label_cex = 0.9) {
  par(oma = c(0, 0, 3, 0))
  corrgram::corrgram(
    cor_mat,
    order      = if (order == "FALSE") FALSE else order,
    abs        = FALSE,
    cex.labels = label_cex,
    font.labels = 2        # ← 2 = bold
  )
  if (!is.null(title)) {
    title(main = title, cex.main = 1.8, font.main = 2, outer = TRUE, line = 1)
  }
}

# plot missingness correlation
plot_miss_cor <- function(df, order, abs_cor, method = "pearson", label_cex = 0.9) {
  
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
                     if (abs_cor) " | Absolute" else ""),
    label_cex = label_cex
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
plot_ggpairs <- function(df, vars, group_on, group_var, group_levels, fontsize = 14, cardinality_threshold = 15) {
  
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
      theme_minimal(base_size = fontsize) +
      theme(plot.title   = element_text(size = fontsize + 6, face = "bold", hjust = 0.5),
            strip.text.x = element_text(size = fontsize, face = "bold"),
            strip.text.y = element_text(size = fontsize, face = "bold", angle = 0),
            axis.text.x  = element_text(size = fontsize, angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y  = element_text(size = fontsize),
            legend.title = element_text(face = "bold"),
            legend.text  = element_text(size = fontsize - 2)) +
      labs(title = paste0("GGPairs | Grouped by ", group_var))
    
  } else {
    plot_df <- df[, sel, drop = FALSE]
    GGally::ggpairs(
      plot_df, progress = FALSE,
      upper = list(continuous = GGally::wrap("cor", size = 4)),
      lower = list(continuous = GGally::wrap("points", alpha = 0.4, size = 0.8)),
      diag  = list(continuous = GGally::wrap("densityDiag"))
    ) +
      theme_minimal(base_size = fontsize) +
      theme(plot.title   = element_text(size = fontsize + 6, face = "bold", hjust = 0.5),
            strip.text.x = element_text(size = fontsize, face = "bold"),
            strip.text.y = element_text(size = fontsize, face = "bold", angle = 0),
            axis.text.x  = element_text(size = fontsize, angle = 90, vjust = 0.5, hjust = 1),
            axis.text.y  = element_text(size = fontsize),
            legend.title = element_text(face = "bold"),
            legend.text  = element_text(size = fontsize - 2)) +
      labs(title = "GGPairs")
  }
}

# plot variable correlation heatmap
plot_var_cor <- function(df, vars, method, order, abs_cor, label_cex = 0.9) {
  
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
                     if (abs_cor) " | Absolute" else ""),
    label_cex = label_cex
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

# for column-specific false negative NAs
apply_col_missingness_strategy <- function(df, rules) {
  for (rule in rules) {
    col <- rule$col
    val <- trimws(rule$val)
    if (is.null(col) || col == "(none)" || is.null(val) || val == "") next
    if (!col %in% names(df)) next
    
    if (is.numeric(df[[col]])) {
      num_val <- suppressWarnings(as.numeric(val))
      if (!is.na(num_val))
        df[[col]][df[[col]] == num_val] <- NA
    } else if (is.factor(df[[col]])) {
      x <- as.character(df[[col]])
      x[x == val] <- NA
      df[[col]] <- factor(x, levels = levels(df[[col]]))
    } else {
      df[[col]][df[[col]] == val] <- NA
    }
  }
  df
}

# for false positive NAs
apply_not_applicable_strategy <- function(df, rules) {
  for (rule in rules) {
    if (is.null(rule$target_col) || rule$target_col == "(none)") next
    if (is.null(rule$cond_col)   || rule$cond_col   == "(none)") next
    if (is.null(rule$cond_val)   || rule$cond_val   == "")       next
    
    mask <- if (rule$cond_val == "NA") {
      is.na(df[[rule$cond_col]]) & is.na(df[[rule$target_col]])
    } else {
      !is.na(df[[rule$cond_col]]) &
        as.character(df[[rule$cond_col]]) == rule$cond_val &
        is.na(df[[rule$target_col]])
    }
    
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
apply_knn_imputation <- function(df, knn_response, knn_cols, knn_k, method = "knn") {
  if (is.null(knn_response) || knn_response == "" || knn_response == "(none)") return(df)
  if (is.null(knn_cols) || length(knn_cols) == 0) return(df)
  if (!knn_response %in% names(df)) return(df)
  
  # only validate k when actually needed
  if (method != "bag" && (is.null(knn_k) || is.na(knn_k) || knn_k < 1)) return(df)
  
  cols_present <- intersect(knn_cols, setdiff(names(df), knn_response))
  if (length(cols_present) == 0) return(df)
  
  df_pred <- df[, setdiff(names(df), knn_response), drop = FALSE]
  
  rec <- recipes::recipe(~ ., data = df_pred)
  
  if (method == "bag") {
    rec <- rec |> recipes::step_impute_bag(tidyselect::all_of(cols_present))
  } else {
    rec <- rec |> recipes::step_impute_knn(tidyselect::all_of(cols_present), neighbors = knn_k)
  }
  
  imputed <- rec |>
    recipes::prep(training = df_pred) |>
    recipes::bake(new_data = df_pred)
  
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

# for centre / scale transformation
apply_centre_scale <- function(df, centre_cols, scale_cols) {
  for (col in centre_cols) {
    if (!col %in% names(df) || !is.numeric(df[[col]])) next
    df[[col]] <- df[[col]] - mean(df[[col]], na.rm = TRUE)
  }
  for (col in scale_cols) {
    if (!col %in% names(df) || !is.numeric(df[[col]])) next
    sd_val <- sd(df[[col]], na.rm = TRUE)
    if (!is.na(sd_val) && sd_val > 0) df[[col]] <- df[[col]] / sd_val
  }
  df
}

# apply recipes-based power transform (Box-Cox or Yeo-Johnson) to selected cols
apply_power_transform_strategy <- function(df, cols, method) {
  if (is.null(cols) || length(cols) == 0 || method == "none") return(df)
  
  cols_present <- intersect(cols, names(df))
  num_cols     <- cols_present[sapply(cols_present, function(v) is.numeric(df[[v]]))]
  if (length(num_cols) == 0) return(df)
  
  tryCatch({
    rec <- recipes::recipe(~ ., data = df[, num_cols, drop = FALSE])
    
    if (method == "boxcox") {
      # Box-Cox only works on strictly positive columns — skip others silently
      pos_cols <- num_cols[sapply(num_cols, function(v) {
        all(df[[v]] > 0, na.rm = TRUE)
      })]
      if (length(pos_cols) == 0) return(df)
      rec <- rec |> recipes::step_BoxCox(tidyselect::all_of(pos_cols))
      num_cols <- pos_cols
    } else if (method == "yeojohnson") {
      rec <- rec |> recipes::step_YeoJohnson(tidyselect::all_of(num_cols))
    }
    
    prepped <- recipes::prep(rec, training = df[, num_cols, drop = FALSE])
    baked   <- recipes::bake(prepped, new_data = NULL)
    
    for (col in intersect(names(baked), names(df))) {
      df[[col]] <- baked[[col]]
    }
    df
  }, error = function(e) {
    message("apply_power_transform_strategy error: ", conditionMessage(e))
    df
  })
}

# apply power transform for visualisation only (never modifies df)
apply_power_transform <- function(x, method) {
  if (method == "none" || all(is.na(x))) return(x)
  tryCatch({
    if (method == "boxcox") {
      # Box-Cox requires strictly positive values
      if (any(x <= 0, na.rm = TRUE)) stop("Box-Cox requires all values > 0")
      est <- MASS::boxcox(x ~ 1, plotit = FALSE)
      lam <- est$x[which.max(est$y)]
      if (abs(lam) < 1e-6) log(x) else (x^lam - 1) / lam
    } else if (method == "yeojohnson") {
      obj <- bestNormalize::yeojohnson(x, standardize = FALSE)
      predict(obj)
    }
  }, error = function(e) {
    warning("Transform failed: ", conditionMessage(e))
    x   # return untransformed on failure
  })
}

# plot mahalanobis distance
plot_mahalanobis <- function(df, predictor_cols, id_col, threshold_p) {
  
  num_cols <- predictor_cols[sapply(predictor_cols, function(v) is.numeric(df[[v]]))]
  
  if (length(num_cols) < 2) {
    plot.new()
    text(0.5, 0.5, "Need at least 2 numeric predictor columns.",
         cex = 1.2, col = "#C41E3A", adj = 0.5)
    return(invisible(NULL))
  }
  
  df_num <- df[, num_cols, drop = FALSE]
  n_before <- nrow(df_num)
  
  # always remove rows with any NA before mahalanobis
  df_num <- df_num[complete.cases(df_num), , drop = FALSE]
  n_after <- nrow(df_num)
  
  if (n_after < 3) {
    plot.new()
    text(0.5, 0.5, paste0(
      "Only ", n_after, " complete rows remain after removing NAs\n",
      "(", n_before - n_after, " rows removed).\n",
      "Apply imputation first, or select fewer columns."
    ), cex = 1.1, col = "#C41E3A", adj = 0.5)
    return(invisible(NULL))
  }
  
  processed <- tryCatch({
    rec <- recipes::recipe(~ ., data = df_num) |>
      recipes::step_nzv(recipes::all_predictors()) |>
      recipes::step_lincomb(recipes::all_numeric_predictors()) |>
      recipes::step_YeoJohnson(recipes::all_numeric_predictors()) |>
      recipes::prep(training = df_num) |>
      recipes::bake(new_data = NULL)
    rec
  }, error = function(e) {
    message("Mahalanobis recipe error: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(processed) || ncol(processed) < 2) {
    plot.new()
    text(0.5, 0.5, paste0(
      "Recipe reduced columns to ", 
      if (is.null(processed)) 0 else ncol(processed),
      ".\nCheck for near-zero variance or linearly dependent variables."
    ), cex = 1.1, col = "#C41E3A", adj = 0.5)
    return(invisible(NULL))
  }
  
  # row names of processed match rows of df_num
  orig_row_names <- rownames(df_num)
  orig_rows      <- match(orig_row_names, rownames(df))
  id_labels      <- as.character(df[[id_col]])[orig_rows]
  
  Covar     <- var(processed)
  Means     <- colMeans(processed)
  md2       <- mahalanobis(x = processed, center = Means, cov = Covar)
  names(md2) <- id_labels
  
  threshold    <- qchisq(p = threshold_p, df = ncol(processed))
  label        <- ifelse(md2 > threshold, id_labels, NA_character_)
  Observations <- ifelse(md2 > threshold, "outlier", "non-outlier")
  id           <- seq_along(md2) / length(md2)
  n_out        <- sum(md2 > threshold)
  
  plot_df <- data.frame(md2, label, id, Observations)
  
  ggplot(plot_df, aes(y = md2, x = id)) +
    geom_point(aes(colour = Observations), size = 2) +
    ggrepel::geom_text_repel(aes(label = label), max.overlaps = 50,
                             size = 3.2, na.rm = TRUE) +
    scale_colour_manual(values = c("non-outlier" = "#4a80d4",
                                   "outlier"     = "#C41E3A")) +
    geom_hline(yintercept = threshold, colour = "black", linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 0.5, 1),
                       labels = c("0%", "50%", "100%")) +
    annotate("text", x = 0.01, y = threshold * 1.05,
             label = sprintf("χ²=%.1f  (%d outliers)", threshold, n_out),
             hjust = 0, size = 3.5, colour = "#333333") +
    labs(
      title = paste0(
        "Mahalanobis D² | p=", threshold_p,
        " | χ²(", ncol(processed), ")=", round(threshold, 1),
        " | n=", n_after, " complete rows"
      ),
      y = "Mahalanobis distance squared",
      x = "Complete observations"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title   = element_text(size = 13, face = "bold", hjust = 0.5),
      legend.title = element_text(face = "bold"),
      legend.text  = element_text(size = 12)
    )
}

get_mahalanobis_outliers <- function(df, predictor_cols, id_col, threshold_p) {
  num_cols <- predictor_cols[sapply(predictor_cols, function(v) is.numeric(df[[v]]))]
  if (length(num_cols) < 2) return(df[0, ])
  
  df_num    <- df[, num_cols, drop = FALSE]
  orig_rows <- which(complete.cases(df_num))
  df_num    <- df_num[orig_rows, , drop = FALSE]
  if (nrow(df_num) < 3) return(df[0, ])
  
  processed <- tryCatch({
    recipes::recipe(~ ., data = df_num) |>
      recipes::step_nzv(recipes::all_predictors()) |>
      recipes::step_lincomb(recipes::all_numeric_predictors()) |>
      recipes::step_YeoJohnson(recipes::all_numeric_predictors()) |>
      recipes::prep(training = df_num) |>
      recipes::bake(new_data = NULL)
  }, error = function(e) NULL)
  
  if (is.null(processed) || ncol(processed) < 2) return(df[0, ])
  
  Covar     <- var(processed)
  Means     <- colMeans(processed)
  md2       <- mahalanobis(x = processed, center = Means, cov = Covar)
  threshold <- qchisq(p = threshold_p, df = ncol(processed))
  
  df[orig_rows[md2 > threshold], , drop = FALSE]
}






# =================================================================================
# ui.R
# =================================================================================

ui <- fluidPage(
  
  # ·· HEADER ·································································
  
  fluidRow(
    column(10,
           div(
             style = "margin-top:20px; text-align:center; font-size:24px; font-weight:600; color:#495057;",
             "DATA423-26S1 Assignment 2 (EDA, Strategy, Model) \u2003 | \u2003 William Hui Chang (69051925)"
           )
    ),
    column(2,
           div(
             style = "margin-top:14px; text-align:right; padding-right:10px;",
             downloadButton(
               "download_csv",
               label = "Download CSV",
               icon  = icon("download"),
               style = "
               background-color: #2a9d8f;
               color: white;
               border: none;
               border-radius: 6px;
               font-weight: 600;
               font-size: 13px;
               padding: 8px 14px;
               box-shadow: 0 1px 3px rgba(0,0,0,0.15);
             "
             )
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
               
               
               # ── UI WORDCLOUD ──────────────────────────────────────────────────────────────
               
               tabPanel("Word Cloud",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Word Cloud: <br><br>
        Useful for spotting inconsistencies in variable names or values.
        Use <b>Check all values</b> mode to detect disguised NAs like
        <code>-99</code>, <code>--</code>, <code>?</code> appearing suspiciously often."),
                                       hr(),
                                       radioButtons("wc_mode", "Mode:",
                                                    choices = c(
                                                      "Categorical variable values" = "catvar",
                                                      "Check variable names"        = "varnames",
                                                      "Quick check for all values (character)" = "allvals"
                                                    ),
                                                    selected = "catvar"),
                                       hr(),
                                       conditionalPanel(
                                         condition = "input.wc_mode == 'catvar'",
                                         checkboxInput("wc_include_numeric",
                                                       "Switch to numeric variables (converted to text, not recommended)",
                                                       value = FALSE),
                                         selectizeInput("wc_var", "Variables:", choices = NULL, multiple = TRUE)
                                       ),
                                       hr(),
                                       checkboxInput("wc_case", "Case sensitive", value = TRUE),
                                       hr(),
                                       radioButtons("wc_split_mode", "Token split mode:",
                                                    choices = c(
                                                      "None (whole value)"    = "none",
                                                      "Individual characters" = "chars",
                                                      "Alpha / numeric runs"  = "alphanum"
                                                    ),
                                                    selected = "none"),
                                       hr(),
                                       sliderInput("wc_max_words", "Max words to show:",
                                                   min = 10, max = 500, value = 200, step = 10),
                                       sliderInput("wc_min_freq", "Min frequency:",
                                                   min = 1, max = 50, value = 1, step = 1),
                                       helpText("Font size is proportional to frequency."),
                                       hr(),
                                       sliderInput("wc_scale", "Plot scale:",
                                                   min = 0.3, max = 3.0, value = 1.0, step = 0.1),
                                       hr(),
                                       selectInput("wc_palette", "Colour palette:",
                                                   choices  = c("Dark2", "Set1", "Set2", "Set3",
                                                                "Paired", "Accent", "Spectral"),
                                                   selected = "Dark2")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("wc_plot", height = "80vh")
                          )
                        )
               ), # end tab panel
               
               
               # ── UI BARCHART ───────────────────────────────────────────────────────────────
               
               tabPanel("Bar Chart",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Bar Chart: <br><br>
                        Inspect the frequency of each level in a categorical or
                        discrete variable. Useful for spotting dominant categories,
                        rare levels, and unexpected values."),
                                       hr(),
                                       selectizeInput("bc_var", "Variables:", choices = NULL, multiple = TRUE),
                                       hr(),
                                       sliderInput("bc_top_n", "Levels to show:",
                                                   min = 1, max = 50, value = 8, step = 1, width = "100%"),
                                       hr(),
                                       checkboxInput("bc_include_na", "Include NA as a level", value = FALSE),
                                       hr(),
                                       checkboxInput("bc_group_on", "Group by categorical variable", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.bc_group_on == true",
                                         selectInput("bc_group_var", "Grouping variable:", choices = NULL)
                                       ),
                                       checkboxInput("bc_stack_vars", 
                                                     "Stack by variable name (when multiple selected)", value = FALSE),
                                       hr(),
                                       textInput("bc_title", "Custom plot title:", placeholder = "Auto-generated if empty")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("bc_output", height = "80vh")
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
               
               tabPanel("Heatmap",
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
                                       checkboxInput("cor_abs", "Absolute correlation", value = TRUE),
                                       hr(),
                                       sliderInput("cor_label_size", "Diagonal label size:",
                                                   min = 0.3, max = 2.5, value = 1.5, step = 0.1, width = "100%")
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
               
               
               # ── UI HISTOGRAM ──────────────────────────────────────────────────────────────
               
               tabPanel("Histogram",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Histogram: <br><br>
                        Inspect the shape of a numeric variable before using
                        boxplots. Heavy skew or multimodality means the default
                        IQR multiplier (1.5) may flag false outliers."),
                                       hr(),
                                       selectInput("hist_var", "Variable:", choices = NULL),
                                       hr(),
                                       sliderInput("hist_bins", "Number of bins:",
                                                   min = 5, max = 200, value = 20, step = 5, width = "100%"),
                                       hr(),
                                       checkboxInput("hist_tolerate_na",
                                                     "Ignore NAs",
                                                     value = TRUE),
                                       hr(),
                                       radioButtons("hist_transform", "Power transform (visual only):",
                                                    choices  = c("None"         = "none",
                                                                 "Box-Cox"      = "boxcox",
                                                                 "Yeo-Johnson"  = "yeojohnson"),
                                                    selected = "none"),
                                       conditionalPanel(
                                         condition = "input.hist_transform != 'none'",
                                         sidebar_note("Transform applied for display only —
                                         the dataset is not changed. <br><br>
                                         Box-Cox requires all values > 0.")
                                       )
                          ),
                          mainPanel(width = 9,
                                    plotOutput("hist_output", height = "60vh")
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
                                                    value = 1.5, min = 0.1, step = 0.5),
                                       hr(),
                                       checkboxInput("bp_tolerate_na", 
                                                     "Tolerate NAs (exclude NA rows for this variable only)", 
                                                     value = TRUE),
                                       hr(),
                                       radioButtons("bp_transform", "Power transform (visual only):",
                                                    choices  = c("None"         = "none",
                                                                 "Box-Cox"      = "boxcox",
                                                                 "Yeo-Johnson"  = "yeojohnson"),
                                                    selected = "none"),
                                       conditionalPanel(
                                         condition = "input.bp_transform != 'none'",
                                         sidebar_note("Transform applied for display only —
                                         the dataset is not changed. <br><br>
                                         Box-Cox requires all values > 0.")
                                       )
                          ),
                          mainPanel(width = 9,
                                    plotOutput("bp_output", height = "40vh"),
                                    hr(),
                                    uiOutput("bp_hint_ui"),
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
                                                    value = 3, min = 1, step = 0.5),
                                       hr(),
                                       checkboxInput("bag_tolerate_na", 
                                                     "Tolerate NAs (exclude NA rows for these variables only)", 
                                                     value = TRUE),
                                       hr(),
                                       radioButtons("bag_transform", "Power transform (visual only):",
                                                    choices  = c("None"         = "none",
                                                                 "Box-Cox"      = "boxcox",
                                                                 "Yeo-Johnson"  = "yeojohnson"),
                                                    selected = "none"),
                                       conditionalPanel(
                                         condition = "input.bag_transform != 'none'",
                                         sidebar_note("Transform applied for display only —
                                         the dataset is not changed. <br><br>
                                         Box-Cox requires all values > 0.")
                                       )
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
                                       relationships and structural irregularities early.
                                       <b>Tip:</b> Set the Outcome role in Data Roles tab first — 
                                       it will auto-select as the grouping variable here."),
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
                                       sliderInput("gg_fontsize", "Label font size:",
                                                   min = 6, max = 24, value = 10, step = 1, width = "100%"),
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
    
    
    # ══ MISS STRATEGY ═════════════════════════════════════════════════════════
    
    tabPanel("Missingness Strategy",
             tabsetPanel(
               
               # ── UI MISSINGNESS STRATEGY ───────────────────────────────────────────
               
               tabPanel("MISS: Variants",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("MISS: Variants: <br><br>
                        <b>Global</b>: treat a value as NA across all columns. <br><br>
                        <b>Column-specific</b>: treat a value as NA in one column only.
                        Use this when the same value is valid elsewhere."),
                                       hr(),
                                       textInput("strat_na_vals",
                                                 label       = "Global — Treat as NA (e.g. -99, --):",
                                                 value       = "",
                                                 placeholder = "comma separated e.g. -99, --, N/A, ?"
                                       )
                          ),
                          mainPanel(width = 9,
                                    h5("Column-Specific NA Rules"),
                                    fluidRow(
                                      column(3, selectInput("strat_col_na_col_1", "Column:", choices = NULL)),
                                      column(3, textInput("strat_col_na_val_1",   "Treat as NA:", placeholder = "e.g. 14"))
                                    ),
                                    fluidRow(
                                      column(3, selectInput("strat_col_na_col_3", "Column:", choices = NULL)),
                                      column(3, textInput("strat_col_na_val_3",   "Treat as NA:", placeholder = "e.g. -1"))
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
                                      column(3, selectInput("strat_na_target_1",   "Target column (has NAs):", choices = NULL)),
                                      column(3, selectInput("strat_na_cond_col_1", "When column:",             choices = NULL)),
                                      column(3, selectInput("strat_na_cond_val_1", "Equals:",                  choices = NULL)),
                                      column(3, selectInput("strat_na_impute_1",   "Impute with:",
                                                            choices = c("Not Applicable" = "not_applicable",
                                                                        "0"              = "zero",
                                                                        "Mean"           = "mean",
                                                                        "Median"         = "median")))
                                    ),
                                    hr(),
                                    h5("Rule 2"),
                                    fluidRow(
                                      column(3, selectInput("strat_na_target_2",   "Target column (has NAs):", choices = NULL)),
                                      column(3, selectInput("strat_na_cond_col_2", "When column:",             choices = NULL)),
                                      column(3, selectInput("strat_na_cond_val_2", "Equals:",                  choices = NULL)),
                                      column(3, selectInput("strat_na_impute_2",   "Impute with:",
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
                                       radioButtons("strat_knn_method", "Imputation method:",
                                                    choices  = c("KNN (k-nearest neighbours)" = "knn",
                                                                 "Bagged trees"               = "bag"),
                                                    selected = "knn"),
                                       conditionalPanel(
                                         condition = "input.strat_knn_method == 'bag'",
                                         sidebar_note("Bag imputation: <br><br>
                                         Uses an ensemble of bagged decision trees. 
                                         More robust than KNN for mixed-type data and non-linear relationships,
                                         but significantly slower on large datasets. 
                                         The <b>k</b> setting above is ignored when bag is selected.")
                                       ),
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
                                       Sometimes we can not rule out MCAR, but we can not prove it either.")
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
    
    
    # ══ OUT STRATEGY ══════════════════════════════════════════════════════════
    
    tabPanel("Outlier Strategy",
             tabsetPanel(
               
               # ── UI OUTLIER STRATEGY ───────────────────────────────────────────────
               
               tabPanel("OUT: Transform",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("TRANSFORM: <br><br>
                     Apply power or scaling transforms to numeric columns. <br><br>
                     <b>Centre</b>: subtract mean → mean = 0. <br><br>
                     <b>Scale</b>: divide by SD → SD = 1. <br><br>
                     <b>Box-Cox</b>: power transform for <b>strictly positive</b> data.
                     Skips columns with any value ≤ 0. <br><br>
                     <b>Yeo-Johnson</b>: power transform that works on any data,
                     including zero and negative values. <br><br>
                     Apply transforms <b>after imputation</b>. <br><br>
                     Transforms are applied in order: 
                     Box-Cox / Yeo-Johnson first, then Centre, then Scale."),
                                       hr(),
                                       sidebar_note("Note: only numeric columns are transformed."),
                                       hr(),
                                       tags$b("Centre + Scale:"),
                                       helpText("These can be applied together."),
                                       fluidRow(
                                         column(6,
                                                actionButton("transform_centre_select_all", "All → Centre",
                                                             icon = icon("check-square"), class = "btn-sm btn-outline-secondary",
                                                             width = "100%")
                                         ),
                                         column(6,
                                                actionButton("transform_scale_select_all", "All → Scale",
                                                             icon = icon("check-square"), class = "btn-sm btn-outline-secondary",
                                                             width = "100%")
                                         )
                                       ),
                                       br(),
                                       actionButton("transform_centre_scale_select_all", "All → Centre + Scale",
                                                    icon = icon("check-square"), class = "btn-sm btn-outline-primary",
                                                    width = "100%"),
                                       hr(),
                                       actionButton("transform_clear_all", "Clear All",
                                                    icon = icon("times"), class = "btn-sm btn-outline-danger",
                                                    width = "100%"),
                                       hr(),
                                       tags$b("Power Transform (pick one):"),
                                       helpText("Box-Cox and Yeo-Johnson are mutually exclusive."),
                                       radioButtons("transform_power_choice",
                                                    label = NULL,
                                                    choices = c("None"        = "none",
                                                                "Box-Cox"     = "boxcox",
                                                                "Yeo-Johnson" = "yeojohnson"),
                                                    selected = "none"),
                                       conditionalPanel(
                                         condition = "input.transform_power_choice != 'none'",
                                         actionButton("transform_power_select_all", "Select All Vars",
                                                      icon = icon("check-square"), class = "btn-sm btn-outline-primary",
                                                      width = "100%")
                                       )
                                       
                          ),
                          mainPanel(width = 9,
                                    
                                    h5("Box-Cox Transform"),
                                    helpText("Strictly positive columns only. Columns with any value ≤ 0 are skipped."),
                                    selectizeInput("strat_boxcox_cols", "Columns to Box-Cox transform:",
                                                   choices  = NULL,
                                                   multiple = TRUE),
                                    hr(),
                                    
                                    h5("Yeo-Johnson Transform"),
                                    helpText("Works on any numeric data including zeros and negatives."),
                                    selectizeInput("strat_yj_cols", "Columns to Yeo-Johnson transform:",
                                                   choices  = NULL,
                                                   multiple = TRUE),
                                    hr(),
                                    
                                    h5("Centre (subtract mean)"),
                                    selectizeInput("strat_centre_cols", "Columns to centre:",
                                                   choices  = NULL,
                                                   multiple = TRUE),
                                    hr(),
                                    
                                    h5("Scale (divide by SD)"),
                                    selectizeInput("strat_scale_cols", "Columns to scale:",
                                                   choices  = NULL,
                                                   multiple = TRUE),
                                    hr(),
                                    
                                    h5("Transform Summary"),
                                    verbatimTextOutput("transform_summary")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("OUT: Mahalanobis Distance",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Mahalanobis Distance: <br><br>
                        Multivariate outlier detection. Treats all numeric predictors
                        jointly as a multi-dimensional normal distribution and flags
                        observations far from the centroid. <br><br>
                        Internally applies: <br>
                        &nbsp;• remove NAs <br>
                        &nbsp;• remove zero/near-zero variance <br>
                        &nbsp;• remove linear combinations <br>
                        &nbsp;• Yeo-Johnson transform <br><br>
                        Then computes D² and compares to a χ² threshold."),
                                       hr(),
                                       selectInput("mah_id_col", "ID / label column:",
                                                   choices = NULL),
                                       hr(),
                                       selectizeInput("mah_pred_cols",
                                                      "Predictor columns (numeric used only):",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(
                                                        placeholder = "Defaults to all Predictor roles"
                                                      )),
                                       hr(),
                                       sliderInput("mah_threshold_p",
                                                   "Chi-squared threshold (p):",
                                                   min = 0.90, max = 0.999,
                                                   value = 0.999, step = 0.001,
                                                   width = "100%"),
                                       helpText("Higher p = stricter, fewer outliers flagged."),
                                       hr(),
                                       checkboxInput("mah_tolerate_na",
                                                     "Remove rows with any NA before computing",
                                                     value = TRUE)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("mah_output", height = "60vh"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("mah_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("OUT: Cook's Distance",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Cook's Distance: <br><br>
                        Measures how much a <b>linear regression</b> is affected by each observation.
                        Flags observations with both high leverage and large residuals. <br><br>
                        Internally applies: <br>
                        &nbsp;• remove nominal predictors <br>
                        &nbsp;• remove rows with NAs <br>
                        &nbsp;• remove near-zero variance <br>
                        &nbsp;• remove linear combinations <br>
                        &nbsp;• remove highly correlated variables <br><br>
                        Then fits a <b>glm (gaussian)</b> and computes D_c per observation. <br><br>
                        Threshold: <b>4 × mean(D_c)</b> — the standard empirical rule."),
                                       hr(),
                                       selectInput("cook_id_col", "ID / label column:",
                                                   choices = NULL),
                                       hr(),
                                       selectInput("cook_response_col", "Response variable (y):",
                                                   choices = NULL),
                                       hr(),
                                       selectizeInput("cook_pred_cols",
                                                      "Predictor columns:",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(
                                                        placeholder = "Defaults to all Predictor roles"
                                                      )),
                                       hr(),
                                       sliderInput("cook_threshold_mult",
                                                   "Threshold multiplier (k × mean D_c):",
                                                   min = 1, max = 10, value = 4, step = 0.5,
                                                   width = "100%"),
                                       helpText("Standard rule: 4 × mean. Higher = fewer outliers flagged.")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("cook_output", height = "60vh"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("cook_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("OUT: LOF",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Local Outlier Factor (LOF): <br><br>
                        Scores each observation by the density of its neighbourhood.
                        Isolated points far from any local cluster get high scores. <br><br>
                        Interpretation: <br>
                        &nbsp;• lof ≤ 1: not an outlier <br>
                        &nbsp;• lof ≥ 1: possible outlier <br>
                        &nbsp;• lof >> 1: probable outlier <br><br>
                        Internally applies: <br>
                        &nbsp;• remove nominal predictors <br>
                        &nbsp;• remove rows with NAs <br>
                        &nbsp;• remove near-zero variance <br>
                        &nbsp;• remove linear combinations <br>
                        &nbsp;• scale by SD <br><br>
                        Uses <code>dbscan::lof()</code>. Requires <b>dbscan</b> package."),
                                       hr(),
                                       selectInput("lof_id_col", "ID / label column:",
                                                   choices = NULL),
                                       hr(),
                                       selectizeInput("lof_pred_cols",
                                                      "Predictor columns (numeric only):",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(
                                                        placeholder = "Defaults to all Predictor roles"
                                                      )),
                                       hr(),
                                       numericInput("lof_min_pts", "minPts (nearest neighbours):",
                                                    value = 4, min = 2, max = 50, step = 1),
                                       helpText("Range 3–6 is usually optimal."),
                                       hr(),
                                       sliderInput("lof_threshold", "LOF threshold:",
                                                   min = 1.0, max = 5.0, value = 2.0, step = 0.1,
                                                   width = "100%"),
                                       helpText("Observations with LOF > threshold are flagged as outliers."),
                                       hr(),
                                       checkboxInput("lof_scale", "Scale predictors (divide by SD)",
                                                     value = TRUE)
                          ),
                          mainPanel(width = 9,
                                    plotOutput("lof_output", height = "60vh"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("lof_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("OUT: SVM",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Support Vector Machine (SVM): <br><br>
                        One-class SVM for novelty/outlier detection. Observations
                        outside the learned boundary are flagged as outliers. <br><br>
                        Key parameters: <br>
                        &nbsp;• <b>nu</b>: upper bound on the fraction of outliers
                        (0.05 = ~5% flagged). Lower = stricter. <br>
                        &nbsp;• <b>kernel</b>: shape of the decision boundary. <br>
                        &nbsp;• <b>scale</b>: whether to scale inside the SVM
                        (redundant if already scaled). <br><br>
                        Internally applies: <br>
                        &nbsp;• remove nominal predictors <br>
                        &nbsp;• remove rows with NAs <br>
                        &nbsp;• remove near-zero variance <br>
                        &nbsp;• remove linear combinations <br><br>
                        Uses <code>e1071::svm()</code>. Requires <b>e1071</b> package."),
                                       hr(),
                                       selectInput("svm_id_col", "ID / label column:",
                                                   choices = NULL),
                                       hr(),
                                       selectizeInput("svm_pred_cols",
                                                      "Predictor columns (numeric only):",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(
                                                        placeholder = "Defaults to all Predictor roles"
                                                      )),
                                       hr(),
                                       sliderInput("svm_nu", "nu (outlier fraction):",
                                                   min = 0.001, max = 0.5, value = 0.05,
                                                   step = 0.005, width = "100%"),
                                       helpText("~nu × n observations will be flagged."),
                                       hr(),
                                       selectInput("svm_kernel", "Kernel:",
                                                   choices  = c("linear", "polynomial",
                                                                "radial", "sigmoid"),
                                                   selected = "linear"),
                                       hr(),
                                       checkboxInput("svm_scale", "Scale inside SVM", value = FALSE),
                                       helpText("Set TRUE if predictors are not already scaled.")
                          ),
                          mainPanel(width = 9,
                                    verbatimTextOutput("svm_summary"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("svm_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("OUT: Random Forest",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Random Forest Residuals: <br><br>
                        Trains a Random Forest to predict the response variable,
                        then flags observations whose <b>residuals</b> are outliers
                        to the residual distribution (IQR method). <br><br>
                        Characteristics: <br>
                        &nbsp;• uses numeric and low-cardinality categorical vars <br>
                        &nbsp;• not sensitive to scaling <br>
                        &nbsp;• does NOT tolerate missing values <br>
                        &nbsp;• somewhat robust to outliers <br>
                        &nbsp;• enables non-linear relationships <br><br>
                        Uses <code>randomForest::randomForest()</code>.
                        Requires <b>randomForest</b> package."),
                                       hr(),
                                       selectInput("rf_id_col", "ID / label column:",
                                                   choices = NULL),
                                       hr(),
                                       selectInput("rf_response_col", "Response variable (y):",
                                                   choices = NULL),
                                       hr(),
                                       selectizeInput("rf_pred_cols",
                                                      "Predictor columns:",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(
                                                        placeholder = "Defaults to all Predictor roles"
                                                      )),
                                       hr(),
                                       numericInput("rf_iqr_k", "IQR multiplier (k):",
                                                    value = 2.0, min = 0.5, step = 0.5),
                                       helpText("Residuals beyond k × IQR are flagged. Try 2.0 for broader detection.")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("rf_output", height = "40vh"),
                                    hr(),
                                    verbatimTextOutput("rf_summary"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("rf_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("OUT: Isolation Forest",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Isolation Forest: <br><br>
             A tree-based technique for detecting outliers. <br><br>
             Characteristics: <br>
             &nbsp;• uses numeric and nominal variables <br>
             &nbsp;• does tolerate missing values <br>
             &nbsp;• has many tuning parameters <br><br>
             The <code>isolation.forest()</code> function from the <b>isotree</b>
             package is used. <code>predict()</code> returns scores between 0 and 1
             — higher scores indicate greater outlierness. <br><br>
             An arbitrary threshold of 0.60 is commonly used as a starting point."),
                                       hr(),
                                       selectInput("iforest_id_col", "ID / label column:",
                                                   choices = NULL),
                                       hr(),
                                       selectizeInput("iforest_pred_cols",
                                                      "Predictor columns:",
                                                      choices  = NULL,
                                                      multiple = TRUE,
                                                      options  = list(
                                                        placeholder = "Defaults to all Predictor roles"
                                                      )),
                                       hr(),
                                       sliderInput("iforest_threshold",
                                                   "Outlier score threshold:",
                                                   min = 0.50, max = 0.95, value = 0.60,
                                                   step = 0.01, width = "100%"),
                                       helpText("Scores above threshold are flagged as outliers.")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("iforest_output", height = "60vh"),
                                    hr(),
                                    verbatimTextOutput("iforest_summary"),
                                    hr(),
                                    h5("Outlier Rows"),
                                    DT::dataTableOutput("iforest_outliers_table")
                          )
                        )
               ), # end tab panel
               
               
               tabPanel("OUT: Outlier Summary",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Outlier Summary: <br><br>
                        Stacks outlier flags from all active detection methods into a
                        single <b>cumulative novelty score</b> per observation. <br><br>
                        Taller bars = flagged by more methods = more likely a true outlier. <br><br>
                        Currently includes: <br>
                        &nbsp;• <b>Mahalanobis</b> distance <br>
                        &nbsp;• <b>Cook's</b> distance <br><br>
                        More methods will be added as they are implemented."),
                                       hr(),
                                       selectInput("os_id_col", "ID / label column:",
                                                   choices = NULL),
                                       hr(),
                                       numericInput("os_min_count", "Min flag count to display:",
                                                    value = 2, min = 1, step = 1),
                                       helpText("Only show observations flagged by at least this many methods.")
                          ),
                          mainPanel(width = 9,
                                    plotOutput("os_output", height = "70vh"),
                                    hr(),
                                    h5("Observations Flagged by Multiple Methods"),
                                    DT::dataTableOutput("os_table")
                          )
                        )
               ), # end tab panel
               
               tabPanel("Coming Soon", p("Under development."))
               
             ) # end strategy tabsetPanel
    ), # end strategy
    
    
    # ══ PREPROCESSING STRATEGY ════════════════════════════════════════════════
    
    tabPanel("Preprocessing Strategy",
             tabsetPanel(
               
               # ── UI PREPROCESSING STRATEGY ─────────────────────────────────────────
               
               tabPanel("Coming Soon", p("Under development."))
               
               
             ) # end strategy tabsetPanel
    ), # end strategy
    
    
    # ══ MODEL ═════════════════════════════════════════════════════════════════
    
    tabPanel("Model",
             tabsetPanel(
               
               tabPanel("MOD: Regularised Regression",
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       sidebar_note("Regularised Regression: <br><br>
                        Fits Ridge, Lasso, or ElasticNet via <b>caret::train</b> with a
                        recipe-based preprocessing pipeline. <br><br>
                        The recipe applies KNN imputation, centering, scaling, and dummy
                        encoding — fitted on training data only to prevent data leakage. <br><br>
                        Assign roles in <b>EDA > Data Roles</b> before running: <br>
                        &nbsp;• <b>Outcome</b>: response variable (y) <br>
                        &nbsp;• <b>Predictor</b>: features to use <br>
                        &nbsp;• <b>Train-Test Split</b>: column with Train/Test labels <br>
                        &nbsp;• <b>Observation ID</b>: excluded from modelling"),
                                       hr(),
                                       
                                       tags$b("Data"),
                                       helpText("Defaults pulled from Data Roles."),
                                       selectInput("mod_response",   "Response variable (y):", choices = NULL),
                                       selectizeInput("mod_pred_cols", "Predictor columns:",
                                                      choices = NULL, multiple = TRUE,
                                                      options = list(placeholder = "Defaults to Predictor roles")),
                                       selectInput("mod_split_col", "Train-Test Split column:",
                                                   choices = NULL),
                                       helpText("Column must contain values 'Train' and 'Test'."),
                                       hr(),
                                       
                                       tags$b("Recipe"),
                                       numericInput("mod_knn_k", "KNN imputation neighbours (k):",
                                                    value = 5, min = 1, max = 50, step = 1),
                                       helpText("Used in step_impute_knn inside the recipe."),
                                       hr(),
                                       
                                       tags$b("Cross Validation"),
                                       selectInput("mod_cv_method", "CV method:",
                                                   choices = c("k-fold"         = "cv",
                                                               "Repeated k-fold" = "repeatedcv",
                                                               "LOOCV"           = "LOOCV"),
                                                   selected = "cv"),
                                       conditionalPanel(
                                         condition = "input.mod_cv_method != 'LOOCV'",
                                         numericInput("mod_k", "Number of folds (k):",
                                                      value = 10, min = 2, max = 30, step = 1)
                                       ),
                                       conditionalPanel(
                                         condition = "input.mod_cv_method == 'repeatedcv'",
                                         numericInput("mod_repeats", "Number of repeats:",
                                                      value = 3, min = 1, max = 10, step = 1)
                                       ),
                                       numericInput("mod_seed", "Random seed:",
                                                    value = 2026, min = 1, step = 1),
                                       hr(),
                                       
                                       tags$b("Regularisation"),
                                       radioButtons("mod_type", "Model type:",
                                                    choices = c("Ridge"      = "ridge",
                                                                "Lasso"      = "lasso",
                                                                "ElasticNet" = "elasticnet"),
                                                    selected = "elasticnet"),
                                       conditionalPanel(
                                         condition = "input.mod_type == 'elasticnet'",
                                         sliderInput("mod_alpha_step", "Alpha grid step:",
                                                     min = 0.1, max = 0.5, value = 0.1, step = 0.1,
                                                     width = "100%"),
                                         helpText("Searches alpha from 0 to 1 in steps. Smaller = finer search.")
                                       ),
                                       radioButtons("mod_lambda_rule", "Lambda selection:",
                                                    choices = c("lambda.min (best CV score)"           = "min",
                                                                "lambda.1se (simplest within 1 SE)"    = "1se"),
                                                    selected = "min"),
                                       hr(),
                                       tags$b("Manual Override (optional)"),
                                       helpText("After running, you can fix alpha and lambda manually and re-run."),
                                       checkboxInput("mod_manual_override", "Use manual alpha and lambda", value = FALSE),
                                       conditionalPanel(
                                         condition = "input.mod_manual_override == true",
                                         numericInput("mod_manual_alpha", "Manual alpha (0=Ridge, 1=Lasso):",
                                                      value = 0.5, min = 0, max = 1, step = 0.05),
                                         numericInput("mod_manual_lambda", "Manual lambda:",
                                                      value = 0.01, min = 0.000001, step = 0.001)
                                       ),
                                       hr(),
                                       
                                       tags$b("Response family"),
                                       selectInput("mod_family", "Family:",
                                                   choices  = c("Gaussian (continuous)" = "gaussian",
                                                                "Binomial (binary)"     = "binomial",
                                                                "Poisson (counts)"      = "poisson"),
                                                   selected = "gaussian"),
                                       hr(),
                                       
                                       actionButton("mod_run", "Run Model",
                                                    icon  = icon("play"),
                                                    width = "100%",
                                                    style = "background-color:#2a9d8f; color:white;
                                                         border:none; border-radius:6px;
                                                         font-weight:600; padding:8px;"),
                                       helpText("Model training may take a moment.")
                          ),
                          mainPanel(width = 9,
                                    tabsetPanel(
                                      tabPanel("CV Error Curve",
                                               plotOutput("mod_cv_plot", height = "60vh"),
                                               hr(),
                                               verbatimTextOutput("mod_cv_summary")
                                      ),
                                      tabPanel("Coefficients",
                                               plotOutput("mod_coef_path", height = "50vh"),
                                               hr(),
                                               h5("Non-zero Coefficients at Selected Lambda"),
                                               DT::dataTableOutput("mod_coef_table")
                                      ),
                                      tabPanel("Test Set Evaluation",
                                               plotOutput("mod_pred_plot", height = "50vh"),
                                               hr(),
                                               verbatimTextOutput("mod_test_metrics")
                                      ),
                                      tabPanel("Model Comparison",
                                               h5("Cumulative Model Runs"),
                                               DT::dataTableOutput("mod_comparison_table"),
                                               hr(),
                                               actionButton("mod_clear_comparison", "Clear History",
                                                            icon  = icon("trash"),
                                                            class = "btn-sm btn-outline-danger")
                                      )
                                    )
                          )
                        )
               ), # end MOD tab
               
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
      
      # missingness strategies
      apply_missingness_strategy(input$strat_na_vals)            |>
      apply_col_missingness_strategy(get_col_na_rules())         |>
      apply_shadow_variables(input$strat_shadow_cols)            |>
      apply_not_applicable_strategy(get_na_rules())              |>
      apply_col_threshold(input$strat_var_thresh)                |>
      apply_row_threshold(input$strat_obs_thresh)                |>
      apply_knn_imputation(
        knn_response = input$strat_knn_response,
        knn_cols     = input$strat_knn_cols,
        knn_k        = input$strat_knn_k,
        method       = input$strat_knn_method
      )                                                          |>
      apply_mmm_imputation(
        mean_cols   = input$strat_mmm_mean_cols,
        median_cols = input$strat_mmm_median_cols,
        mode_cols   = input$strat_mmm_mode_cols
      )                                                          |>
      
      # power transforms before centre/scale
      apply_centre_scale(
        centre_cols = input$strat_centre_cols,
        scale_cols  = input$strat_scale_cols
      )                                                          |>
      apply_power_transform_strategy(input$strat_boxcox_cols, "boxcox")      |>
      apply_power_transform_strategy(input$strat_yj_cols,     "yeojohnson")
      
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
  
  
  # ── DOWNLOAD DF ANY TIME ───────────────────────────────────────────────────
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("strategised_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(get_data(), file, row.names = FALSE)
    }
  )
  
  
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
  
  model_comparison_rv <- reactiveVal(data.frame(
    Run         = integer(),
    Model       = character(),
    Family      = character(),
    CV_Method   = character(),
    Folds       = integer(),
    Best_Alpha  = numeric(),
    Best_Lambda = numeric(),
    Lambda_Rule = character(),
    Predictors  = integer(),
    RMSE        = numeric(),
    MAE         = numeric(),
    R2          = numeric(),
    Time_sec    = numeric(),
    stringsAsFactors = FALSE
  ))
  
  
  observe({
    df      <- get_data()
    current <- roles_rv()
    new_roles <- setNames(rep("Predictor", ncol(df)), names(df))
    if (!is.null(current)) {
      shared <- intersect(names(new_roles), names(current))
      new_roles[shared] <- current[shared]
    }
    # default new shadow columns to Ignore
    shadow_cols <- grep("_shadow$", names(new_roles), value = TRUE)
    new_shadow  <- shadow_cols[!shadow_cols %in% names(current)]
    new_roles[new_shadow] <- "Ignore"
    
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
  
  
  # ── SERVER EDA WORD CLOUD ────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    cat_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    num_cols <- names(df)[sapply(df, is.numeric)]
    choices  <- if (isTRUE(input$wc_include_numeric)) num_cols else cat_cols
    updateSelectizeInput(session, "wc_var",
                         choices  = choices,
                         selected = if (length(choices) > 0) choices[1] else NULL,
                         server   = TRUE)
  })
  
  output$wc_plot <- renderPlot({
    req(input$wc_mode)
    if (input$wc_mode == "catvar") req(length(input$wc_var) > 0)
    tryCatch(
      plot_wordcloud(
        df              = get_data(),
        mode            = input$wc_mode,
        wc_var          = input$wc_var,
        include_numeric = isTRUE(input$wc_include_numeric),
        case_sensitive  = isTRUE(input$wc_case),
        split_mode      = input$wc_split_mode,
        max_words       = input$wc_max_words,
        min_freq        = input$wc_min_freq,
        scale_val       = input$wc_scale,
        palette         = input$wc_palette
      ),
      error = function(e) {
        plot.new()
        text(0.5, 0.5, conditionMessage(e), cex = 1.2, col = "#C41E3A", adj = 0.5)
      }
    )
  })
  
  
  # ── SERVER EDA BARCHART ───────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    all_cols <- names(df)
    cat_cols <- names(df)[sapply(df, is.factor)]
    updateSelectizeInput(session, "bc_var", choices = all_cols,
                         selected = if (length(all_cols) > 0) all_cols[1] else NULL,
                         server = TRUE)
    updateSelectInput(session, "bc_group_var", choices = cat_cols)
  })
  
  output$bc_output <- renderPlot({
    req(length(input$bc_var) > 0)
    plot_barchart(
      df         = get_data(),
      vars       = input$bc_var,
      top_n      = input$bc_top_n,
      include_na = isTRUE(input$bc_include_na),
      group_on   = isTRUE(input$bc_group_on) &&
        !is.null(input$bc_group_var) &&
        input$bc_group_var %in% names(get_data()),
      group_var  = input$bc_group_var,
      stack_vars = isTRUE(input$bc_stack_vars),
      custom_title = input$bc_title
    )
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
    df     <- get_data()
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
    df          <- get_data()
    roles       <- get_roles()
    role_cols   <- names(roles[roles %in% c("Predictor", "Outcome")])
    role_cols   <- intersect(role_cols, names(df))
    
    updateSelectizeInput(session, "cor_vars",
                         choices  = names(df),
                         selected = role_cols,
                         server   = TRUE)
  })
  
  output$cor_output <- renderPlot({
    req(input$cor_vars)
    df  <- get_data()
    sel <- intersect(input$cor_vars, names(df))
    df  <- df[, sel, drop = FALSE]
    
    if (input$cor_view == "misscor") {
      plot_miss_cor(df, input$cor_order, isTRUE(input$cor_abs), 
                    input$cor_method, label_cex = input$cor_label_size)
    } else {
      plot_var_cor(df, names(df), input$cor_method, input$cor_order, 
                   isTRUE(input$cor_abs), label_cex = input$cor_label_size)
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
  
  
  # ── SERVER EDA HISTOGRAM ──────────────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "hist_var", choices = num_cols)
  })
  
  output$hist_output <- renderPlot({
    req(input$hist_var)
    df <- get_data()
    if (isTRUE(input$hist_tolerate_na)) {
      df <- df[!is.na(df[[input$hist_var]]), , drop = FALSE]
    }
    
    x      <- df[[input$hist_var]]
    x_plot <- apply_power_transform(x, input$hist_transform)
    
    x_label <- if (input$hist_transform == "none") {
      input$hist_var
    } else {
      paste0(input$hist_var, " [", input$hist_transform, "]")
    }
    
    plot_df <- data.frame(x = x_plot)
    ggplot(plot_df, aes(x = x)) +
      geom_histogram(bins = input$hist_bins) +
      labs(title = paste("Histogram for", x_label),
           x     = x_label,
           y     = "Count") +
      theme_minimal(base_size = 13)
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
    df <- get_data()
    if (isTRUE(input$bp_tolerate_na)) {
      df <- df[!is.na(df[[input$bp_var]]), , drop = FALSE]
    }
    
    if (input$bp_transform != "none") {
      x_trans <- apply_power_transform(df[[input$bp_var]], input$bp_transform)
      df[[input$bp_var]] <- x_trans
      # relabel for plot title
      attr(df[[input$bp_var]], "transform_label") <- paste0(
        input$bp_var, " [", input$bp_transform, "]"
      )
    }
    
    var_label <- if (input$bp_transform != "none") {
      paste0(input$bp_var, " [", input$bp_transform, "]")
    } else {
      input$bp_var
    }
    
    # reuse plot_boxplot but override the var column name for labelling
    df_plot <- df
    names(df_plot)[names(df_plot) == input$bp_var] <- var_label
    
    plot_boxplot(df_plot, var_label, input$bp_label_col, input$bp_iqr_k)
  })
  
  output$bp_outliers_table <- renderDT({
    req(input$bp_var, input$bp_iqr_k)
    df <- get_data()
    if (isTRUE(input$bp_tolerate_na)) {
      df <- df[!is.na(df[[input$bp_var]]), , drop = FALSE]
    }
    make_hints_dt(get_raw(), get_boxplot_outliers(df, input$bp_var, input$bp_iqr_k))
  }, server = FALSE)
  
  output$bp_hint_ui <- renderUI({
    req(input$bp_var, input$bp_iqr_k)
    df <- get_data()
    if (isTRUE(input$bp_tolerate_na)) {
      df <- df[!is.na(df[[input$bp_var]]), , drop = FALSE]
    }
    
    x <- df[[input$bp_var]]
    n <- length(x)
    
    if (n < 3 || anyNA(x)) return(NULL)
    
    # distribution check
    sk  <- moments::skewness(x)
    kur <- moments::kurtosis(x) - 3   # excess kurtosis
    sw  <- if (n <= 5000) shapiro.test(x)$p.value else NA_real_
    
    dist_msg <- if (abs(sk) < 0.5 && abs(kur) < 1 && (is.na(sw) || sw > 0.05)) {
      tags$span(style = "color:#2a9d8f; font-weight:600;",
                icon("check-circle"), " Distribution looks approximately normal.",
                " IQR-based outlier detection is reliable here."
      )
    } else {
      reasons <- c(
        if (abs(sk) >= 0.5) sprintf("skewness = %.2f (%s)", sk,
                                    if (sk > 0) "right-skewed" else "left-skewed"),
        if (abs(kur) >= 1)  sprintf("excess kurtosis = %.2f (%s)", kur,
                                    if (kur > 0) "heavy tails" else "light tails"),
        if (!is.na(sw) && sw <= 0.05) sprintf("Shapiro-Wilk p = %.3f (non-normal)", sw)
      )
      tags$span(style = "color:#e07b39; font-weight:600;",
                icon("exclamation-triangle"),
                HTML(paste0(
                  " Non-normal distribution detected (",
                  paste(reasons, collapse = "; "),
                  "). IQR outlier boundaries may flag",
                  if (sk > 0.5) " extra points on the right tail —" else
                    if (sk < -0.5) " extra points on the left tail —" else " more points than expected —",
                  " interpret with caution."
                ))
      )
    }
    
    # outlier count context
    k    <- input$bp_iqr_k
    n_out <- nrow(get_boxplot_outliers(df, input$bp_var, k))
    
    # Under normality, P(outside k*IQR) ≈ 2 * pnorm(q) where q maps k to z-score
    # IQR ≈ 1.35*SD, so k*IQR ≈ k*1.35*SD
    z_equiv   <- k * 1.35
    p_outlier <- 2 * pnorm(-z_equiv)
    expected  <- round(n * p_outlier)
    threshold <- max(expected * 2, expected + 3)   # rough "worth checking" line
    
    out_msg <- if (n_out == 0) {
      tags$span(style = "color:#2a9d8f;",
                icon("check-circle"),
                sprintf(" No outliers detected at k = %.1f.", k)
      )
    } else if (n_out <= threshold) {
      tags$span(style = "color:#495057;",
                icon("info-circle"),
                HTML(sprintf(
                  " With <b>n = %d</b> observations and <b>k = %.1f</b>, 
                  roughly <b>%d</b> outlier(s) are expected by chance under normality.",
                  n, k, expected
                )),
                HTML(sprintf(
                  " We have <b>%d</b> — within the expected range. 
                  These may not warrant investigation unless domain context flags them.",
                  n_out
                ))
      )
    } else {
      tags$span(style = "color:#C41E3A; font-weight:600;",
                icon("exclamation-circle"),
                HTML(sprintf(
                  " With <b>n = %d</b> and <b>k = %.1f</b>, only ~<b>%d</b> outlier(s) are expected by chance.",
                  n, k, expected
                )),
                HTML(sprintf(
                  " Wehave <b>%d</b> — notably more than expected. These are worth investigating.",
                  n_out
                ))
      )
    }
    
    # render hint box
    div(
      style = "
      background: #f8f9fa;
      border-left: 4px solid #0d6efd;
      border-radius: 6px;
      padding: 12px 16px;
      font-size: 13px;
      line-height: 1.7;
    ",
      tags$b("Interpretation Hints"),
      tags$br(),
      dist_msg,
      tags$br(), tags$br(),
      out_msg
    )
  })
  
  
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
    df <- get_data()
    if (isTRUE(input$bag_tolerate_na)) {
      df <- df[!is.na(df[[input$bag_x_var]]) & !is.na(df[[input$bag_y_var]]), , drop = FALSE]
    }
    
    x_label <- input$bag_x_var
    y_label <- input$bag_y_var
    
    if (input$bag_transform != "none") {
      df[[input$bag_x_var]] <- apply_power_transform(df[[input$bag_x_var]], input$bag_transform)
      df[[input$bag_y_var]] <- apply_power_transform(df[[input$bag_y_var]], input$bag_transform)
      x_label <- paste0(input$bag_x_var, " [", input$bag_transform, "]")
      y_label <- paste0(input$bag_y_var, " [", input$bag_transform, "]")
    }
    
    # rename cols so plot_bagplot uses the right labels
    names(df)[names(df) == input$bag_x_var] <- x_label
    names(df)[names(df) == input$bag_y_var] <- y_label
    
    plot_bagplot(df, x_label, y_label, input$bag_label_col, input$bag_k)
  })
  
  output$bag_outliers_table <- renderDT({
    req(input$bag_x_var, input$bag_y_var, input$bag_k)
    df <- get_data()
    if (isTRUE(input$bag_tolerate_na)) {
      df <- df[!is.na(df[[input$bag_x_var]]) & !is.na(df[[input$bag_y_var]]), , drop = FALSE]
    }
    make_hints_dt(get_raw(), get_bagplot_outliers(df, input$bag_x_var, input$bag_y_var, input$bag_k))
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
    df          <- get_data()
    roles       <- get_roles()
    outcome_col <- get_outcome_col()
    
    # filter to only Predictor + Outcome roles
    role_cols <- names(roles[roles %in% c("Predictor", "Outcome")])
    role_cols <- intersect(role_cols, names(df))  # guard against dropped cols
    
    cat_cols  <- names(df)[sapply(df, is.factor)]
    
    updateSelectizeInput(session, "gg_vars",
                         choices  = role_cols,
                         selected = role_cols,   # select ALL predictor+outcome by default
                         server   = TRUE)
    
    updateSelectInput(session, "gg_group_var",
                      choices  = cat_cols,
                      selected = if (length(outcome_col) > 0) outcome_col[1] else cat_cols[1])
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
        group_levels = input$gg_group_levels,
        fontsize     = input$gg_fontsize
      )
    })
  })
  
  
  # ── SERVER STRATEGY COL-SPECIFIC NA ───────────────────────────────────────────
  
  observe({
    df   <- get_raw()
    cols <- c("(none)", names(df))
    for (i in 1:4) {
      updateSelectInput(session, paste0("strat_col_na_col_", i), choices = cols)
    }
  })
  
  get_col_na_rules <- reactive({
    lapply(1:4, function(i) {
      list(
        col = input[[paste0("strat_col_na_col_", i)]],
        val = input[[paste0("strat_col_na_val_", i)]]
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
    updateSelectInput(session, "strat_na_target_2",   choices = cols)
    updateSelectInput(session, "strat_na_cond_col_2", choices = cols)
  })
  
  # populate condition value dropdown when condition column changes
  lapply(1:2, function(i) {
    observeEvent(input[[paste0("strat_na_cond_col_", i)]], {
      col <- input[[paste0("strat_na_cond_col_", i)]]
      req(col != "(none)")
      vals <- unique(as.character(get_raw()[[col]]))
      vals <- sort(vals[!is.na(vals)])
      updateSelectInput(session, paste0("strat_na_cond_val_", i),
                        choices = c("", "NA", vals))
    })
  })
  
  # reactive: collect rules from inputs
  get_na_rules <- reactive({
    list(
      list(
        target_col = input$strat_na_target_1,
        cond_col   = input$strat_na_cond_col_1,
        cond_val   = input$strat_na_cond_val_1,
        impute     = input$strat_na_impute_1
      ),
      list(
        target_col = input$strat_na_target_2,
        cond_col   = input$strat_na_cond_col_2,
        cond_val   = input$strat_na_cond_val_2,
        impute     = input$strat_na_impute_2
      )
    )
  })
  
  
  # ── SERVER STRATEGY THRESHOLD ─────────────────────────────────────────────────
  
  output$thresh_col_summary <- renderPrint({
    df_before <- get_raw() |>
      apply_missingness_strategy(input$strat_na_vals) |>
      apply_col_missingness_strategy(get_col_na_rules()) |>
      apply_not_applicable_strategy(get_na_rules())
    df_after <- apply_col_threshold(df_before, input$strat_var_thresh)
    removed  <- ncol(df_before) - ncol(df_after)
    cat(sprintf("Variables removed: %d  |  Remaining: %d / %d",
                removed, ncol(df_after), ncol(df_before)))
  })
  
  output$thresh_col_removed <- renderDT({
    df_before <- get_raw() |>
      apply_missingness_strategy(input$strat_na_vals) |>
      apply_col_missingness_strategy(get_col_na_rules()) |>
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
      apply_col_missingness_strategy(get_col_na_rules()) |>
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
      apply_col_missingness_strategy(get_col_na_rules()) |>
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
    method <- input$strat_knn_method
    
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
  
  
  # ── SERVER STRATEGY TRANSFORMS ─────────────────────────────────────────────────
  
  observe({
    df       <- get_raw()
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    updateSelectizeInput(session, "strat_boxcox_cols",  choices = num_cols, server = TRUE)
    updateSelectizeInput(session, "strat_yj_cols",      choices = num_cols, server = TRUE)
    updateSelectizeInput(session, "strat_centre_cols",  choices = num_cols, server = TRUE)
    updateSelectizeInput(session, "strat_scale_cols",   choices = num_cols, server = TRUE)
  })
  
  output$transform_summary <- renderPrint({
    boxcox  <- input$strat_boxcox_cols
    yj      <- input$strat_yj_cols
    centre  <- input$strat_centre_cols
    scale   <- input$strat_scale_cols
    
    nothing <- all(sapply(list(boxcox, yj, centre, scale), 
                          function(x) is.null(x) || length(x) == 0))
    if (nothing) {
      cat("No transformations applied.\n")
      return(invisible(NULL))
    }
    
    df <- get_raw()
    
    if (!is.null(boxcox) && length(boxcox) > 0) {
      pos_cols  <- boxcox[sapply(boxcox, function(v) 
        v %in% names(df) && all(df[[v]] > 0, na.rm = TRUE))]
      skip_cols <- setdiff(boxcox, pos_cols)
      cat(sprintf("Box-Cox (%d column(s)):\n", length(pos_cols)))
      for (col in pos_cols)  cat(sprintf("  ✓ %s\n", col))
      for (col in skip_cols) cat(sprintf("  ✗ %s  [skipped — contains values ≤ 0]\n", col))
      cat("\n")
    }
    
    if (!is.null(yj) && length(yj) > 0) {
      cat(sprintf("Yeo-Johnson (%d column(s)):\n", length(yj)))
      for (col in yj) cat(sprintf("  ✓ %s\n", col))
      cat("\n")
    }
    
    if (!is.null(centre) && length(centre) > 0) {
      cat(sprintf("Centre (%d column(s)):\n", length(centre)))
      for (col in centre) cat(sprintf("  %s  →  %s - mean(%s)\n", col, col, col))
      cat("\n")
    }
    
    if (!is.null(scale) && length(scale) > 0) {
      cat(sprintf("Scale (%d column(s)):\n", length(scale)))
      for (col in scale) cat(sprintf("  %s  →  %s / sd(%s)\n", col, col, col))
    }
  })
  
  # ── SERVER OUTLIER MAHALANOBIS ─────────────────────────────────────────────────
  
  # keep power transform selectize inputs in sync with the radio button choice
  observeEvent(input$transform_power_choice, {
    choice <- input$transform_power_choice
    # clear both first, then re-enable the chosen one's current selection
    if (choice == "none") {
      updateSelectizeInput(session, "strat_boxcox_cols", selected = character(0))
      updateSelectizeInput(session, "strat_yj_cols",     selected = character(0))
    } else if (choice == "boxcox") {
      updateSelectizeInput(session, "strat_yj_cols", selected = character(0))
    } else if (choice == "yeojohnson") {
      updateSelectizeInput(session, "strat_boxcox_cols", selected = character(0))
    }
  })
  
  # "Select All Vars" for whichever power transform is active
  observeEvent(input$transform_power_select_all, {
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    choice   <- input$transform_power_choice
    if (choice == "boxcox") {
      pos_cols <- num_cols[sapply(num_cols, function(v) all(df[[v]] > 0, na.rm = TRUE))]
      updateSelectizeInput(session, "strat_boxcox_cols", selected = pos_cols)
    } else if (choice == "yeojohnson") {
      updateSelectizeInput(session, "strat_yj_cols", selected = num_cols)
    }
  })
  
  # "All → Centre" button
  observeEvent(input$transform_centre_select_all, {
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "strat_centre_cols", selected = num_cols)
  })
  
  # "All → Scale" button
  observeEvent(input$transform_scale_select_all, {
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "strat_scale_cols", selected = num_cols)
  })
  
  # "All → Centre + Scale" button
  observeEvent(input$transform_centre_scale_select_all, {
    df       <- get_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "strat_centre_cols", selected = num_cols)
    updateSelectizeInput(session, "strat_scale_cols",  selected = num_cols)
  })
  
  # "Clear All" button
  observeEvent(input$transform_clear_all, {
    updateSelectizeInput(session, "strat_boxcox_cols",  selected = character(0))
    updateSelectizeInput(session, "strat_yj_cols",      selected = character(0))
    updateSelectizeInput(session, "strat_centre_cols",  selected = character(0))
    updateSelectizeInput(session, "strat_scale_cols",   selected = character(0))
    updateRadioButtons(session, "transform_power_choice", selected = "none")
  })
  
  observe({
    df      <- get_data()
    id_col  <- get_id_col()
    roles   <- get_roles()
    
    pred_cols <- names(roles[roles == "Predictor"])
    pred_cols <- intersect(pred_cols, names(df))
    
    # auto-exclude shadow columns and non-numeric from default selection
    num_pred_cols <- pred_cols[sapply(pred_cols, function(v) {
      is.numeric(df[[v]]) && !grepl("_shadow$", v)
    })]
    
    updateSelectInput(session, "mah_id_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    
    updateSelectizeInput(session, "mah_pred_cols",
                         choices  = names(df),
                         selected = num_pred_cols,   # ← only numeric, no shadows
                         server   = TRUE)
  })
  
  output$mah_output <- renderPlot({
    req(input$mah_id_col, input$mah_pred_cols)
    plot_mahalanobis(
      df             = get_data(),
      predictor_cols = input$mah_pred_cols,
      id_col         = input$mah_id_col,
      threshold_p    = input$mah_threshold_p
    )
  })
  
  output$mah_outliers_table <- renderDT({
    req(input$mah_id_col, input$mah_pred_cols)
    outliers <- get_mahalanobis_outliers(
      df             = get_data(),
      predictor_cols = input$mah_pred_cols,
      id_col         = input$mah_id_col,
      threshold_p    = input$mah_threshold_p
    )
    make_hints_dt(get_raw(), outliers)
  }, server = FALSE)
  
  
  # ── SERVER OUTLIER COOK'S DISTANCE ────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    id_col   <- get_id_col()
    resp_col <- get_outcome_col()
    roles    <- get_roles()
    
    pred_cols <- names(roles[roles == "Predictor"])
    pred_cols <- intersect(pred_cols, names(df))
    num_pred_cols <- pred_cols[sapply(pred_cols, function(v) {
      is.numeric(df[[v]]) && !grepl("_shadow$", v)
    })]
    
    updateSelectInput(session, "cook_id_col",
                      choices  = names(df),
                      selected = if (length(id_col)   > 0) id_col[1]   else names(df)[1])
    updateSelectInput(session, "cook_response_col",
                      choices  = names(df),
                      selected = if (length(resp_col) > 0) resp_col[1] else names(df)[1])
    updateSelectizeInput(session, "cook_pred_cols",
                         choices  = names(df),
                         selected = num_pred_cols,
                         server   = TRUE)
  })
  
  get_cook_processed <- reactive({
    req(input$cook_response_col, input$cook_pred_cols)
    df       <- get_data()
    response <- input$cook_response_col
    preds    <- setdiff(input$cook_pred_cols, response)
    
    if (length(preds) < 1) return(NULL)
    
    cols_needed <- union(preds, response)
    cols_needed <- intersect(cols_needed, names(df))
    df_sub      <- df[, cols_needed, drop = FALSE]
    
    # keep only numeric predictors
    num_preds <- preds[sapply(preds, function(v) v %in% names(df_sub) && is.numeric(df_sub[[v]]))]
    if (length(num_preds) < 1) return(NULL)
    
    df_sub <- df_sub[, c(num_preds, response), drop = FALSE]
    
    # ── FIX: remove NAs manually first so orig_rows stays aligned with baked ──
    complete_mask <- complete.cases(df_sub)
    orig_rows     <- which(complete_mask)          # indices into get_data()
    df_complete   <- df_sub[complete_mask, , drop = FALSE]
    
    if (nrow(df_complete) < 3) return(NULL)
    
    tryCatch({
      rec <- recipes::recipe(as.formula(paste(response, "~ .")), data = df_complete) |>
        recipes::step_nzv(recipes::all_predictors())                |>
        recipes::step_lincomb(recipes::all_numeric_predictors())    |>
        recipes::step_corr(recipes::all_numeric_predictors(), threshold = 0.9) |>
        recipes::prep(training = df_complete)
      
      baked <- recipes::bake(rec, new_data = NULL)
      
      # after nzv/lincomb/corr some rows are never dropped, so baked rows == df_complete rows
      list(baked = baked, orig_rows = orig_rows)
    }, error = function(e) {
      message("Cook's recipe error: ", conditionMessage(e))
      NULL
    })
  })
  
  output$cook_output <- renderPlot({
    req(input$cook_id_col, input$cook_response_col, input$cook_pred_cols)
    result <- get_cook_processed()
    
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Could not prepare data.\nCheck response and predictor selections.",
                 colour = "#C41E3A", size = 5) +
        theme_void()
      return()
    }
    
    baked     <- result$baked
    orig_rows <- result$orig_rows
    response  <- input$cook_response_col
    df        <- get_data()
    
    if (ncol(baked) < 2 || nrow(baked) < 3) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Not enough complete rows or columns after preprocessing.",
                 colour = "#C41E3A", size = 5) +
        theme_void()
      return()
    }
    
    tryCatch({
      lmod      <- glm(formula = as.formula(paste(response, "~ .")),
                       data    = baked, family = gaussian)
      dc        <- cooks.distance(lmod)
      threshold <- input$cook_threshold_mult * mean(dc, na.rm = TRUE)
      id        <- seq_along(dc) / length(dc)
      id_labels <- as.character(df[[input$cook_id_col]])[orig_rows]
      label     <- ifelse(dc > threshold, id_labels, NA_character_)
      Observations <- ifelse(dc > threshold, "outlier", "non-outlier")
      n_out     <- sum(dc > threshold, na.rm = TRUE)
      
      plot_df <- data.frame(dc, id, label, Observations)
      
      ggplot(plot_df, aes(y = dc, x = id)) +
        geom_point(aes(colour = Observations), size = 2) +
        ggrepel::geom_text_repel(aes(label = label), max.overlaps = 50,
                                 size = 3.2, na.rm = TRUE) +
        scale_colour_manual(values = c("non-outlier" = "#4a80d4",
                                       "outlier"     = "#C41E3A")) +
        geom_hline(yintercept = threshold, colour = "black", linetype = "dashed") +
        scale_x_continuous(breaks = c(0, 0.5, 1),
                           labels = c("0%", "50%", "100%")) +
        annotate("text", x = 0.01, y = threshold * 1.05,
                 label = sprintf("threshold=%.4f  (%d outliers)", threshold, n_out),
                 hjust = 0, size = 3.5, colour = "#333333") +
        labs(
          title = paste0("Cook's Distance | threshold = ",
                         input$cook_threshold_mult, " × mean(D_c) = ",
                         round(threshold, 4),
                         " | n = ", nrow(baked), " complete rows"),
          y = "Cook's distance",
          x = "Complete Observations"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title   = element_text(size = 12, face = "bold", hjust = 0.5),
          legend.title = element_text(face = "bold"),
          legend.text  = element_text(size = 12)
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("Model error: ", conditionMessage(e)),
                 colour = "#C41E3A", size = 4) +
        theme_void()
    })
  })
  
  output$cook_outliers_table <- renderDT({
    req(input$cook_id_col, input$cook_response_col, input$cook_pred_cols)
    result <- get_cook_processed()
    if (is.null(result)) return(data.frame(Message = "No data."))
    
    baked     <- result$baked
    orig_rows <- result$orig_rows
    response  <- input$cook_response_col
    df        <- get_data()
    
    tryCatch({
      lmod      <- glm(formula = as.formula(paste(response, "~ .")),
                       data    = baked, family = gaussian)
      dc        <- cooks.distance(lmod)
      threshold <- input$cook_threshold_mult * mean(dc, na.rm = TRUE)
      outlier_orig_rows <- orig_rows[dc > threshold]
      make_hints_dt(get_raw(), df[outlier_orig_rows, , drop = FALSE])
    }, error = function(e) {
      data.frame(Message = paste0("Error: ", conditionMessage(e)))
    })
  }, server = FALSE)
  
  
  # ── SERVER OUTLIER LOF ────────────────────────────────────────────────────────
  
  observe({
    df     <- get_data()
    id_col <- get_id_col()
    roles  <- get_roles()
    
    pred_cols <- names(roles[roles == "Predictor"])
    pred_cols <- intersect(pred_cols, names(df))
    num_pred_cols <- pred_cols[sapply(pred_cols, function(v) {
      is.numeric(df[[v]]) && !grepl("_shadow$", v)
    })]
    
    updateSelectInput(session, "lof_id_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    updateSelectizeInput(session, "lof_pred_cols",
                         choices  = names(df),
                         selected = num_pred_cols,
                         server   = TRUE)
  })
  
  get_lof_result <- reactive({
    req(input$lof_id_col, input$lof_pred_cols)
    df        <- get_data()
    preds     <- input$lof_pred_cols
    num_preds <- preds[sapply(preds, function(v) v %in% names(df) && is.numeric(df[[v]]))]
    if (length(num_preds) < 2) return(NULL)
    
    df_sub <- df[, num_preds, drop = FALSE]
    
    # remove NAs manually to track orig_rows
    complete_mask <- complete.cases(df_sub)
    orig_rows     <- which(complete_mask)
    df_complete   <- df_sub[complete_mask, , drop = FALSE]
    if (nrow(df_complete) < 5) return(NULL)
    
    tryCatch({
      rec <- recipes::recipe(~ ., data = df_complete) |>
        recipes::step_nzv(recipes::all_predictors()) |>
        recipes::step_lincomb(recipes::all_numeric_predictors())
      
      if (isTRUE(input$lof_scale)) {
        rec <- rec |> recipes::step_scale(recipes::all_numeric_predictors())
      }
      
      baked <- rec |>
        recipes::prep(training = df_complete) |>
        recipes::bake(new_data = NULL)
      
      if (ncol(baked) < 1) return(NULL)
      
      mat <- as.matrix(baked)
      lof_scores <- dbscan::lof(mat, minPts = input$lof_min_pts)
      
      list(lof = lof_scores, orig_rows = orig_rows)
    }, error = function(e) {
      message("LOF error: ", conditionMessage(e))
      NULL
    })
  })
  
  output$lof_output <- renderPlot({
    req(input$lof_id_col, input$lof_pred_cols)
    result <- get_lof_result()
    
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Could not compute LOF.\nCheck predictor selections and ensure dbscan is installed.",
                 colour = "#C41E3A", size = 5) +
        theme_void()
      return()
    }
    
    df        <- get_data()
    lof       <- result$lof
    orig_rows <- result$orig_rows
    threshold <- input$lof_threshold
    id_labels <- as.character(df[[input$lof_id_col]])[orig_rows]
    
    id           <- seq_along(lof) / length(lof)
    label        <- ifelse(lof > threshold, id_labels, NA_character_)
    Observations <- ifelse(lof > threshold, "outlier", "non-outlier")
    n_out        <- sum(lof > threshold)
    
    plot_df <- data.frame(lof, id, label, Observations)
    
    ggplot(plot_df, aes(y = lof, x = id)) +
      geom_point(aes(colour = Observations), size = 2) +
      ggrepel::geom_text_repel(aes(label = label), max.overlaps = 50,
                               size = 3.2, na.rm = TRUE) +
      scale_colour_manual(values = c("non-outlier" = "#4a80d4",
                                     "outlier"     = "#C41E3A")) +
      geom_hline(yintercept = threshold, colour = "black", linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.5, 1),
                         labels = c("0%", "50%", "100%")) +
      scale_y_continuous(limits = c(0, NA)) +
      annotate("text", x = 0.01, y = threshold * 1.05,
               label = sprintf("threshold=%.1f  (%d outliers)", threshold, n_out),
               hjust = 0, size = 3.5, colour = "#333333") +
      labs(
        title = paste0("LOF | minPts = ", input$lof_min_pts,
                       " | threshold = ", threshold,
                       if (isTRUE(input$lof_scale)) " | scaled" else " | unscaled",
                       " | n = ", length(lof), " complete rows"),
        y = "LOF",
        x = "Complete Observations"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title   = element_text(size = 12, face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text  = element_text(size = 12)
      )
  })
  
  output$lof_outliers_table <- renderDT({
    req(input$lof_id_col, input$lof_pred_cols)
    result <- get_lof_result()
    if (is.null(result)) return(data.frame(Message = "No data."))
    
    df        <- get_data()
    lof       <- result$lof
    orig_rows <- result$orig_rows
    threshold <- input$lof_threshold
    outlier_orig_rows <- orig_rows[lof > threshold]
    make_hints_dt(get_raw(), df[outlier_orig_rows, , drop = FALSE])
  }, server = FALSE)
  
  
  # ── SERVER OUTLIER SVM ────────────────────────────────────────────────────────
  
  observe({
    df     <- get_data()
    id_col <- get_id_col()
    roles  <- get_roles()
    
    pred_cols <- names(roles[roles == "Predictor"])
    pred_cols <- intersect(pred_cols, names(df))
    num_pred_cols <- pred_cols[sapply(pred_cols, function(v) {
      is.numeric(df[[v]]) && !grepl("_shadow$", v)
    })]
    
    updateSelectInput(session, "svm_id_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    updateSelectizeInput(session, "svm_pred_cols",
                         choices  = names(df),
                         selected = num_pred_cols,
                         server   = TRUE)
  })
  
  get_svm_result <- reactive({
    req(input$svm_id_col, input$svm_pred_cols)
    df        <- get_data()
    preds     <- input$svm_pred_cols
    num_preds <- preds[sapply(preds, function(v) v %in% names(df) && is.numeric(df[[v]]))]
    if (length(num_preds) < 1) return(NULL)
    
    df_sub <- df[, num_preds, drop = FALSE]
    
    # remove NAs manually to track orig_rows
    complete_mask <- complete.cases(df_sub)
    orig_rows     <- which(complete_mask)
    df_complete   <- df_sub[complete_mask, , drop = FALSE]
    if (nrow(df_complete) < 5) return(NULL)
    
    tryCatch({
      rec <- recipes::recipe(~ ., data = df_complete) |>
        recipes::step_nzv(recipes::all_predictors()) |>
        recipes::step_lincomb(recipes::all_numeric_predictors()) |>
        recipes::prep(training = df_complete)
      
      baked <- recipes::bake(rec, new_data = NULL)
      if (ncol(baked) < 1) return(NULL)
      
      mat   <- as.matrix(baked)
      model <- e1071::svm(mat, y = NULL,
                          type   = "one-classification",
                          nu     = input$svm_nu,
                          scale  = isTRUE(input$svm_scale),
                          kernel = input$svm_kernel)
      
      svm_outlier <- !predict(model, mat)   # TRUE = outlier
      list(svm_outlier = svm_outlier, orig_rows = orig_rows, n_total = nrow(mat))
    }, error = function(e) {
      message("SVM error: ", conditionMessage(e))
      NULL
    })
  })
  
  output$svm_summary <- renderPrint({
    req(input$svm_id_col, input$svm_pred_cols)
    result <- get_svm_result()
    
    if (is.null(result)) {
      cat("Could not fit SVM.\nCheck predictor selections and ensure e1071 is installed.\n")
      return(invisible(NULL))
    }
    
    df          <- get_data()
    svm_outlier <- result$svm_outlier
    orig_rows   <- result$orig_rows
    n_out       <- sum(svm_outlier)
    n_total     <- result$n_total
    out_ids     <- as.character(df[[input$svm_id_col]])[orig_rows[svm_outlier]]
    
    cat(sprintf("One-Class SVM | kernel = %s | nu = %.4f | scale = %s\n",
                input$svm_kernel, input$svm_nu,
                if (isTRUE(input$svm_scale)) "TRUE" else "FALSE"))
    cat(sprintf("Complete rows : %d\n", n_total))
    cat(sprintf("Outliers      : %d  (%.1f%%)\n", n_out, 100 * n_out / n_total))
    cat("──────────────────────────────\n")
    if (n_out == 0) {
      cat("No outliers flagged.\n")
    } else {
      cat("Flagged IDs:\n")
      cat(paste(out_ids, collapse = "  "), "\n")
    }
  })
  
  output$svm_outliers_table <- renderDT({
    req(input$svm_id_col, input$svm_pred_cols)
    result <- get_svm_result()
    if (is.null(result)) return(data.frame(Message = "No data."))
    
    df          <- get_data()
    svm_outlier <- result$svm_outlier
    orig_rows   <- result$orig_rows
    outlier_orig_rows <- orig_rows[svm_outlier]
    
    if (length(outlier_orig_rows) == 0) return(data.frame(Message = "No outliers flagged."))
    make_hints_dt(get_raw(), df[outlier_orig_rows, , drop = FALSE])
  }, server = FALSE)
  
  
  # ── SERVER OUTLIER RANDOM FOREST ──────────────────────────────────────────────
  
  observe({
    df       <- get_data()
    id_col   <- get_id_col()
    resp_col <- get_outcome_col()
    roles    <- get_roles()
    
    pred_cols <- names(roles[roles == "Predictor"])
    pred_cols <- intersect(pred_cols, names(df))
    # RF handles low-cardinality factors — exclude shadow cols only
    rf_pred_cols <- pred_cols[!grepl("_shadow$", pred_cols)]
    
    updateSelectInput(session, "rf_id_col",
                      choices  = names(df),
                      selected = if (length(id_col)   > 0) id_col[1]   else names(df)[1])
    updateSelectInput(session, "rf_response_col",
                      choices  = names(df),
                      selected = if (length(resp_col) > 0) resp_col[1] else names(df)[1])
    updateSelectizeInput(session, "rf_pred_cols",
                         choices  = names(df),
                         selected = rf_pred_cols,
                         server   = TRUE)
  })
  
  get_rf_result <- reactive({
    req(input$rf_id_col, input$rf_response_col, input$rf_pred_cols)
    df       <- get_data()
    response <- input$rf_response_col
    preds    <- setdiff(input$rf_pred_cols, response)
    if (length(preds) < 1) return(NULL)
    if (!is.numeric(df[[response]])) return(NULL)
    
    cols_needed  <- union(preds, response)
    cols_needed  <- intersect(cols_needed, names(df))
    df_sub       <- df[, cols_needed, drop = FALSE]
    
    # remove rows with any NA
    complete_mask <- complete.cases(df_sub)
    orig_rows     <- which(complete_mask)
    df_complete   <- df_sub[complete_mask, , drop = FALSE]
    if (nrow(df_complete) < 10) return(NULL)
    
    tryCatch({
      # remove near-zero variance and linear combos from numeric preds only
      num_preds <- preds[sapply(preds, function(v) v %in% names(df_complete) &&
                                  is.numeric(df_complete[[v]]))]
      if (length(num_preds) > 0) {
        rec <- recipes::recipe(as.formula(paste(response, "~ .")),
                               data = df_complete) |>
          recipes::step_nzv(recipes::all_predictors()) |>
          recipes::step_lincomb(recipes::all_numeric_predictors()) |>
          recipes::prep(training = df_complete)
        df_model <- recipes::bake(rec, new_data = NULL)
      } else {
        df_model <- df_complete
      }
      
      if (ncol(df_model) < 2) return(NULL)
      
      gen_model <- randomForest::randomForest(
        as.formula(paste(response, "~ .")),
        data = df_model
      )
      
      residuals      <- df_complete[[response]] - predict(gen_model, newdata = df_complete)
      names(residuals) <- as.character(df[[input$rf_id_col]])[orig_rows]
      
      # IQR-based outlier detection on residuals
      k      <- input$rf_iqr_k
      limits <- boxplot.stats(x = residuals, coef = k)$stats
      is_out <- residuals < limits[1] | residuals > limits[5]
      
      list(
        residuals = residuals,
        is_out    = is_out,
        limits    = limits,
        orig_rows = orig_rows,
        k         = k
      )
    }, error = function(e) {
      message("RF error: ", conditionMessage(e))
      NULL
    })
  })
  
  output$rf_output <- renderPlot({
    req(input$rf_id_col, input$rf_response_col, input$rf_pred_cols)
    result <- get_rf_result()
    
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Could not fit Random Forest.\nCheck selections, ensure no NAs remain,\nand that randomForest is installed.",
                 colour = "#C41E3A", size = 5) +
        theme_void()
      return()
    }
    
    residuals <- result$residuals
    is_out    <- result$is_out
    k         <- result$k
    label     <- ifelse(is_out, names(residuals), NA_character_)
    
    plot_df <- data.frame(
      residuals    = residuals,
      Observations = ifelse(is_out, "outlier", "non-outlier"),
      label        = label
    )
    
    ggplot(plot_df, aes(x = residuals, y = 0)) +
      geom_boxplot(coef = k, outlier.colour = "#C41E3A") +
      ggrepel::geom_text_repel(aes(label = label), max.overlaps = 50,
                               na.rm = TRUE, size = 3.2) +
      labs(
        title = paste0("Random Forest Residuals | IQR multiplier k = ", k,
                       " | n = ", length(residuals), " complete rows"),
        x = "residuals"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title   = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  
  output$rf_summary <- renderPrint({
    req(input$rf_id_col, input$rf_response_col, input$rf_pred_cols)
    result <- get_rf_result()
    if (is.null(result)) {
      cat("No result available.\n")
      return(invisible(NULL))
    }
    residuals <- result$residuals
    is_out    <- result$is_out
    limits    <- result$limits
    n_out     <- sum(is_out)
    
    cat(sprintf("Random Forest Residual Outliers | k = %.1f\n", result$k))
    cat(sprintf("SD of residuals : %.4f\n", sd(residuals)))
    cat(sprintf("Whisker limits  : [%.4f,  %.4f]\n", limits[1], limits[5]))
    cat(sprintf("Complete rows   : %d\n", length(residuals)))
    cat(sprintf("Outliers        : %d\n", n_out))
    cat("──────────────────────────────\n")
    if (n_out == 0) {
      cat("No outliers flagged.\n")
    } else {
      cat("Flagged IDs:\n")
      cat(paste(names(residuals)[is_out], collapse = "  "), "\n")
    }
  })
  
  output$rf_outliers_table <- renderDT({
    req(input$rf_id_col, input$rf_response_col, input$rf_pred_cols)
    result <- get_rf_result()
    if (is.null(result)) return(data.frame(Message = "No data."))
    
    is_out    <- result$is_out
    orig_rows <- result$orig_rows
    df        <- get_data()
    
    outlier_orig_rows <- orig_rows[is_out]
    if (length(outlier_orig_rows) == 0) return(data.frame(Message = "No outliers flagged."))
    make_hints_dt(get_raw(), df[outlier_orig_rows, , drop = FALSE])
  }, server = FALSE)
  
  
  # ── SERVER OUTLIER ISOLATION FOREST ──────────────────────────────────────────
  
  observe({
    df     <- get_data()
    id_col <- get_id_col()
    roles  <- get_roles()
    
    pred_cols <- names(roles[roles == "Predictor"])
    pred_cols <- intersect(pred_cols, names(df))
    iforest_pred_cols <- pred_cols[!grepl("_shadow$", pred_cols)]
    
    updateSelectInput(session, "iforest_id_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
    updateSelectizeInput(session, "iforest_pred_cols",
                         choices  = names(df),
                         selected = iforest_pred_cols,
                         server   = TRUE)
  })
  
  get_iforest_result <- reactive({
    req(input$iforest_id_col, input$iforest_pred_cols)
    df    <- get_data()
    preds <- input$iforest_pred_cols
    preds <- intersect(preds, names(df))
    if (length(preds) < 1) return(NULL)
    
    df_sub <- df[, preds, drop = FALSE]
    if (nrow(df_sub) < 5) return(NULL)
    
    tryCatch({
      itree  <- isotree::isolation.forest(df_sub)
      scores <- predict(itree, newdata = df_sub)
      names(scores) <- as.character(df[[input$iforest_id_col]])
      
      list(scores = scores, orig_rows = seq_len(nrow(df)))
    }, error = function(e) {
      message("Isolation Forest error: ", conditionMessage(e))
      NULL
    })
  })
  
  output$iforest_output <- renderPlot({
    req(input$iforest_id_col, input$iforest_pred_cols)
    result <- get_iforest_result()
    
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Could not fit Isolation Forest.\nCheck selections and ensure isotree is installed.",
                 colour = "#C41E3A", size = 5) +
        theme_void()
      return()
    }
    
    scores    <- result$scores
    threshold <- input$iforest_threshold
    n         <- length(scores)
    id        <- seq_len(n) / n
    is_out    <- scores > threshold
    n_out     <- sum(is_out)
    label     <- ifelse(is_out, names(scores), NA_character_)
    Observations <- ifelse(is_out, "outlier", "non-outlier")
    
    plot_df <- data.frame(scores, id, label, Observations)
    
    ggplot(plot_df, aes(y = scores, x = id)) +
      geom_point(aes(colour = Observations), size = 2) +
      ggrepel::geom_text_repel(aes(label = label), max.overlaps = 50,
                               size = 3.2, na.rm = TRUE) +
      scale_colour_manual(values = c("non-outlier" = "#4a80d4",
                                     "outlier"     = "#C41E3A")) +
      geom_hline(yintercept = threshold, colour = "black", linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.5, 1),
                         labels = c("0%", "50%", "100%")) +
      scale_y_continuous(limits = c(0, NA)) +
      annotate("text", x = 0.01, y = threshold * 1.02,
               label = sprintf("threshold=%.2f  (%d outliers)", threshold, n_out),
               hjust = 0, size = 3.5, colour = "#333333") +
      labs(
        title = paste0("Isolation Forest | threshold = ", threshold,
                       " | n = ", n, " observations"),
        y = "Iso Score",
        x = "Complete Observations"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title   = element_text(size = 12, face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text  = element_text(size = 12)
      )
  })
  
  output$iforest_summary <- renderPrint({
    req(input$iforest_id_col, input$iforest_pred_cols)
    result <- get_iforest_result()
    
    if (is.null(result)) {
      cat("No result available.\n")
      return(invisible(NULL))
    }
    
    scores    <- result$scores
    threshold <- input$iforest_threshold
    is_out    <- scores > threshold
    n_out     <- sum(is_out)
    n_total   <- length(scores)
    
    cat(sprintf("Isolation Forest | threshold = %.2f\n", threshold))
    cat(sprintf("Observations  : %d\n", n_total))
    cat(sprintf("Outliers      : %d  (%.1f%%)\n", n_out, 100 * n_out / n_total))
    cat("──────────────────────────────\n")
    if (n_out == 0) {
      cat("No outliers flagged.\n")
    } else {
      cat("Flagged IDs:\n")
      cat(paste(names(scores)[is_out], collapse = "  "), "\n")
    }
  })
  
  output$iforest_outliers_table <- renderDT({
    req(input$iforest_id_col, input$iforest_pred_cols)
    result <- get_iforest_result()
    if (is.null(result)) return(data.frame(Message = "No data."))
    
    scores    <- result$scores
    threshold <- input$iforest_threshold
    is_out    <- scores > threshold
    orig_rows <- result$orig_rows
    outlier_orig_rows <- orig_rows[is_out]
    
    if (length(outlier_orig_rows) == 0) return(data.frame(Message = "No outliers flagged."))
    make_hints_dt(get_raw(), get_data()[outlier_orig_rows, , drop = FALSE])
  }, server = FALSE)
  
  
  # ── SERVER OUTLIER SUMMARY ─────────────────────────────────────────────────────
  
  observe({
    df     <- get_data()
    id_col <- get_id_col()
    updateSelectInput(session, "os_id_col",
                      choices  = names(df),
                      selected = if (length(id_col) > 0) id_col[1] else names(df)[1])
  })
  
  get_outlier_summary <- reactive({
    req(input$os_id_col)
    df <- get_data()
    
    id_labels <- as.character(df[[input$os_id_col]])
    flags     <- list()
    
    # ── Mahalanobis ──
    if (!is.null(input$mah_pred_cols) && length(input$mah_pred_cols) > 0 &&
        !is.null(input$mah_id_col)) {
      tryCatch({
        out_rows <- get_mahalanobis_outliers(
          df             = df,
          predictor_cols = input$mah_pred_cols,
          id_col         = input$mah_id_col,
          threshold_p    = input$mah_threshold_p
        )
        if (nrow(out_rows) > 0) {
          flags[["mahalanobis"]] <- as.character(out_rows[[input$os_id_col]])
        }
      }, error = function(e) NULL)
    }
    
    # ── Cook's Distance ──
    if (!is.null(input$cook_response_col) && !is.null(input$cook_pred_cols) &&
        length(input$cook_pred_cols) > 0) {
      tryCatch({
        result <- get_cook_processed()
        if (!is.null(result)) {
          baked     <- result$baked
          orig_rows <- result$orig_rows
          response  <- input$cook_response_col
          
          if (ncol(baked) >= 2 && nrow(baked) >= 3) {
            lmod      <- glm(as.formula(paste(response, "~ .")), data = baked, family = gaussian)
            dc        <- cooks.distance(lmod)
            threshold <- input$cook_threshold_mult * mean(dc, na.rm = TRUE)
            cook_out  <- orig_rows[dc > threshold]
            if (length(cook_out) > 0) {
              flags[["cooks"]] <- as.character(df[[input$os_id_col]])[cook_out]
            }
          }
        }
      }, error = function(e) NULL)
    }
    
    # ── LOF ──
    if (!is.null(input$lof_pred_cols) && length(input$lof_pred_cols) > 0 &&
        !is.null(input$lof_id_col)) {
      tryCatch({
        result <- get_lof_result()
        if (!is.null(result)) {
          lof       <- result$lof
          orig_rows <- result$orig_rows
          threshold <- input$lof_threshold
          lof_out   <- orig_rows[lof > threshold]
          if (length(lof_out) > 0) {
            flags[["lof"]] <- as.character(df[[input$os_id_col]])[lof_out]
          }
        }
      }, error = function(e) NULL)
    }
    
    # ── SVM ──
    if (!is.null(input$svm_pred_cols) && length(input$svm_pred_cols) > 0 &&
        !is.null(input$svm_id_col)) {
      tryCatch({
        result <- get_svm_result()
        if (!is.null(result)) {
          svm_outlier <- result$svm_outlier
          orig_rows   <- result$orig_rows
          svm_out     <- orig_rows[svm_outlier]
          if (length(svm_out) > 0) {
            flags[["svm"]] <- as.character(df[[input$os_id_col]])[svm_out]
          }
        }
      }, error = function(e) NULL)
    }
    
    # ── Random Forest ──
    if (!is.null(input$rf_pred_cols) && length(input$rf_pred_cols) > 0 &&
        !is.null(input$rf_response_col) && !is.null(input$rf_id_col)) {
      tryCatch({
        result <- get_rf_result()
        if (!is.null(result)) {
          is_out    <- result$is_out
          orig_rows <- result$orig_rows
          rf_out    <- orig_rows[is_out]
          if (length(rf_out) > 0) {
            flags[["rf"]] <- as.character(df[[input$os_id_col]])[rf_out]
          }
        }
      }, error = function(e) NULL)
    }
    
    # ── Isolation Forest ──
    if (!is.null(input$iforest_pred_cols) && length(input$iforest_pred_cols) > 0 &&
        !is.null(input$iforest_id_col)) {
      tryCatch({
        result <- get_iforest_result()
        if (!is.null(result)) {
          scores    <- result$scores
          threshold <- input$iforest_threshold
          is_out    <- scores > threshold
          orig_rows <- result$orig_rows
          iforest_out <- orig_rows[is_out]
          if (length(iforest_out) > 0) {
            flags[["iforest"]] <- as.character(df[[input$os_id_col]])[iforest_out]
          }
        }
      }, error = function(e) NULL)
    }
    
    # before plotting
    if (length(flags) == 0) return(NULL)
    
    # build long-format data frame
    long_df <- do.call(rbind, lapply(names(flags), function(method) {
      data.frame(id = flags[[method]], Type = method, stringsAsFactors = FALSE)
    }))
    
    long_df
  })
  
  output$os_output <- renderPlot({
    long_df <- get_outlier_summary()
    
    if (is.null(long_df) || nrow(long_df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No outliers flagged yet.\nRun Mahalanobis and/or Cook's Distance first.",
                 colour = "#495057", size = 5) +
        theme_void()
      return()
    }
    
    # filter by min count
    counts   <- table(long_df$id)
    keep_ids <- names(counts[counts >= input$os_min_count])
    plot_df  <- long_df[long_df$id %in% keep_ids, ]
    
    if (nrow(plot_df) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste0("No observations flagged by ≥ ", input$os_min_count, " methods."),
                 colour = "#495057", size = 5) +
        theme_void()
      return()
    }
    
    # order x-axis by total count descending
    id_order <- names(sort(table(plot_df$id), decreasing = TRUE))
    plot_df$id <- factor(plot_df$id, levels = id_order)
    
    method_colors <- c(
      "mahalanobis" = "#2ab7ca",
      "cooks"       = "#fe4a49",
      "lof"         = "#27ae60",
      "svm"         = "#e056fd",
      "rf"          = "#4a80d4",
      "iforest"     = "#f39c12"
    )
    
    ggplot(plot_df, aes(x = id, fill = Type)) +
      geom_bar(position = "stack") +
      scale_fill_manual(values = method_colors) +
      labs(
        title = "Observation Novelty (cumulative outlier scores)",
        x     = "id",
        y     = "Count",
        fill  = "Type"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x  = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y  = element_text(size = 11),
        plot.title   = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        legend.text  = element_text(size = 12)
      )
  })
  
  output$os_table <- renderDT({
    long_df <- get_outlier_summary()
    if (is.null(long_df) || nrow(long_df) == 0) {
      return(data.frame(Message = "No outliers flagged yet."))
    }
    
    counts   <- table(long_df$id)
    keep_ids <- names(counts[counts >= input$os_min_count])
    plot_df  <- long_df[long_df$id %in% keep_ids, ]
    
    if (nrow(plot_df) == 0) return(data.frame(Message = "No observations meet threshold."))
    
    # pivot to wide: one row per id, one col per method
    summary_df <- as.data.frame.matrix(
      table(plot_df$id, plot_df$Type)
    )
    summary_df$id          <- rownames(summary_df)
    summary_df$total_flags <- rowSums(summary_df[, setdiff(names(summary_df), "id"), drop = FALSE])
    summary_df <- summary_df[order(-summary_df$total_flags), ]
    rownames(summary_df) <- NULL
    
    # reorder cols: id first, total_flags second, then methods
    method_cols <- setdiff(names(summary_df), c("id", "total_flags"))
    summary_df  <- summary_df[, c("id", "total_flags", method_cols)]
    
    datatable(summary_df,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  }, server = FALSE)
  
  
  # ── SERVER MODEL REGULARISED REGRESSION ───────────────────────────────────────
  
  # populate dropdowns from roles
  observe({
    df        <- get_data()
    resp_col  <- get_outcome_col()
    pred_cols <- get_predictor_cols()
    split_col <- get_split_col()
    id_col    <- get_id_col()
    
    # filter predictor cols — exclude ID, split, shadow cols
    exclude <- c(
      if (length(id_col)    > 0) id_col,
      if (length(split_col) > 0) split_col
    )
    pred_cols <- setdiff(pred_cols, exclude)
    pred_cols <- pred_cols[!grepl("_shadow$", pred_cols)]
    pred_cols <- intersect(pred_cols, names(df))
    
    updateSelectInput(session, "mod_response",
                      choices  = names(df),
                      selected = if (length(resp_col)  > 0) resp_col[1]  else names(df)[1])
    updateSelectizeInput(session, "mod_pred_cols",
                         choices  = names(df),
                         selected = pred_cols,
                         server   = TRUE)
    updateSelectInput(session, "mod_split_col",
                      choices  = names(df),
                      selected = if (length(split_col) > 0) split_col[1] else names(df)[1])
  })
  
  # main model reactive — triggered by Run button only
  get_model_result <- eventReactive(input$mod_run, {
    req(input$mod_response, input$mod_pred_cols, input$mod_split_col)
    
    df        <- get_data()
    response  <- input$mod_response
    split_col <- input$mod_split_col
    id_cols   <- get_id_col()
    
    # ── validate split column ──
    split_vals <- unique(as.character(df[[split_col]]))
    if (!all(c("Train", "Test") %in% split_vals)) {
      return(list(error = paste0(
        "Split column '", split_col,
        "' must contain both 'Train' and 'Test' values.\n",
        "Found: ", paste(split_vals, collapse = ", ")
      )))
    }
    
    # ── predictor cols — exclude response, split, id ──
    preds <- setdiff(input$mod_pred_cols, c(response, split_col, id_cols))
    preds <- intersect(preds, names(df))
    if (length(preds) < 1) return(list(error = "No predictor columns available."))
    
    # ── split ──
    train <- df[as.character(df[[split_col]]) == "Train", , drop = FALSE]
    test  <- df[as.character(df[[split_col]]) == "Test",  , drop = FALSE]
    
    if (nrow(train) < 5) return(list(error = "Training set has fewer than 5 rows."))
    if (nrow(test)  < 1) return(list(error = "Test set is empty."))
    
    # ── recipe ──
    keep_cols <- union(preds, c(response, split_col, id_cols))
    keep_cols <- intersect(keep_cols, names(train))
    train_sub <- train[, keep_cols, drop = FALSE]
    test_sub  <- test[,  keep_cols, drop = FALSE]
    
    tryCatch({
      
      rec <- recipes::recipe(
        as.formula(paste(response, "~ .")),
        data = train_sub
      )
      
      # update roles for non-predictor columns
      if (length(id_cols) > 0) {
        id_in_data <- intersect(id_cols, names(train_sub))
        if (length(id_in_data) > 0)
          rec <- rec |> recipes::update_role(
            tidyselect::all_of(id_in_data), new_role = "id"
          )
      }
      if (split_col %in% names(train_sub)) {
        rec <- rec |> recipes::update_role(
          tidyselect::all_of(split_col), new_role = "split"
        )
      }
      
      rec <- rec |>
        recipes::step_impute_knn(
          recipes::all_predictors(), neighbors = input$mod_knn_k
        ) |>
        recipes::step_center(recipes::all_numeric_predictors())  |>
        recipes::step_scale(recipes::all_numeric_predictors())   |>
        recipes::step_dummy(recipes::all_nominal_predictors())
      
      # ── alpha grid ──
      # if manual override, skip grid search entirely
      if (isTRUE(input$mod_manual_override)) {
        alpha_grid <- input$mod_manual_alpha
      } else {
        alpha_grid <- switch(input$mod_type,
                             "ridge"      = 0,
                             "lasso"      = 1,
                             "elasticnet" = seq(0, 1, by = input$mod_alpha_step)
        )
      }
      
      # ── trainControl ──
      nfolds <- if (input$mod_cv_method == "LOOCV") nrow(train) else input$mod_k
      
      ctrl <- caret::trainControl(
        method  = input$mod_cv_method,
        number  = nfolds,
        repeats = if (input$mod_cv_method == "repeatedcv") input$mod_repeats else 1,
        seeds   = NULL
      )
      
      # ── train ──
      set.seed(input$mod_seed)
      t_start <- proc.time()
      
      mod <- caret::train(
        rec,
        data      = train_sub,
        method    = "glmnet",
        trControl = ctrl,
        tuneGrid  = expand.grid(
          alpha  = alpha_grid,
          lambda = 10^seq(3, -4, length = 100)
        ),
        family = input$mod_family
      )
      
      elapsed <- (proc.time() - t_start)[["elapsed"]]
      
      # ── extract best alpha and cv object ──
      best_alpha  <- mod$bestTune$alpha
      best_lambda <- mod$bestTune$lambda
      
      # refit glmnet directly to get cv object for plotting
      baked_train <- recipes::prep(rec, training = train_sub) |>
        recipes::bake(new_data = NULL)
      
      x_train <- baked_train |>
        dplyr::select(-tidyselect::all_of(response)) |>
        as.matrix()
      y_train <- baked_train[[response]]
      
      set.seed(input$mod_seed)
      cv_fit <- glmnet::cv.glmnet(
        x_train, y_train,
        alpha       = best_alpha,
        nfolds      = nfolds,
        family      = input$mod_family,
        standardize = FALSE
      )
      
      # ── lambda selection ──
      chosen_lambda <- if (isTRUE(input$mod_manual_override)) {
        input$mod_manual_lambda
      } else if (input$mod_lambda_rule == "min") {
        cv_fit$lambda.min
      } else {
        cv_fit$lambda.1se
      }
      
      final_fit <- glmnet::glmnet(
        x_train, y_train,
        alpha       = best_alpha,
        lambda      = chosen_lambda,
        family      = input$mod_family,
        standardize = FALSE
      )
      
      # ── bake test set ──
      baked_test <- recipes::prep(rec, training = train_sub) |>
        recipes::bake(new_data = test_sub)
      
      x_test <- baked_test |>
        dplyr::select(-tidyselect::all_of(response)) |>
        as.matrix()
      y_test <- baked_test[[response]]
      
      # ── predictions ──
      preds_raw <- predict(final_fit, newx = x_test, type = "response")
      y_hat     <- as.numeric(preds_raw)
      
      # ── metrics ──
      rmse <- sqrt(mean((y_test - y_hat)^2))
      mae  <- mean(abs(y_test - y_hat))
      ss_res <- sum((y_test - y_hat)^2)
      ss_tot <- sum((y_test - mean(y_test))^2)
      r2   <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
      
      # ── non-zero coefficients ──
      coef_mat   <- as.matrix(coef(final_fit))
      coef_df    <- data.frame(
        Predictor   = rownames(coef_mat),
        Coefficient = round(coef_mat[, 1], 6),
        stringsAsFactors = FALSE
      )
      coef_df <- coef_df[coef_df$Predictor != "(Intercept)" & coef_df$Coefficient != 0, ]
      coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]
      
      list(
        error        = NULL,
        cv_fit       = cv_fit,
        final_fit    = final_fit,
        best_alpha   = best_alpha,
        best_lambda  = best_lambda,
        chosen_lambda = chosen_lambda,
        lambda_rule  = input$mod_lambda_rule,
        coef_df      = coef_df,
        y_test       = y_test,
        y_hat        = y_hat,
        rmse         = rmse,
        mae          = mae,
        r2           = r2,
        elapsed      = elapsed,
        n_train      = nrow(train),
        n_test       = nrow(test),
        cv_method    = input$mod_cv_method,
        nfolds       = nfolds,
        model_type   = input$mod_type,
        family       = input$mod_family
      )
      
    }, error = function(e) {
      list(error = conditionMessage(e))
    })
  })
  
  # ── CV error curve ──
  output$mod_cv_plot <- renderPlot({
    result <- get_model_result()
    if (is.null(result)) {
      ggplot() + annotate("text", x=0.5, y=0.5,
                          label="Press 'Run Model' to train.", colour="#495057", size=5) + theme_void()
      return()
    }
    if (!is.null(result$error)) {
      ggplot() + annotate("text", x=0.5, y=0.5,
                          label=result$error, colour="#C41E3A", size=4) + theme_void()
      return()
    }
    par(mar = c(5, 4, 6, 2))
    plot(result$cv_fit,
         main = paste0("CV Error vs log(Lambda) | alpha = ", result$best_alpha))
  })
  
  output$mod_cv_summary <- renderPrint({
    result <- get_model_result()
    if (is.null(result) || !is.null(result$error)) return(invisible(NULL))
    
    cat("── Optimised Parameters ──────────────────\n")
    cat(sprintf("Best alpha    : %.4f\n", result$best_alpha))
    cat(sprintf("lambda.min    : %.6f\n", result$cv_fit$lambda.min))
    cat(sprintf("lambda.1se    : %.6f\n", result$cv_fit$lambda.1se))
    cat(sprintf("Chosen lambda : %.6f  (%s)\n", result$chosen_lambda,
                if (isTRUE(input$mod_manual_override)) "manual" else result$lambda_rule))
    if (isTRUE(input$mod_manual_override)) {
      cat("⚠ Manual override active — CV values above are for reference only.\n")
    }
    cat("\n── Model Settings ────────────────────────\n")
    cat(sprintf("Model type    : %s\n",   toupper(result$model_type)))
    cat(sprintf("Family        : %s\n",   result$family))
    cat(sprintf("CV method     : %s\n",   result$cv_method))
    cat(sprintf("Folds         : %d\n",   result$nfolds))
    cat(sprintf("Training rows : %d\n",   result$n_train))
    cat(sprintf("Test rows     : %d\n",   result$n_test))
    cat(sprintf("Time elapsed  : %.2f s\n", result$elapsed))
  })
  
  # ── coefficient path + table ──
  output$mod_coef_path <- renderPlot({
    result <- get_model_result()
    if (is.null(result) || !is.null(result$error)) return(invisible(NULL))
    par(mar = c(5, 4, 6, 8))
    plot(result$final_fit,
         xvar  = "lambda",
         label = TRUE,
         main  = paste0("Coefficient Path | alpha = ", result$best_alpha))
    abline(v = log(result$chosen_lambda), lty = 2, col = "red")
  })
  
  output$mod_coef_table <- renderDT({
    result <- get_model_result()
    if (is.null(result) || !is.null(result$error)) {
      return(data.frame(Message = "Run model first."))
    }
    datatable(result$coef_df,
              options  = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  }, server = FALSE)
  
  # ── test set evaluation ──
  output$mod_pred_plot <- renderPlot({
    result <- get_model_result()
    if (is.null(result) || !is.null(result$error)) return(invisible(NULL))
    
    plot_df <- data.frame(Actual = result$y_test, Predicted = result$y_hat)
    lim     <- range(c(plot_df$Actual, plot_df$Predicted), na.rm = TRUE)
    
    ggplot(plot_df, aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.6, colour = "#4a80d4", size = 2) +
      geom_abline(slope = 1, intercept = 0, colour = "#C41E3A",
                  linetype = "dashed", linewidth = 0.8) +
      coord_fixed(xlim = lim, ylim = lim) +
      labs(
        title = paste0("Predicted vs Actual | Test Set | RMSE = ",
                       round(result$rmse, 4)),
        x = "Actual", y = "Predicted"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
  })
  
  output$mod_test_metrics <- renderPrint({
    result <- get_model_result()
    if (is.null(result) || !is.null(result$error)) return(invisible(NULL))
    
    cat("── Test Set Performance ──────────────────\n")
    cat(sprintf("RMSE : %.6f\n", result$rmse))
    cat(sprintf("MAE  : %.6f\n", result$mae))
    cat(sprintf("R²   : %.6f\n", result$r2))
    cat(sprintf("n    : %d observations\n", length(result$y_test)))
    cat(sprintf("Predictors used : %d\n", nrow(result$coef_df)))
  })
  
  # ── model comparison table ──
  observeEvent(get_model_result(), {
    result <- get_model_result()
    if (is.null(result) || !is.null(result$error)) return()
    
    current <- model_comparison_rv()
    new_run <- nrow(current) + 1
    
    new_row <- data.frame(
      Run         = new_run,
      Model       = toupper(result$model_type),
      Family      = result$family,
      CV_Method   = result$cv_method,
      Folds       = result$nfolds,
      Best_Alpha  = round(result$best_alpha,   4),
      Best_Lambda = round(result$chosen_lambda, 6),
      Lambda_Rule = if (isTRUE(input$mod_manual_override)) "manual" else result$lambda_rule,
      Predictors  = nrow(result$coef_df),
      RMSE        = round(result$rmse, 6),
      MAE         = round(result$mae,  6),
      R2          = round(result$r2,   6),
      Time_sec    = round(result$elapsed, 3),
      stringsAsFactors = FALSE
    )
    
    model_comparison_rv(rbind(current, new_row))
  })
  
  output$mod_comparison_table <- renderDT({
    df <- model_comparison_rv()
    if (nrow(df) == 0) return(data.frame(Message = "No model runs yet."))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  }, server = FALSE)
  
  observeEvent(input$mod_clear_comparison, {
    model_comparison_rv(data.frame(
      Run=integer(), Model=character(), Family=character(),
      CV_Method=character(), Folds=integer(), Best_Alpha=numeric(),
      Best_Lambda=numeric(), Lambda_Rule=character(), Predictors=integer(),
      RMSE=numeric(), MAE=numeric(), R2=numeric(), Time_sec=numeric(),
      stringsAsFactors=FALSE
    ))
  })
  
  
} # end server








# =================================================================================
# Run
# =================================================================================

shinyApp(ui, server)