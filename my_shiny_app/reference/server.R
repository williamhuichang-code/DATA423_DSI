# =============================================================================
# server.R
# All reactive logic lives here.
# Each section matches a tab in ui.R.
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # HELPER: get the right numeric column subset
  # ---------------------------------------------------------------------------
  numeric_group <- function(grp) {
    switch(grp,
      meta   = meta_num,
      sens_a = sensor_cols[1:min(15, length(sensor_cols))],
      sens_b = {
        s <- sensor_cols[16:min(30, length(sensor_cols))]
        if (length(s) == 0) sensor_cols else s
      },
      all    = num_cols
    )
  }

  # ---------------------------------------------------------------------------
  # POPULATE UI CHOICES (run once on startup)
  # ---------------------------------------------------------------------------
  observe({
    # Mosaic — exclude ID (360 unique levels, useless in a mosaic)
    mosaic_choices <- factor_cols[factor_cols != "ID"]
    updateSelectInput(session, "mosaic_x", choices = mosaic_choices,
                      selected = mosaic_choices[1])
    updateSelectInput(session, "mosaic_y", choices = mosaic_choices,
                      selected = mosaic_choices[min(2, length(mosaic_choices))])

    # GGPairs colour-by — exclude ID (exceeds GGally cardinality threshold)
    colour_choices <- factor_cols[factor_cols != "ID"]
    updateSelectInput(session, "ggpairs_colour", choices = c("None", colour_choices),
                      selected = "None")

    # Boxplot variable selector
    updateSelectInput(session, "box_vars", choices = num_cols,
                      selected = meta_num[1:min(5, length(meta_num))])

    # Rising order
    updateSelectInput(session, "rise_var", choices = num_cols,
                      selected = num_cols[1])

    # Data table columns
    updateCheckboxGroupInput(session, "dt_cols", choices = all_cols,
                             selected = all_cols[1:min(10, length(all_cols))])

    # Model tab
    updateSelectInput(session, "mod_y", choices = num_cols, selected = "Y")
    updateSelectInput(session, "mod_x", choices = num_cols,
                      selected = meta_num[min(2, length(meta_num))])
  })

  # ---------------------------------------------------------------------------
  # 0. OVERVIEW
  # ---------------------------------------------------------------------------
  output$overview_dim <- renderPrint({
    cat("Rows:", nrow(dat), "\n")
    cat("Columns:", ncol(dat), "\n")
    cat("  Numeric:", length(num_cols), "\n")
    cat("  Factor: ", length(factor_cols), "\n")
    cat("  Date:   ", length(date_cols), "\n")
  })

  output$overview_types <- renderPrint({
    sapply(dat, class)
  })

  output$overview_summary <- renderPrint({
    data_subset <- dat[, c(meta_num, factor_cols)]
    # option 1: base R summary
    # summary(data_subset)
    # option 2: tibble::glimpse
    # tibble::glimpse(data_subset)
    # option 3: summarytools::dfSummary
    summarytools::dfSummary(data_subset)
  })

  # ---------------------------------------------------------------------------
  # 1. MOSAIC CHART
  # FIX: isTRUE() so shade= never receives NULL on first render
  # FIX: ID excluded from choices in observe() above
  # ---------------------------------------------------------------------------
  output$mosaic_plot <- renderPlot({
    req(input$mosaic_x, input$mosaic_y)
    x_var    <- input$mosaic_x
    y_var    <- input$mosaic_y
    shade_on <- isTRUE(input$mosaic_shade)

    # Drop unused factor levels BEFORE building the table.
    # Sparse/empty cells cause chisq residuals to be NaN, which makes
    # vcd::mosaic() crash with "missing value where TRUE/FALSE needed"
    # when shade = TRUE.
    x_clean <- droplevels(dat[[x_var]])
    y_clean <- droplevels(dat[[y_var]])
    tbl     <- table(x_clean, y_clean)
    dimnames(tbl) <- list(x_var = levels(x_clean),
                          y_var = levels(y_clean))

    # If shade is requested but the table is too sparse for chi-sq, fall back
    # to unshaded so the plot still renders rather than crashing.
    if (shade_on) {
      ct <- tryCatch(chisq.test(tbl), error = function(e) NULL)
      if (is.null(ct) || any(is.nan(ct$residuals))) {
        shade_on <- FALSE
        message("Mosaic: shade disabled — table too sparse for chi-sq residuals.")
      }
    }

    vcd::mosaic(tbl,
                shade    = shade_on,
                legend   = shade_on,
                main     = paste("Mosaic:", x_var, "vs", y_var),
                xlab     = x_var,
                ylab     = y_var,
                labeling = vcd::labeling_border(rot_labels = c(0, 0, 0, 90)))
  })

  # ---------------------------------------------------------------------------
  # 2. GGPAIRS
  # FIX: NULL guard on colour_var before observe() fires
  # FIX: ID excluded from colour choices (360 levels exceeds cardinality threshold)
  # ---------------------------------------------------------------------------
  output$ggpairs_plot <- renderPlot({
    req(input$ggpairs_group)
    grp      <- input$ggpairs_group
    col_set  <- numeric_group(grp)
    col_data <- dat[, col_set, drop = FALSE]

    colour_var <- input$ggpairs_colour
    if (is.null(colour_var) || colour_var == "None") {
      mapping    <- NULL
      colour_lbl <- ""
    } else {
      col_data[[colour_var]] <- dat[[colour_var]]
      mapping    <- ggplot2::aes(colour = .data[[colour_var]])
      colour_lbl <- paste("coloured by", colour_var)
    }

    smooth_fn <- if (isTRUE(input$ggpairs_smooth)) "smooth" else "points"

    GGally::ggpairs(
      col_data,
      mapping = mapping,
      lower   = list(continuous = smooth_fn),
      title   = paste("GGPairs -", grp, colour_lbl)
    )
  })

  # ---------------------------------------------------------------------------
  # 3. CORRELATION PLOT
  # FIX: guard against < 2 columns (meta_num may only have 1 col in some datasets)
  # FIX: absolute mode wraps result back as data.frame — corrgram needs data.frame
  # ---------------------------------------------------------------------------
  output$corr_plot <- renderPlot({
    col_set <- numeric_group(input$corr_vars)

    if (length(col_set) < 2) {
      plot.new()
      text(0.5, 0.5,
           paste0("Need at least 2 numeric columns.\n'",
                  input$corr_vars, "' only has ", length(col_set), "."),
           cex = 1.2, col = "firebrick", adj = 0.5)
      return()
    }

    d <- dat[, col_set, drop = FALSE]

    if (isTRUE(input$corr_abs)) {
      d <- as.data.frame(abs(cor(d, use = "pairwise.complete.obs")))
    }

    corrgram::corrgram(
      d,
      order  = TRUE,
      lower.panel = switch(input$corr_type,
                           shade   = corrgram::panel.shade,
                           pie     = corrgram::panel.pie,
                           ellipse = corrgram::panel.ellipse,
                           number  = corrgram::panel.pts),
      upper.panel = corrgram::panel.conf,
      text.panel  = corrgram::panel.txt,
      main        = paste("Corrgram -", input$corr_vars,
                          if (isTRUE(input$corr_abs)) "(absolute)" else "")
    )
  })

  # ---------------------------------------------------------------------------
  # 4. MISSINGNESS
  # ---------------------------------------------------------------------------
  output$miss_plot <- renderPlot({
    visdat::vis_miss(dat, cluster = isTRUE(input$miss_cluster)) +
      ggplot2::labs(title = paste0("Missing data - threshold flagged at ",
                                   input$miss_thresh, "%")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
  })

  # ---------------------------------------------------------------------------
  # 5. BOXPLOT
  # FIX: keep scale() output as matrix — car::Boxplot works with matrix, not data.frame
  # ---------------------------------------------------------------------------
  output$box_plot <- renderPlot({
    req(input$box_vars)
    vars <- input$box_vars

    d <- dat[, vars, drop = FALSE]
    d_scaled <- scale(d,
                      center = isTRUE(input$box_center),
                      scale  = isTRUE(input$box_scale))

    subtitle <- paste0(
      if (isTRUE(input$box_center)) "Centred" else "",
      if (isTRUE(input$box_center) && isTRUE(input$box_scale)) " + " else "",
      if (isTRUE(input$box_scale))  "Scaled"  else "",
      if (!isTRUE(input$box_center) && !isTRUE(input$box_scale)) "Raw values" else "",
      "  |  IQR multiplier: ", input$box_coef
    )

    car::Boxplot(d_scaled,
                 coef  = input$box_coef,
                 main  = paste("Boxplot:", paste(vars, collapse = ", ")),
                 sub   = subtitle,
                 ylab  = if (isTRUE(input$box_scale)) "Standardised value" else "Value",
                 col   = "lightblue",
                 las   = 2)
  })

  # ---------------------------------------------------------------------------
  # 6. RISING ORDER CHART
  # ---------------------------------------------------------------------------
  output$rise_plot <- renderPlot({
    req(input$rise_var)
    v   <- input$rise_var
    raw <- sort(dat[[v]], na.last = NA)
    n   <- length(raw)
    df  <- data.frame(rank = seq_len(n), value = raw)

    diffs   <- diff(raw)
    gap_sd  <- sd(diffs, na.rm = TRUE)
    gap_mu  <- mean(diffs, na.rm = TRUE)
    is_gap  <- diffs > (gap_mu + 3 * gap_sd)
    gap_idx <- which(is_gap)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = rank, y = value)) +
      ggplot2::geom_line(colour = "steelblue") +
      ggplot2::labs(
        title    = paste("Rising Order Chart:", v),
        subtitle = paste(sum(is_gap), "gap(s) detected"),
        x        = "Rank (sorted position)",
        y        = v
      ) +
      ggplot2::theme_bw()

    if (isTRUE(input$rise_points)) {
      p <- p + ggplot2::geom_point(size = 1, colour = "steelblue")
    }

    if (isTRUE(input$rise_gaps) && length(gap_idx) > 0) {
      gap_df <- data.frame(rank  = gap_idx + 0.5,
                           value = (raw[gap_idx] + raw[gap_idx + 1]) / 2)
      p <- p + ggplot2::geom_vline(data = gap_df,
                                   ggplot2::aes(xintercept = rank),
                                   colour  = "red", linetype = "dashed")
    }

    print(p)
  })

  # ---------------------------------------------------------------------------
  # 7. DATA TABLE
  # ---------------------------------------------------------------------------
  output$data_table <- DT::renderDataTable({
    cols <- input$dt_cols
    if (is.null(cols) || length(cols) == 0) cols <- all_cols[1:5]
    DT::datatable(
      dat[, cols, drop = FALSE],
      filter  = "top",
      options = list(pageLength = 15, scrollX = TRUE),
      caption = "Table 1: Interactive data listing"
    )
  })

  # ---------------------------------------------------------------------------
  # 8. MODEL (Simple Linear Regression - A2/A3 Preview)
  # FIX: validate() prevents the nonsensical Y ~ Y case (R^2 = 1 trivially)
  # ---------------------------------------------------------------------------
  lm_model <- reactive({
    req(input$mod_y, input$mod_x)
    validate(need(input$mod_y != input$mod_x,
                  "Response and Predictor must be different variables."))
    lm(dat[[input$mod_y]] ~ dat[[input$mod_x]], data = dat, na.action = na.omit)
  })

  output$mod_scatter <- renderPlot({
    req(input$mod_y, input$mod_x)
    y_var <- input$mod_y
    x_var <- input$mod_x

    df_plot <- data.frame(x = dat[[x_var]], y = dat[[y_var]])
    df_plot <- df_plot[complete.cases(df_plot), ]

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.5, colour = "steelblue") +
      ggplot2::geom_smooth(method = "lm",
                            se     = isTRUE(input$mod_ci),
                            colour = "firebrick") +
      ggplot2::labs(
        title    = paste("OLS:", y_var, "~", x_var),
        subtitle = paste0("R2 = ", round(summary(lm_model())$r.squared, 3)),
        x        = x_var,
        y        = y_var
      ) +
      ggplot2::theme_bw()

    print(p)
  })

  output$mod_resid_plot <- renderPlot({
    m  <- lm_model()
    df <- data.frame(fitted = fitted(m), residual = residuals(m))

    ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = residual)) +
      ggplot2::geom_point(alpha = 0.5, colour = "steelblue") +
      ggplot2::geom_hline(yintercept = 0, colour = "firebrick", linetype = "dashed") +
      ggplot2::geom_smooth(se = FALSE, colour = "orange", method = "loess") +
      ggplot2::labs(title    = "Residuals vs Fitted",
                    subtitle = "A pattern here suggests non-linearity or heteroscedasticity",
                    x        = "Fitted values",
                    y        = "Residuals") +
      ggplot2::theme_bw()
  })

  output$mod_summary <- renderPrint({
    cat("=== OLS Model Summary ===\n")
    print(summary(lm_model()))
    cat("\n--- NOTE ---\n")
    cat("This is a simple OLS baseline.\n")
    cat("Assignment 2 -> replace with glmnet (ridge/lasso/elastic net)\n")
    cat("Assignment 3 -> extend to caret with multiple methods & cross-validation\n")
  })

}  # end server
