# =================================================================================
# mod_split.R
# Split tab module — 6 resampling subtabs with sidebar-right layout
# =================================================================================

# ── LIBRARIES ─────────────────────────────────────────────────────────────────
library(caret)
library(cluster)
library(ggplot2)
library(ggrepel)

# ── SHARED STYLE HELPERS ──────────────────────────────────────────────────────

# Reusable sidebar style (matches mod_eda_vis.R)
.sidebar_style <- "background-color: #f4f6fb; border-left: 3px solid #6a9fd8;
                   min-height: 100vh; padding: 16px 14px;"

# Reusable info box
.info_box <- function(...) {
  div(
    style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
    icon("info-circle", style = "color:#0d6efd;"),
    HTML("&nbsp;"),
    ...
  )
}

# Reusable section label
.ctrl_label <- function(text) {
  tags$label(text, style = "font-weight:600; font-size:13px; color:#343a40;")
}

# Reusable badge
.badge <- function(text, color = "purple") {
  colors <- list(
    purple = "background:#EEEDFE; color:#3C3489;",
    teal   = "background:#E1F5EE; color:#085041;",
    amber  = "background:#FAEEDA; color:#633806;",
    coral  = "background:#FAECE7; color:#712B13;"
  )
  span(text, style = paste0("font-size:11px; padding:2px 8px; border-radius:99px;
                              display:inline-block; margin-bottom:6px; ",
                            colors[[color]]))
}

# Global label weight fix (same as mod_eda_vis.R)
.label_fix <- tags$style(HTML("
  .shiny-input-container label,
  .radio label,
  .checkbox label {
    font-weight: 400 !important;
    font-size: 13px;
  }
"))


# =================================================================================
# UI
# =================================================================================

split_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    .label_fix,
    tabsetPanel(
      id = ns("active_tab"),
      type = "pills",
      
      # ── TAB 1: 2-partition ────────────────────────────────────────────────
      tabPanel("2-partition (train + test)",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .sidebar_style,
                   .info_box(HTML("<strong>Tab note:</strong><br>Splits data into train + test using
                           stratified random sampling. Stratification preserves the outcome
                           distribution across both partitions.")),
                   hr(),
                   .ctrl_label("Train proportion"),
                   sliderInput(ns("p1_train"), label = NULL,
                               min = 0.5, max = 0.95, value = 0.8, step = 0.05, width = "100%"),
                   hr(),
                   .ctrl_label("Stratify by"),
                   selectInput(ns("p1_stratify"), label = NULL,
                               choices = c("Response (stratified)" = "stratified",
                                           "None (simple random)"  = "simple")),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p1_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Independent observations", "purple"),
                   div(style = "font-size:11px; color:#6c757d;",
                       "Uses ", code("createDataPartition()"))
                 ),
                 mainPanel(
                   width = 9,
                   h4("Partition sizes"),
                   tableOutput(ns("p1_table")),
                   hr(),
                   h4("Response distribution — train vs test"),
                   plotOutput(ns("p1_plot"), height = "350px")
                 )
               )
      ),
      
      # ── TAB 2: 3-partition ────────────────────────────────────────────────
      tabPanel("3-partition (train + val + test)",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .sidebar_style,
                   .info_box(HTML("<strong>Tab note:</strong><br>Two-stage split: first extracts the
                           train set, then splits the remainder into validation + test.
                           Useful when you want a held-out validation set for model selection
                           before final test evaluation.")),
                   hr(),
                   .ctrl_label("Train proportion"),
                   sliderInput(ns("p2_train"), label = NULL,
                               min = 0.4, max = 0.8, value = 0.6, step = 0.05, width = "100%"),
                   .ctrl_label("Val / Test split (of remainder)"),
                   sliderInput(ns("p2_valtest"), label = NULL,
                               min = 0.2, max = 0.8, value = 0.5, step = 0.05, width = "100%"),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p2_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Independent observations", "purple"),
                   div(style = "font-size:11px; color:#6c757d;",
                       "Uses ", code("createDataPartition()"), " twice")
                 ),
                 mainPanel(
                   width = 9,
                   h4("Partition sizes"),
                   tableOutput(ns("p2_table")),
                   hr(),
                   h4("Response distribution — train / validation / test"),
                   plotOutput(ns("p2_plot"), height = "350px")
                 )
               )
      ),
      
      # ── TAB 3: Stratified bootstrap ───────────────────────────────────────
      tabPanel("Stratified bootstrap",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .sidebar_style,
                   .info_box(HTML("<strong>Tab note:</strong><br>Stratified bootstrap draws samples
                           with replacement, preserving the outcome distribution. Each resample
                           contains ~63% unique observations — the rest are duplicates.")),
                   hr(),
                   .ctrl_label("Number of resamples"),
                   sliderInput(ns("p3_times"), label = NULL,
                               min = 5, max = 100, value = 25, step = 5, width = "100%"),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p3_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Independent observations", "purple"),
                   div(style = "font-size:11px; color:#6c757d;",
                       "Uses ", code("createResample()"))
                 ),
                 mainPanel(
                   width = 9,
                   h4("Resample summary"),
                   tableOutput(ns("p3_table")),
                   hr(),
                   h4("% unique observations per resample"),
                   plotOutput(ns("p3_plot"), height = "350px")
                 )
               )
      ),
      
      # ── TAB 4: Leave group out ────────────────────────────────────────────
      tabPanel("Leave group out",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .sidebar_style,
                   .info_box(HTML("<strong>Tab note:</strong><br>Ensures no group (e.g. patient)
                           appears in both train and test folds. Prevents data leakage from
                           repeated measurements on the same subject.")),
                   hr(),
                   .ctrl_label("Group variable"),
                   selectInput(ns("p4_group"), label = NULL, choices = NULL),
                   hr(),
                   .ctrl_label("Number of folds (k)"),
                   sliderInput(ns("p4_k"), label = NULL,
                               min = 2, max = 3, value = 2, step = 1, width = "100%"),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p4_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Group dependent observations", "teal"),
                   div(style = "font-size:11px; color:#6c757d;",
                       "Uses ", code("groupKFold()"))
                 ),
                 mainPanel(
                   width = 9,
                   h4("Fold summary"),
                   tableOutput(ns("p4_table")),
                   hr(),
                   h4("Group counts per fold — in train vs held-out"),
                   plotOutput(ns("p4_plot"), height = "350px")
                 )
               )
      ),
      
      # ── TAB 5: Time series ────────────────────────────────────────────────
      tabPanel("Time series",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .sidebar_style,
                   .info_box(HTML("<strong>Tab note:</strong><br>Train always precedes test in time —
                           no future leakage. Expanding window grows the train set each slice;
                           fixed window keeps the train size constant.")),
                   hr(),
                   .ctrl_label("Initial window"),
                   sliderInput(ns("p5_init"), label = NULL,
                               min = 2, max = 50, value = 5, step = 1, width = "100%"),
                   .ctrl_label("Horizon"),
                   sliderInput(ns("p5_horizon"), label = NULL,
                               min = 1, max = 20, value = 3, step = 1, width = "100%"),
                   hr(),
                   .ctrl_label("Fixed window"),
                   selectInput(ns("p5_fixed"), label = NULL,
                               choices = c("FALSE (expanding window)" = "FALSE",
                                           "TRUE  (fixed window)"    = "TRUE")),
                   hr(),
                   .badge("Time dependent observations", "amber"),
                   div(style = "font-size:11px; color:#6c757d;",
                       "Uses ", code("createTimeSlices()"))
                 ),
                 mainPanel(
                   width = 9,
                   h4("Time slice summary"),
                   tableOutput(ns("p5_table")),
                   hr(),
                   h4("Train / test windows across slices"),
                   plotOutput(ns("p5_plot"), height = "400px")
                 )
               )
      ),
      
      # ── TAB 6: Diversity down-sampling ────────────────────────────────────
      tabPanel("Diversity down-sampling",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .sidebar_style,
                   .info_box(HTML("<strong>Tab note:</strong><br>K-medoids clustering selects one
                           representative (medoid) from each cluster as the training sample.
                           This maximises coverage of the feature space.")),
                   hr(),
                   .ctrl_label("Number of clusters (k)"),
                   sliderInput(ns("p6_k"), label = NULL,
                               min = 10, max = 300, value = 80, step = 10, width = "100%"),
                   hr(),
                   .ctrl_label("X axis variable"),
                   selectInput(ns("p6_x"), label = NULL, choices = NULL),
                   .ctrl_label("Y axis variable"),
                   selectInput(ns("p6_y"), label = NULL, choices = NULL),
                   hr(),
                   .ctrl_label("Distance metric"),
                   selectInput(ns("p6_metric"), label = NULL,
                               choices = c("euclidean", "manhattan")),
                   .ctrl_label("Standardise features"),
                   selectInput(ns("p6_stand"), label = NULL,
                               choices = c("TRUE", "FALSE")),
                   hr(),
                   .badge("Diversity down-sampling", "coral"),
                   div(style = "font-size:11px; color:#6c757d;",
                       "Uses ", code("cluster::pam()"))
                 ),
                 mainPanel(
                   width = 9,
                   h4("Down-sample summary"),
                   tableOutput(ns("p6_table")),
                   hr(),
                   h4("Sampled vs not-sampled"),
                   plotOutput(ns("p6_plot"), height = "400px")
                 )
               )
      )
    ) # end tabsetPanel
  )
}


# =================================================================================
# SERVER
# =================================================================================

split_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Shared helpers ────────────────────────────────────────────────────────
    
    # Numeric columns only (for clustering / axis selectors)
    numeric_cols <- reactive({
      df <- get_data()
      names(df)[sapply(df, is.numeric) & names(df) != "Response"]
    })
    
    # Factor columns (for group selector)
    factor_cols <- reactive({
      df <- get_data()
      names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    })
    
    # Populate group variable selector
    observe({
      updateSelectInput(session, "p4_group",
                        choices  = factor_cols(),
                        selected = factor_cols()[1])
    })
    
    # Dynamically cap k slider so it never exceeds n_groups - 1
    observeEvent(input$p4_group, {
      df       <- get_data()
      req(input$p4_group %in% names(df))
      n_groups <- length(unique(df[[input$p4_group]]))
      max_k    <- max(2, n_groups - 1)
      cur_k    <- min(isolate(input$p4_k), max_k)
      updateSliderInput(session, "p4_k",
                        max   = max_k,
                        value = cur_k)
    })
    
    # Populate x/y axis selectors
    observe({
      cols <- numeric_cols()
      updateSelectInput(session, "p6_x", choices = cols, selected = cols[1])
      updateSelectInput(session, "p6_y", choices = cols,
                        selected = if (length(cols) > 1) cols[2] else cols[1])
    })
    
    
    # ── TAB 1: 2-partition ────────────────────────────────────────────────────
    
    p1_split <- reactive({
      df <- get_data()
      set.seed(input$p1_seed)
      if (input$p1_stratify == "stratified") {
        caret::createDataPartition(y = df$Response, p = input$p1_train, list = FALSE)
      } else {
        sample(nrow(df), size = floor(input$p1_train * nrow(df)), replace = FALSE)
      }
    })
    
    output$p1_table <- renderTable({
      df    <- get_data()
      idx   <- p1_split()
      train <- df[idx, ]
      test  <- df[-idx, ]
      data.frame(
        Partition = c("Train", "Test", "Total"),
        N         = c(nrow(train), nrow(test), nrow(df)),
        Proportion = round(c(nrow(train), nrow(test), nrow(df)) / nrow(df), 3),
        `Response mean` = round(c(mean(train$Response, na.rm = TRUE),
                                  mean(test$Response,  na.rm = TRUE),
                                  mean(df$Response,    na.rm = TRUE)), 2),
        check.names = FALSE
      )
    })
    
    output$p1_plot <- renderPlot({
      df  <- get_data()
      idx <- p1_split()
      df$Partition <- ifelse(seq_len(nrow(df)) %in% idx, "Train", "Test")
      ggplot(df, aes(x = Response, fill = Partition)) +
        geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
        scale_fill_manual(values = c("Train" = "#534AB7", "Test" = "#0F6E56")) +
        labs(title = "Response distribution by partition",
             x = "Response", y = "Count") +
        theme_minimal(base_size = 13)
    })
    
    
    # ── TAB 2: 3-partition ────────────────────────────────────────────────────
    
    p2_split <- reactive({
      df <- get_data()
      set.seed(input$p2_seed)
      train_idx <- caret::createDataPartition(y = df$Response,
                                              p = input$p2_train, list = FALSE)
      remainder <- df[-train_idx, ]
      set.seed(input$p2_seed + 1)
      val_idx_in_rem <- caret::createDataPartition(y = remainder$Response,
                                                   p = input$p2_valtest, list = FALSE)
      list(
        train = train_idx,
        val   = as.integer(rownames(remainder)[val_idx_in_rem]),
        test  = as.integer(rownames(remainder)[-val_idx_in_rem])
      )
    })
    
    output$p2_table <- renderTable({
      df  <- get_data()
      idx <- p2_split()
      data.frame(
        Partition  = c("Train", "Validation", "Test", "Total"),
        N          = c(length(idx$train), length(idx$val), length(idx$test), nrow(df)),
        Proportion = round(c(length(idx$train), length(idx$val),
                             length(idx$test), nrow(df)) / nrow(df), 3),
        `Response mean` = round(c(
          mean(df$Response[idx$train], na.rm = TRUE),
          mean(df$Response[idx$val],   na.rm = TRUE),
          mean(df$Response[idx$test],  na.rm = TRUE),
          mean(df$Response,            na.rm = TRUE)
        ), 2),
        check.names = FALSE
      )
    })
    
    output$p2_plot <- renderPlot({
      df  <- get_data()
      idx <- p2_split()
      df$Partition <- "Test"
      df$Partition[idx$train] <- "Train"
      df$Partition[idx$val]   <- "Validation"
      df$Partition <- factor(df$Partition, levels = c("Train", "Validation", "Test"))
      ggplot(df, aes(x = Response, fill = Partition)) +
        geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
        scale_fill_manual(values = c("Train" = "#534AB7",
                                     "Validation" = "#BA7517",
                                     "Test" = "#0F6E56")) +
        labs(title = "Response distribution by partition",
             x = "Response", y = "Count") +
        theme_minimal(base_size = 13)
    })
    
    
    # ── TAB 3: Stratified bootstrap ───────────────────────────────────────────
    
    p3_resamples <- reactive({
      df <- get_data()
      set.seed(input$p3_seed)
      caret::createResample(y = df$Response, times = input$p3_times, list = TRUE)
    })
    
    output$p3_table <- renderTable({
      rs       <- p3_resamples()
      pct_uniq <- sapply(rs, function(x) round(length(unique(x)) / length(x) * 100, 1))
      data.frame(
        Statistic = c("Number of resamples", "Avg % unique obs",
                      "Min % unique obs",    "Max % unique obs"),
        Value     = c(length(rs),
                      round(mean(pct_uniq), 1),
                      round(min(pct_uniq),  1),
                      round(max(pct_uniq),  1))
      )
    })
    
    output$p3_plot <- renderPlot({
      rs       <- p3_resamples()
      pct_uniq <- sapply(rs, function(x) length(unique(x)) / length(x) * 100)
      df_plot  <- data.frame(
        Resample = seq_along(pct_uniq),
        PctUniq  = pct_uniq
      )
      ggplot(df_plot, aes(x = Resample, y = PctUniq)) +
        geom_col(fill = "#534AB7", alpha = 0.8, width = 0.7) +
        geom_hline(yintercept = 63.2, linetype = "dashed", color = "#C41E3A", linewidth = 0.8) +
        annotate("text", x = nrow(df_plot) * 0.85, y = 65,
                 label = "theoretical 63.2%", color = "#C41E3A", size = 3.5) +
        labs(title = "% unique observations per bootstrap resample",
             x = "Resample index", y = "% unique observations") +
        ylim(0, 100) +
        theme_minimal(base_size = 13)
    })
    
    
    # ── TAB 4: Leave group out ────────────────────────────────────────────────
    
    p4_folds <- reactive({
      df       <- get_data()
      req(input$p4_group %in% names(df))
      grp      <- df[[input$p4_group]]
      n_groups <- length(unique(grp))
      req(input$p4_k < n_groups)   # silently wait if k is still too large
      set.seed(input$p4_seed)
      caret::groupKFold(group = grp, k = input$p4_k)
    })
    
    output$p4_table <- renderTable({
      df    <- get_data()
      req(input$p4_group %in% names(df))
      folds <- p4_folds()
      grp   <- as.character(df[[input$p4_group]])
      rows  <- lapply(seq_along(folds), function(i) {
        in_train  <- sort(unique(grp[folds[[i]]]))
        held_out  <- sort(unique(grp[-folds[[i]]]))
        data.frame(
          Fold             = i,
          `Train obs`      = length(folds[[i]]),
          `Held-out obs`   = nrow(df) - length(folds[[i]]),
          `Train groups`   = length(in_train),
          `Held-out groups`= length(held_out),
          check.names = FALSE
        )
      })
      do.call(rbind, rows)
    })
    
    output$p4_plot <- renderPlot({
      df    <- get_data()
      req(input$p4_group %in% names(df))
      folds <- p4_folds()
      grp   <- as.character(df[[input$p4_group]])
      rows  <- lapply(seq_along(folds), function(i) {
        data.frame(
          Fold   = paste("Fold", i),
          Split  = c("Train", "Held-out"),
          Groups = c(length(unique(grp[folds[[i]]])),
                     length(unique(grp[-folds[[i]]])))
        )
      })
      df_plot <- do.call(rbind, rows)
      ggplot(df_plot, aes(x = Fold, y = Groups, fill = Split)) +
        geom_col(position = "dodge", alpha = 0.85) +
        scale_fill_manual(values = c("Train" = "#534AB7", "Held-out" = "#D85A30")) +
        labs(title = paste("Distinct groups per fold — grouped by", input$p4_group),
             x = NULL, y = "Number of distinct groups") +
        theme_minimal(base_size = 13)
    })
    
    
    # ── TAB 5: Time series ────────────────────────────────────────────────────
    
    p5_slices <- reactive({
      df <- get_data()
      n  <- nrow(df)
      req(input$p5_init + input$p5_horizon <= n)
      caret::createTimeSlices(
        y           = seq_len(n),
        initialWindow = input$p5_init,
        horizon       = input$p5_horizon,
        fixedWindow   = as.logical(input$p5_fixed)
      )
    })
    
    output$p5_table <- renderTable({
      sl <- p5_slices()
      n_slices    <- length(sl$train)
      train_sizes <- sapply(sl$train, length)
      test_sizes  <- sapply(sl$test,  length)
      data.frame(
        Statistic = c("Number of slices", "Initial window", "Horizon",
                      "Fixed window", "Min train size", "Max train size"),
        Value     = c(n_slices, input$p5_init, input$p5_horizon,
                      input$p5_fixed,
                      min(train_sizes), max(train_sizes))
      )
    })
    
    output$p5_plot <- renderPlot({
      sl       <- p5_slices()
      n_slices <- length(sl$train)
      # show up to 30 slices to keep chart readable
      show_idx <- if (n_slices > 30) round(seq(1, n_slices, length.out = 30)) else seq_len(n_slices)
      rows <- lapply(show_idx, function(i) {
        rbind(
          data.frame(Slice = i, Index = sl$train[[i]], Type = "Train"),
          data.frame(Slice = i, Index = sl$test[[i]],  Type = "Test")
        )
      })
      df_plot <- do.call(rbind, rows)
      ggplot(df_plot, aes(x = Index, y = factor(Slice), color = Type)) +
        geom_line(linewidth = 3, alpha = 0.7) +
        scale_color_manual(values = c("Train" = "#534AB7", "Test" = "#D85A30")) +
        labs(title = "Time slices — train (blue) and test (orange) windows",
             x = "Observation index (time order)", y = "Slice") +
        theme_minimal(base_size = 12) +
        theme(axis.text.y = element_text(size = 8))
    })
    
    
    # ── TAB 6: Diversity down-sampling ────────────────────────────────────────
    
    p6_clusters <- reactive({
      df   <- get_data()
      # use only complete numeric rows for clustering
      nums <- df[, sapply(df, is.numeric), drop = FALSE]
      nums <- nums[complete.cases(nums), ]
      req(nrow(nums) >= input$p6_k)
      set.seed(42)
      cluster::pam(nums,
                   k      = input$p6_k,
                   metric = input$p6_metric,
                   stand  = as.logical(input$p6_stand))
    })
    
    output$p6_table <- renderTable({
      df  <- get_data()
      cl  <- p6_clusters()
      n   <- nrow(df)
      k   <- input$p6_k
      data.frame(
        Statistic = c("Total observations", "Sampled (medoids)", "Not sampled", "Sample rate"),
        Value     = c(n, k, n - k, paste0(round(k / n * 100, 1), "%"))
      )
    })
    
    output$p6_plot <- renderPlot({
      df  <- get_data()
      cl  <- p6_clusters()
      req(input$p6_x %in% names(df), input$p6_y %in% names(df))
      
      # Build plot df — only rows used in clustering (complete numeric cases)
      nums      <- df[, sapply(df, is.numeric), drop = FALSE]
      comp_rows <- which(complete.cases(nums))
      df_plot   <- df[comp_rows, ]
      df_plot$Type <- "Not sampled"
      df_plot$Type[cl$id.med] <- "Sampled (medoid)"
      
      ggplot(df_plot, aes(x = .data[[input$p6_x]],
                          y = .data[[input$p6_y]],
                          color = Type, size = Type, alpha = Type)) +
        geom_point() +
        scale_color_manual(values = c("Not sampled" = "#aaaaaa",
                                      "Sampled (medoid)" = "#D85A30")) +
        scale_size_manual(values  = c("Not sampled" = 1.5,
                                      "Sampled (medoid)" = 3)) +
        scale_alpha_manual(values = c("Not sampled" = 0.4,
                                      "Sampled (medoid)" = 0.9)) +
        labs(title = paste0("Diversity down-sampling  (k = ", input$p6_k, ")"),
             x = input$p6_x, y = input$p6_y,
             color = NULL, size = NULL, alpha = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
    })
    
    # ── Return train indices ──────────────────────────────────────────────────
    # NOTE: switch values must exactly match the tabPanel() title strings
    return(reactive({
      switch(input$active_tab,
             "2-partition (train + test)"       = p1_split(),
             "3-partition (train + val + test)" = p2_split()$train,
             "Stratified bootstrap"             = p3_resamples()[[1]],
             "Leave group out"                  = p4_folds()[[1]],
             "Time series"                      = p5_slices()$train[[length(p5_slices()$train)]],
             "Diversity down-sampling"          = p6_clusters()$id.med,
             p1_split()  # default fallback — also covers NULLs before a tab is selected
      )
    }))
    
  })
}