# =================================================================================
# mod_split.R
# Split tab module — 6 resampling subtabs, sidebar-right layout
# =================================================================================

# ── LIBRARIES ─────────────────────────────────────────────────────────────────
library(caret)
library(cluster)
library(ggplot2)

# ── SHARED STYLE HELPERS ──────────────────────────────────────────────────────

.sidebar_style <- "background-color: #f4f6fb; border-left: 3px solid #6a9fd8;
                   min-height: 100vh; padding: 16px 14px;"

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

.ctrl_label <- function(text) {
  tags$label(text, style = "font-weight:600; font-size:13px; color:#343a40;")
}

.badge <- function(text, color = "purple") {
  cols <- list(
    purple = "background:#EEEDFE; color:#3C3489;",
    teal   = "background:#E1F5EE; color:#085041;",
    amber  = "background:#FAEEDA; color:#633806;",
    coral  = "background:#FAECE7; color:#712B13;"
  )
  span(text, style = paste0(
    "font-size:11px; padding:2px 8px; border-radius:99px;
     display:inline-block; margin-bottom:6px; ", cols[[color]]))
}

.label_fix <- tags$style(HTML("
  .shiny-input-container label,
  .radio label,
  .checkbox label {
    font-weight: 400 !important;
    font-size: 13px;
  }
"))

.section_head <- function(text) {
  h4(text, style = "border-left: 3px solid #534AB7; padding-left: 8px;
                    font-size:14px; margin-top:16px; margin-bottom:8px;")
}


# =================================================================================
# UI
# =================================================================================

split_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    .label_fix,
    tabsetPanel(
      id   = ns("active_tab"),
      type = "pills",
      
      
      # ── TAB 1: 2-partition ────────────────────────────────────────────────
      tabPanel("2-partition (train + test)",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .sidebar_style,
                   .info_box(HTML("<strong>Tab note:</strong><br>
              Compares simple random sampling vs stratified random sampling.
              Stratification uses quartiles of the outcome to preserve its
              distribution across partitions.")),
                   hr(),
                   .ctrl_label("Train proportion"),
                   sliderInput(ns("p1_train"), label = NULL,
                               min = 0.5, max = 0.95, value = 0.8,
                               step = 0.05, width = "100%"),
                   hr(),
                   .ctrl_label("Stratify by (y =)"),
                   selectInput(ns("p1_y"), label = NULL, choices = NULL),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p1_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Independent observations", "purple"),
                   div(style = "font-size:11px; color:#6c757d; margin-top:4px;",
                       "Uses ", code("base::sample()"), " and ",
                       code("caret::createDataPartition()")),
                   hr(),
                   actionButton(ns("p1_apply"), "Apply this split",
                                icon = icon("check"), width = "100%",
                                style = "background:#534AB7; color:white; border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   uiOutput(ns("p1_applied")),
                   .section_head("Full data — outcome frequency table"),
                   verbatimTextOutput(ns("p1_full_table")),
                   hr(),
                   .section_head("Simple random sampling — train frequency table"),
                   verbatimTextOutput(ns("p1_simple_table")),
                   hr(),
                   .section_head("Stratified random sampling — train frequency table"),
                   verbatimTextOutput(ns("p1_strat_table")),
                   hr(),
                   .section_head("Partition sizes"),
                   tableOutput(ns("p1_sizes"))
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
                   .info_box(HTML("<strong>Tab note:</strong><br>
              Two-stage stratified split: first extracts train, then divides
              the remainder into validation + test. Observation counts are
              printed for each partition.")),
                   hr(),
                   .ctrl_label("Train proportion"),
                   sliderInput(ns("p2_train"), label = NULL,
                               min = 0.4, max = 0.8, value = 0.6,
                               step = 0.05, width = "100%"),
                   .ctrl_label("Validation proportion (of remainder)"),
                   sliderInput(ns("p2_val"), label = NULL,
                               min = 0.1, max = 0.9, value = 0.5,
                               step = 0.05, width = "100%"),
                   hr(),
                   .ctrl_label("Stratify by (y =)"),
                   selectInput(ns("p2_y"), label = NULL, choices = NULL),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p2_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Independent observations", "purple"),
                   div(style = "font-size:11px; color:#6c757d; margin-top:4px;",
                       "Uses ", code("caret::createDataPartition()"), " twice"),
                   hr(),
                   actionButton(ns("p2_apply"), "Apply this split",
                                icon = icon("check"), width = "100%",
                                style = "background:#534AB7; color:white; border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   uiOutput(ns("p2_applied")),
                   .section_head("Partition observation counts"),
                   verbatimTextOutput(ns("p2_counts")),
                   hr(),
                   .section_head("Partition sizes summary"),
                   tableOutput(ns("p2_sizes"))
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
                   .info_box(HTML("<strong>Tab note:</strong><br>
              Stratified bootstrap draws samples with replacement, preserving
              outcome distribution. Shows the structure of the resamples via
              <code>str()</code> and the % unique observations per resample
              via <code>calcPerc()</code>.")),
                   hr(),
                   .ctrl_label("Number of resamples"),
                   sliderInput(ns("p3_times"), label = NULL,
                               min = 5, max = 100, value = 25,
                               step = 5, width = "100%"),
                   hr(),
                   .ctrl_label("Stratify by (y =)"),
                   selectInput(ns("p3_y"), label = NULL, choices = NULL),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p3_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Independent observations", "purple"),
                   div(style = "font-size:11px; color:#6c757d; margin-top:4px;",
                       "Uses ", code("caret::createResample()")),
                   hr(),
                   actionButton(ns("p3_apply"), "Apply this split",
                                icon = icon("check"), width = "100%",
                                style = "background:#534AB7; color:white; border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   uiOutput(ns("p3_applied")),
                   .section_head("str(resamples) — resample structure"),
                   verbatimTextOutput(ns("p3_str")),
                   hr(),
                   .section_head("calcPerc() — % unique observations per resample"),
                   verbatimTextOutput(ns("p3_perc"))
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
                   .info_box(HTML("<strong>Tab note:</strong><br>
              Ensures no group appears in both train and test. Shows the fold
              structure, and for Fold 1: groups in train, groups held out,
              and distinct group counts — mirroring the tutorial outputs.")),
                   hr(),
                   .ctrl_label("Group variable"),
                   selectInput(ns("p4_group"), label = NULL, choices = NULL),
                   hr(),
                   .ctrl_label("Number of folds (k)"),
                   sliderInput(ns("p4_k"), label = NULL,
                               min = 2, max = 3, value = 2,
                               step = 1, width = "100%"),
                   hr(),
                   .ctrl_label("Random seed"),
                   numericInput(ns("p4_seed"), label = NULL, value = 199, min = 1),
                   hr(),
                   .badge("Group dependent observations", "teal"),
                   div(style = "font-size:11px; color:#6c757d; margin-top:4px;",
                       "Uses ", code("caret::groupKFold()")),
                   hr(),
                   actionButton(ns("p4_apply"), "Apply this split",
                                icon = icon("check"), width = "100%",
                                style = "background:#0F6E56; color:white; border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   uiOutput(ns("p4_applied")),
                   .section_head("Group frequency table"),
                   verbatimTextOutput(ns("p4_freq_table")),
                   hr(),
                   .section_head("Fold structure — folds list"),
                   verbatimTextOutput(ns("p4_folds_print")),
                   hr(),
                   .section_head("Fold 1 — groups in train (inGroups)"),
                   verbatimTextOutput(ns("p4_in_groups")),
                   verbatimTextOutput(ns("p4_in_distinct")),
                   hr(),
                   .section_head("Fold 1 — groups held out (outGroups)"),
                   verbatimTextOutput(ns("p4_out_groups")),
                   verbatimTextOutput(ns("p4_out_distinct"))
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
                   .info_box(HTML("<strong>Tab note:</strong><br>
              Train always precedes test in time — no future leakage.
              Shows <code>str()</code> of train and test slice lists,
              matching the tutorial output exactly.")),
                   hr(),
                   .ctrl_label("Time variable (sort order)"),
                   selectInput(ns("p5_time"), label = NULL, choices = NULL),
                   hr(),
                   .ctrl_label("Initial window"),
                   sliderInput(ns("p5_init"), label = NULL,
                               min = 2, max = 50, value = 5,
                               step = 1, width = "100%"),
                   .ctrl_label("Horizon"),
                   sliderInput(ns("p5_horizon"), label = NULL,
                               min = 1, max = 20, value = 3,
                               step = 1, width = "100%"),
                   hr(),
                   .ctrl_label("Fixed window"),
                   selectInput(ns("p5_fixed"), label = NULL,
                               choices = c("FALSE (expanding)" = "FALSE",
                                           "TRUE  (fixed)"     = "TRUE")),
                   hr(),
                   .ctrl_label("Skip (slices between each window)"),
                   sliderInput(ns("p5_skip"), label = NULL,
                               min = 0, max = 10, value = 0,
                               step = 1, width = "100%"),
                   hr(),
                   .badge("Time dependent observations", "amber"),
                   div(style = "font-size:11px; color:#6c757d; margin-top:4px;",
                       "Uses ", code("caret::createTimeSlices()")),
                   hr(),
                   actionButton(ns("p5_apply"), "Apply this split",
                                icon = icon("check"), width = "100%",
                                style = "background:#BA7517; color:white; border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   uiOutput(ns("p5_applied")),
                   .section_head("str(tsSamples$train) — train slice structure"),
                   verbatimTextOutput(ns("p5_train_str")),
                   hr(),
                   .section_head("str(tsSamples$test) — test slice structure"),
                   verbatimTextOutput(ns("p5_test_str"))
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
                   .info_box(HTML("<strong>Tab note:</strong><br>
              K-medoids clustering selects one representative (medoid) from
              each cluster as the training sample — maximising feature-space
              coverage. Scatter plot coloured by sampled vs not sampled,
              matching the tutorial plot.")),
                   hr(),
                   .ctrl_label("Number of clusters (k)"),
                   sliderInput(ns("p6_k"), label = NULL,
                               min = 10, max = 300, value = 80,
                               step = 10, width = "100%"),
                   hr(),
                   .ctrl_label("Clustering features"),
                   selectizeInput(ns("p6_features"), label = NULL,
                                  choices = NULL, multiple = TRUE,
                                  options = list(placeholder = "All numeric by default")),
                   hr(),
                   .ctrl_label("X axis variable (plot only)"),
                   selectInput(ns("p6_x"), label = NULL, choices = NULL),
                   .ctrl_label("Y axis variable (plot only)"),
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
                   div(style = "font-size:11px; color:#6c757d; margin-top:4px;",
                       "Uses ", code("cluster::pam()")),
                   hr(),
                   actionButton(ns("p6_apply"), "Apply this split",
                                icon = icon("check"), width = "100%",
                                style = "background:#993C1D; color:white; border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   uiOutput(ns("p6_applied")),
                   .section_head("Sampled vs not-sampled — scatter plot"),
                   plotOutput(ns("p6_plot"), height = "450px"),
                   hr(),
                   .section_head("Down-sample summary"),
                   tableOutput(ns("p6_table"))
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
    
    numeric_cols <- reactive({
      df <- get_data()
      names(df)[sapply(df, is.numeric) & names(df) != "Response"]
    })
    
    factor_cols <- reactive({
      df <- get_data()
      names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    })
    
    # All columns (for stratify-by selectors)
    all_cols <- reactive({
      names(get_data())
    })
    
    # Date/time columns (for time series tab)
    date_cols <- reactive({
      df <- get_data()
      names(df)[sapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")))]
    })
    
    observe({
      cols <- date_cols()
      updateSelectInput(session, "p5_time",
                        choices  = cols,
                        selected = if (length(cols) > 0) cols[1] else NULL)
    })
    
    # Populate stratify-by selectors — default to Response
    observe({
      cols <- all_cols()
      for (id in c("p1_y", "p2_y", "p3_y")) {
        updateSelectInput(session, id,
                          choices  = cols,
                          selected = if ("Response" %in% cols) "Response" else cols[1])
      }
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
      updateSliderInput(session, "p4_k", max = max_k, value = cur_k)
    })
    
    # Populate x/y axis selectors and feature selector
    observe({
      cols <- numeric_cols()
      updateSelectInput(session, "p6_x", choices = cols, selected = cols[1])
      updateSelectInput(session, "p6_y", choices = cols,
                        selected = if (length(cols) > 1) cols[2] else cols[1])
      # default: all numeric columns selected for clustering
      updateSelectizeInput(session, "p6_features",
                           choices  = cols,
                           selected = cols)
    })
    
    
    # ── TAB 1: 2-partition ────────────────────────────────────────────────────
    
    # simple random train indices
    p1_simple <- reactive({
      df <- get_data()
      set.seed(input$p1_seed)
      base::sample(x = nrow(df), size = floor(input$p1_train * nrow(df)), replace = FALSE)
    })
    
    # stratified train indices — uses user-selected y variable
    p1_strat <- reactive({
      df <- get_data()
      req(input$p1_y %in% names(df))
      set.seed(input$p1_seed)
      caret::createDataPartition(y = df[[input$p1_y]], p = input$p1_train, list = FALSE)
    })
    
    # helper: frequency table for a column (numeric -> quartile bins, factor -> levels)
    .freq_table <- function(vec) {
      if (is.numeric(vec)) {
        table(cut(vec, breaks = quantile(vec, na.rm = TRUE), include.lowest = TRUE))
      } else {
        table(as.factor(vec))
      }
    }
    
    # full data frequency table
    output$p1_full_table <- renderPrint({
      df <- get_data()
      req(input$p1_y %in% names(df))
      cat(input$p1_y, "groups (full data):\n")
      print(.freq_table(df[[input$p1_y]]))
    })
    
    # simple random — train frequency table
    output$p1_simple_table <- renderPrint({
      df  <- get_data()
      req(input$p1_y %in% names(df))
      idx <- p1_simple()
      cat(input$p1_y, "groups (simple random — train):\n")
      print(.freq_table(df[[input$p1_y]][idx]))
    })
    
    # stratified — train frequency table
    output$p1_strat_table <- renderPrint({
      df  <- get_data()
      req(input$p1_y %in% names(df))
      idx <- p1_strat()
      cat(input$p1_y, "groups (stratified — train):\n")
      print(.freq_table(df[[input$p1_y]][idx]))
    })
    
    # partition sizes summary table
    output$p1_sizes <- renderTable({
      df    <- get_data()
      idx   <- p1_strat()
      n     <- nrow(df)
      data.frame(
        Partition  = c("Train", "Test", "Total"),
        N          = c(length(idx), n - length(idx), n),
        Proportion = round(c(length(idx), n - length(idx), n) / n, 3),
        check.names = FALSE
      )
    })
    
    
    # ── TAB 2: 3-partition ────────────────────────────────────────────────────
    
    p2_split <- reactive({
      df <- get_data()
      req(input$p2_y %in% names(df))
      set.seed(input$p2_seed)
      train_idx <- caret::createDataPartition(y = df[[input$p2_y]],
                                              p = input$p2_train, list = FALSE)
      remainder <- df[-train_idx, ]
      set.seed(input$p2_seed + 1)
      val_idx_in_rem <- caret::createDataPartition(y = remainder[[input$p2_y]],
                                                   p = input$p2_val, list = FALSE)
      list(
        train = train_idx,
        val   = as.integer(rownames(remainder)[val_idx_in_rem]),
        test  = as.integer(rownames(remainder)[-val_idx_in_rem])
      )
    })
    
    # tutorial-style cat() output
    output$p2_counts <- renderPrint({
      idx <- p2_split()
      cat("There are", length(idx$train), "observations in the train data\n")
      cat("There are", length(idx$val),   "observations in the validation data\n")
      cat("There are", length(idx$test),  "observations in the test data\n")
    })
    
    output$p2_sizes <- renderTable({
      df  <- get_data()
      idx <- p2_split()
      n   <- nrow(df)
      data.frame(
        Partition  = c("Train", "Validation", "Test", "Total"),
        N          = c(length(idx$train), length(idx$val), length(idx$test), n),
        Proportion = round(c(length(idx$train), length(idx$val),
                             length(idx$test), n) / n, 3),
        check.names = FALSE
      )
    })
    
    
    # ── TAB 3: Stratified bootstrap ───────────────────────────────────────────
    
    p3_resamples <- reactive({
      df <- get_data()
      req(input$p3_y %in% names(df))
      set.seed(input$p3_seed)
      caret::createResample(y = df[[input$p3_y]], times = input$p3_times, list = TRUE)
    })
    
    # str(resamples) — exact tutorial output
    output$p3_str <- renderPrint({
      rs <- p3_resamples()
      str(rs)
    })
    
    # calcPerc — exact tutorial output
    output$p3_perc <- renderPrint({
      rs <- p3_resamples()
      calcPerc <- function(x) {
        unqCount <- length(unique(x))
        propUnq  <- unqCount / length(x)
        cat("Unique =", round(propUnq * 100), "%\n")
      }
      invisible(mapply(rs, FUN = calcPerc))
    })
    
    
    # ── TAB 4: Leave group out ────────────────────────────────────────────────
    
    p4_folds <- reactive({
      df       <- get_data()
      req(input$p4_group %in% names(df))
      grp      <- df[[input$p4_group]]
      n_groups <- length(unique(grp))
      req(input$p4_k < n_groups)   # guard: silently wait if k still too large
      set.seed(input$p4_seed)
      caret::groupKFold(group = grp, k = input$p4_k)
    })
    
    # table(as.factor(group)) — tutorial line
    output$p4_freq_table <- renderPrint({
      df  <- get_data()
      req(input$p4_group %in% names(df))
      grp <- df[[input$p4_group]]
      print(table(as.factor(grp)))
    })
    
    # print(folds) — tutorial line
    output$p4_folds_print <- renderPrint({
      print(p4_folds())
    })
    
    # inGroups for Fold1 — tutorial lines
    output$p4_in_groups <- renderPrint({
      df    <- get_data()
      req(input$p4_group %in% names(df))
      folds <- p4_folds()
      grp   <- as.character(df[[input$p4_group]])
      inGroups <- sort(grp[folds[[1]]])
      print(inGroups)
    })
    
    output$p4_in_distinct <- renderPrint({
      df    <- get_data()
      req(input$p4_group %in% names(df))
      folds <- p4_folds()
      grp   <- as.character(df[[input$p4_group]])
      inGroups <- sort(grp[folds[[1]]])
      cat("Distinct groups in train for Fold1:", length(unique(inGroups)), "\n")
    })
    
    # outGroups for Fold1 — tutorial lines
    output$p4_out_groups <- renderPrint({
      df    <- get_data()
      req(input$p4_group %in% names(df))
      folds <- p4_folds()
      grp   <- as.character(df[[input$p4_group]])
      outGroups <- sort(grp[-folds[[1]]])
      print(outGroups)
    })
    
    output$p4_out_distinct <- renderPrint({
      df    <- get_data()
      req(input$p4_group %in% names(df))
      folds <- p4_folds()
      grp   <- as.character(df[[input$p4_group]])
      outGroups <- sort(grp[-folds[[1]]])
      cat("Distinct groups in test for Fold1:", length(unique(outGroups)), "\n")
    })
    
    
    # ── TAB 5: Time series ────────────────────────────────────────────────────
    
    p5_slices <- reactive({
      df <- get_data()
      req(input$p5_time %in% names(df))
      # sort by time variable — row order must reflect time order for slices to be meaningful
      df <- df[order(df[[input$p5_time]]), ]
      n  <- nrow(df)
      req(input$p5_init + input$p5_horizon <= n)
      caret::createTimeSlices(
        y             = seq_len(n),
        initialWindow = input$p5_init,
        horizon       = input$p5_horizon,
        fixedWindow   = as.logical(input$p5_fixed),
        skip          = input$p5_skip
      )
    })
    
    # print(str(tsSamples$train)) — exact tutorial output
    output$p5_train_str <- renderPrint({
      sl <- p5_slices()
      print(str(sl$train))
    })
    
    # print(str(tsSamples$test)) — exact tutorial output
    output$p5_test_str <- renderPrint({
      sl <- p5_slices()
      print(str(sl$test))
    })
    
    
    # ── TAB 6: Diversity down-sampling ────────────────────────────────────────
    
    p6_clusters <- reactive({
      df   <- get_data()
      # use user-selected features; fall back to all numeric if none selected
      feat_cols <- if (length(input$p6_features) > 0) {
        intersect(input$p6_features, names(df))
      } else {
        names(df)[sapply(df, is.numeric)]
      }
      req(length(feat_cols) > 0)
      nums <- df[, feat_cols, drop = FALSE]
      nums <- nums[complete.cases(nums), ]
      req(nrow(nums) >= input$p6_k)
      set.seed(199)
      cluster::pam(nums,
                   k      = input$p6_k,
                   metric = input$p6_metric,
                   stand  = as.logical(input$p6_stand))
    })
    
    # ggplot geom_point coloured by Type — matches tutorial exactly
    output$p6_plot <- renderPlot({
      df  <- get_data()
      cl  <- p6_clusters()
      req(input$p6_x %in% names(df), input$p6_y %in% names(df))
      
      nums      <- df[, sapply(df, is.numeric), drop = FALSE]
      comp_rows <- which(complete.cases(nums))
      df_plot   <- df[comp_rows, ]
      df_plot$Type <- NA
      df_plot$Type[-cl$id.med] <- "not sampled"
      df_plot$Type[cl$id.med]  <- "sampled"
      
      ggplot() +
        geom_point(data = df_plot,
                   mapping = aes(x = .data[[input$p6_x]],
                                 y = .data[[input$p6_y]],
                                 color = Type)) +
        labs(title = paste0("Data (sampled for diversity, k = ", input$p6_k, ")"),
             x = input$p6_x, y = input$p6_y) +
        theme_minimal(base_size = 13)
    })
    
    output$p6_table <- renderTable({
      df <- get_data()
      cl <- p6_clusters()
      n  <- nrow(df)
      k  <- input$p6_k
      data.frame(
        Statistic = c("Total observations", "Sampled (medoids)",
                      "Not sampled", "Sample rate"),
        Value     = c(n, k, n - k, paste0(round(k / n * 100, 1), "%"))
      )
    })
    
    
    # ── Apply-button-driven return ───────────────────────────────────────────────
    # Each Apply button freezes the current tab's indices into applied_indices.
    # Exploring other tabs does NOT change what downstream models use.
    
    applied_indices <- reactiveVal({
      # initialise with 2-partition stratified split so app is usable immediately
      df <- isolate(get_data())
      set.seed(199)
      as.integer(caret::createDataPartition(y = df$Response, p = 0.8, list = FALSE))
    })
    
    applied_label <- reactiveVal("2-partition (train + test) — default")
    
    # helper to show a notification confirming which split was applied
    .notify_applied <- function(label, n_train, n_test) {
      showNotification(
        paste0("Split applied: ", label,
               "  |  Train: ", n_train, "  Test: ", n_test),
        type = "message", duration = 4
      )
    }
    
    observeEvent(input$p1_apply, {
      idx <- isolate(p1_strat())
      n   <- isolate(nrow(get_data()))
      applied_indices(as.integer(idx))
      applied_label("2-partition (train + test)")
      .notify_applied("2-partition", length(idx), n - length(idx))
    })
    
    observeEvent(input$p2_apply, {
      idx <- isolate(p2_split()$train)
      n   <- isolate(nrow(get_data()))
      applied_indices(as.integer(idx))
      applied_label("3-partition (train + val + test)")
      .notify_applied("3-partition — train set", length(idx), n - length(idx))
    })
    
    observeEvent(input$p3_apply, {
      idx <- isolate(p3_resamples()[[1]])
      n   <- isolate(nrow(get_data()))
      applied_indices(as.integer(unique(idx)))  # unique — bootstrap has duplicates
      applied_label("Stratified bootstrap (resample 1)")
      .notify_applied("Stratified bootstrap", length(unique(idx)), n - length(unique(idx)))
    })
    
    observeEvent(input$p4_apply, {
      idx <- isolate(p4_folds()[[1]])
      n   <- isolate(nrow(get_data()))
      applied_indices(as.integer(idx))
      applied_label("Leave group out (Fold 1)")
      .notify_applied("Leave group out", length(idx), n - length(idx))
    })
    
    observeEvent(input$p5_apply, {
      sl  <- isolate(p5_slices())
      idx <- sl$train[[length(sl$train)]]  # last (largest) train slice
      n   <- isolate(nrow(get_data()))
      applied_indices(as.integer(idx))
      applied_label("Time series (last train slice)")
      .notify_applied("Time series", length(idx), n - length(idx))
    })
    
    observeEvent(input$p6_apply, {
      idx <- isolate(p6_clusters()$id.med)
      n   <- isolate(nrow(get_data()))
      applied_indices(as.integer(idx))
      applied_label("Diversity down-sampling (medoids)")
      .notify_applied("Diversity down-sampling", length(idx), n - length(idx))
    })
    
    # show applied label in each tab's main panel
    output$p1_applied <- output$p2_applied <- output$p3_applied <-
      output$p4_applied <- output$p5_applied <- output$p6_applied <-
      renderUI({
        div(
          style = "font-size:12px; color:white; background:#534AB7;
                   padding:6px 10px; border-radius:5px; margin-bottom:10px;
                   display:inline-block;",
          icon("circle-check"), HTML("&nbsp;"),
          paste("Currently applied:", applied_label())
        )
      })
    
    return(reactive({ applied_indices() }))
    
  })
}