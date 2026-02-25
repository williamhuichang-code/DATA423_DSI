# =============================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =============================================================================

# Note: 
#   global file defines features related to dataset
#   UI, server files should be universally applicable

library(shiny)
library(dplyr)
library(vcd)
library(DT)

# load the raw dataset
raw_dataset <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = FALSE)

# create a copy of raw dataset (R use copy-on-modify)
ds_copied <- raw_dataset

# standardise all the col names with initial capital naming style
ds_renamed <- ds_copied %>% 
  rename_with(~ tools::toTitleCase(.x))  # e.g., sensors have bad names

# master column order — all datasets follow this, skipping cols that don't exist
master_col_order <- c(
  # target
  "Y",
  # identifiers and operators
  "IdGroup", "ID", "Operator", 
  # date and derived factors
  "Date", "Year", "Season", "Month", "Day",
  # context
  "Priority", "Price", "Speed", "Duration", "Temp",
  "Location", "Agreed", "State", "Class", "Surface",
  # raw sensors
  paste0("Sensor", 1:30),
  # sensor group 1 (averaged raw)
  "SensorGroup1", "SensorGroup2", "SensorGroup3",
  # centred sensor group (subtract mean)
  "CenterSG1", "CenterSG2", "CenterSG3",
  # standardised sensor group (z-score)
  "StandardisedSG1", "StandardisedSG2", "StandardisedSG3",
  # normalised sensor group (min-max)
  "NormalisedSG1", "NormalisedSG2", "NormalisedSG3"
)

# helper function: reorder a df by master order, skipping missing cols
reorder_cols <- function(df) {
  keep <- master_col_order[master_col_order %in% names(df)]  # only cols that exist
  df[, keep]
}


# validate dataset with schema
# - predefine a schema
schema <- list(
  numeric_cols = c("Y", grep("^Sensor", names(ds_copied), value = TRUE)),
  date_cols    = "Date",
  factor_cols  = c("ID", "Operator", "Priority", "Price",
                   "Speed", "Duration", "Temp", "Location",
                   "Agreed", "State", "Class", "Surface")
)

# - explicitly apply proper schema to our dataset, and solve name issues
ds_typed <- ds_renamed %>%
  
  # - assigning schema
  mutate(
    across(all_of(schema$numeric_cols), as.numeric),
    across(all_of(schema$factor_cols),  as.factor),
    across(all_of(schema$date_cols),    as.Date)
  ) %>% 
  
  # - manually defining level order for potential ordinal factors
  # - (has to be strict, since no context provided)
  mutate(
    # Priority = factor(Priority, levels = c("Low", "Medium", "High"),      ordered = TRUE),
    # Price    = factor(Price,    levels = c("Expensive", "Fair", "Cheap"), ordered = TRUE),
    # Speed    = factor(Speed,    levels = c("Slow", "Medium", "Fast"),     ordered = TRUE),
    # Duration = factor(Duration, levels = c("Short", "Long", "Very Long"), ordered = TRUE),
    # Temp     = factor(Temp,     levels = c("Cold", "Warm", "Hot"),        ordered = TRUE),
    Agreed   = factor(Agreed,   levels = c("No", "Yes"))
  )


# enrich the dataset with new cols
ds_col_enriched <- ds_typed %>%
  
  # - derive IdGroup, ID values start with significant pattern of G type and D type
  mutate(
    IdGroup = as.factor(substr(as.character(ID), 1, 1)),  # ID group info subtracted from the 1st letter in ID
  ) %>% 
  
  # - derive meaningful cyclic info from Date, including Year, Season, Month, Day
  mutate(
    
    # many years use numeric, for its order, real distance meaning, good for correlation / time trend plots
    # few years prefer factor, purely categorical regimes, for barchart comparison
    # month stays categorical, we do not assume January < February < March in a monotonic sense
    # day mostly noise, but just curious about its day-wise distribution in histogram, so better numeric
    Year   = factor(format(Date, "%Y")),
    Month  = as.integer(format(Date, "%m")),
    Day    = as.integer(format(Date, "%d")),
    Season = factor(
      case_when(
        Month %in% 1:3   ~ "S1",
        Month %in% 4:6   ~ "S2",
        Month %in% 7:9   ~ "S3",
        Month %in% 10:12 ~ "S4"
      )),
    Month  = factor(Month)
  ) %>% 
  
  # - sensor patterns, including grouping and feature scaling
  mutate(
    # - sensor groups based on boxplot patterns
    SensorGroup1 = as.numeric(rowMeans(across(Sensor1:Sensor10), na.rm = TRUE)),   # dimensionality reduction with sensor groups
    SensorGroup2 = as.numeric(rowMeans(across(Sensor11:Sensor20), na.rm = TRUE)),  # based on value pattern similarities across sensors
    SensorGroup3 = as.numeric(rowMeans(across(Sensor21:Sensor30), na.rm = TRUE)),  # making it a perfect choice and can handle NAs
    # - centering (subtract mean, mean becomes 0, spread preserved)
    CenterSG1 = SensorGroup1 - mean(SensorGroup1, na.rm = TRUE),
    CenterSG2 = SensorGroup2 - mean(SensorGroup2, na.rm = TRUE),
    CenterSG3 = SensorGroup3 - mean(SensorGroup3, na.rm = TRUE),
    # - Z-score standardisation (mean=0, SD=1)
    StandardisedSG1 = as.numeric(scale(SensorGroup1, center = TRUE, scale = TRUE)),
    StandardisedSG2 = as.numeric(scale(SensorGroup2, center = TRUE, scale = TRUE)),
    StandardisedSG3 = as.numeric(scale(SensorGroup3, center = TRUE, scale = TRUE)),
    # - min-max normalisation (rescales to [0, 1])
    NormalisedSG1 = (SensorGroup1 - min(SensorGroup1, na.rm = TRUE)) /
      (max(SensorGroup1, na.rm = TRUE) - min(SensorGroup1, na.rm = TRUE)),
    NormalisedSG2 = (SensorGroup2 - min(SensorGroup2, na.rm = TRUE)) /
      (max(SensorGroup2, na.rm = TRUE) - min(SensorGroup2, na.rm = TRUE)),
    NormalisedSG3 = (SensorGroup3 - min(SensorGroup3, na.rm = TRUE)) /
      (max(SensorGroup3, na.rm = TRUE) - min(SensorGroup3, na.rm = TRUE))
  )

# enrich the dataset by examining proper rows (place holder)
#  - could be clean dataset, since we don't want NA in some cases (place holder)
enriched_dataset <- ds_col_enriched %>% 
  reorder_cols()

# model dataset, since we don't need ID, Date and maybe Sensor1-30 as well for further modelling
model_dataset <- enriched_dataset %>% 
  select(-ID, -Date, -matches("^Sensor\\d+$")) %>%   # drop those cols not needed for modelling
  reorder_cols()
  
# model_dataset <- model_dataset %>% select(-IdGroup, -Year, -Month) # trial for variable testing


# =============================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =============================================================================

# UI
ui <- fluidPage(
  
  # ── GLOBAL DS DROPDOWN ─────────────────────────────────────────────────────
  
  # global dropdown choces for dataset at different stages
  fluidRow(
    column(2, selectInput("dataset_choice", "Choose Dataset Stage:",
                          choices = c("Raw Dataset", "Enriched Dataset", "Model Dataset"),
                          selected = "Enriched Dataset")),
    column(1, actionButton("dataset_info", label = NULL,
                           icon  = icon("circle-info"),
                           style = "margin-top: 25px; font-size: 20px;
                                color: #0d6efd; background: none;
                                border: none; padding: 0;"))
  ),
  
  
  # ── TABS ───────────────────────────────────────────────────────────────────
  
  # tabs panels
  tabsetPanel(
    
    # ── TAB TABLE ────────────────────────────────────────────────────────────
    
    tabPanel("Data Table",   DT::dataTableOutput("data_table")),  # end of tab panel
    
    
    # ── TAB SUMMARY ──────────────────────────────────────────────────────────
    
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(width = 2,
                            radioButtons("summary_style", "Summary Style:",
                                         choices = c("Base R"      = "base",
                                                     "Glimpse"     = "glimpse",
                                                     "dfSummary"   = "dfsummary"),
                                         selected = "glimpse")
               ),
               mainPanel(width = 10,
                         verbatimTextOutput("data_summary")
               )
             )
    ),  # end of tab panel
    
    
    # ── TAB MISSINGNESS ──────────────────────────────────────────────────────
    
    tabPanel("Missingness",
             sidebarLayout(
               sidebarPanel(width = 3,
                            
                            textInput("vc_value", "Value to count:",
                                      value = "NA",
                                      placeholder = "e.g. Yes, High, NA"),
                            
                            checkboxInput("vc_case", "Case sensitive", value = FALSE),
                            
                            selectizeInput("vc_cols", "Search in columns:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            
                            selectInput("vc_metric", "Show as:",
                                        choices = c("Raw Count" = "count", "Percentage (%)" = "pct")),
                            
                            hr(),
                            checkboxInput("vc_sort", "Sort by value (descending)", value = TRUE)
                            
               ),
               mainPanel(width = 9,
                         plotOutput("vc_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── TAB RISING ORDER ─────────────────────────────────────────────────────
    
    tabPanel("Rising Order",
             sidebarLayout(
               sidebarPanel(width = 2,
                            selectizeInput("rising_var",
                                           "Select numeric variable:",
                                           choices = NULL,
                                           multiple = TRUE)
               ),
               mainPanel(width = 10,
                         plotOutput("rising_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── TAB Q-Q PLOT ──────────────────────────────────────────────────────────
    
    tabPanel("Q-Q Plot",
             sidebarLayout(
               sidebarPanel(width = 3,
                            selectInput("qq_var", "Select Variable:", choices = NULL),
                            selectInput("qq_dist", "Distribution:", 
                                        choices = c("Normal" = "norm", 
                                                    "Exponential" = "exp", 
                                                    "Log-normal" = "lnorm", 
                                                    "Gamma" = "gamma", 
                                                    "Weibull" = "weibull")),
                            hr(),
                            h4("Distribution Parameters"),
                            uiOutput("qq_params"), # dynamic UI for parameters
                            hr(),
                            checkboxInput("qq_line", "Show Reference Line", value = TRUE)
               ),
               mainPanel(width = 9,
                         plotOutput("qq_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── TAB MOSAIC ───────────────────────────────────────────────────────────
    
    tabPanel("Mosaic",
             sidebarLayout(
               sidebarPanel(width = 2,   # 2/12 = ~17%, narrow sidebar
                 
                 # side bar for mosaic plot controls
                 uiOutput("mosaic_x_ui"),
                 uiOutput("mosaic_y_ui"),
                 uiOutput("mosaic_z_ui"),
                 checkboxInput("mosaic_shade", "Shade (colour by residuals)", value = TRUE),
                 
                 hr(),
                 
                 # side bar for mosaic pair advisor controls
                 strong("Pair Advisor"),
                 br(),
                 helpText("Ranks variable combinations by Cramér's V (effect size). Higher = stronger association."),
                 br(),
                 radioButtons("pairs_way", "Combinations:",
                              choices = c("2-way", "3-way"), selected = "2-way",
                              inline = TRUE),
                 numericInput("pairs_top", "Show top N:", value = 15, min = 5, max = 200),
                 actionButton("pairs_search", "Find Pairs", icon = icon("search"), width = "100%"),
                 br(), br(),
                 helpText("Click a row to load variables into the plot above.")
               ),
               
               mainPanel(
                 width = 10,     # must add up to 12
                 
                 # mosaic plotting
                 plotOutput("mosaic_plot", height = "90vh"),
                 
                 hr(),
                 
                 # pair advisor results (hidden until Search clicked)
                 conditionalPanel(
                   condition = "input.pairs_search > 0",
                   h4("Pair Advisor Results — ranked by Cramér's V"),
                   helpText("Cramér's V: 0.1 = weak, 0.3 = moderate, 0.5+ = strong. Not inflated by sample size."),
                   helpText("3-way score = average Cramér's V across the 3 possible pairs within the trio."),
                   DTOutput("pairs_table")
                 )
               )
             )
    ),  # end of tab panel
    
    
    # ── TAB GGPAIRS ──────────────────────────────────────────────────────────
    
    tabPanel("GGPairs",      p("Coming soon")),  # end of tab panel
    
    
    # ── TAB CORRELATION ──────────────────────────────────────────────────────
    
    tabPanel("Correlation",  p("Coming soon")),  # end of tab panel
    
    
    # ── TAB BOXPLOT ──────────────────────────────────────────────────────────
    
    tabPanel("Boxplot",      p("Coming soon")),  # end of tab panel
    
    
    # ── TAB OTHERS ───────────────────────────────────────────────────────────
    
    tabPanel("Others",  p("Coming soon")),  # end of tab panel
    
    
  ) # end tabsetPanel
)   # end fluidPage


# =============================================================================
# server.R
# =============================================================================

# server
server <- function(input, output, session) {
  
  # ── GLOBAL REACTIVE ────────────────────────────────────────────────────────
  
  # switch dataset based on drop down
  selected_data <- reactive({
    switch(input$dataset_choice,
           "Raw Dataset"      = raw_dataset,
           "Enriched Dataset" = enriched_dataset,
           "Model Dataset"  = model_dataset,
           NULL)
    })
  
  observeEvent(input$dataset_info, {
    showModal(modalDialog(
      title = "Dataset Stages Explained",
      tags$dl(
        tags$dt("Note: Dataset selection is global and applies consistently across all tabs in the app."),
        tags$br(),
        tags$dt("Raw Dataset"),
        tags$dd("Original CSV as loaded. No modifications — includes ID, Date, all Sensor1–30 columns."),
        tags$dt("Enriched Dataset"),
        tags$dd("Raw + derived columns: IdGroup (from ID prefix), Year/Season/Month/Day (from Date),
                SensorGroup1–3 (row means of sensor bands), plus centred, standardised, and normalised variants.
                Sensor columns were renamed with consistent initial capitalisation as well."),
        tags$dt("Model Dataset"),
        tags$dd("Trimmed dataset from enriched one with ID, Date, and Sensor1–30 dropped. Cleaner view for modelling.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  # ── TAB TABLE ──────────────────────────────────────────────────────────────
  # (renderTable cannot handle date format well, so changed to DT::dataTableOutput)
  output$data_table <- DT::renderDataTable({
    # instead of head(selected_data(), 1000), this new code adds col info as well
    df <- head(selected_data(), 1000)
    # can add "switch dataset stage to explore different dimensional views"
    DT::datatable(df, caption = paste0(input$dataset_choice, " Dimensions: ", nrow(df), " × ", ncol(df)))
  })
  
  
  # ── TAB SUMMARY ────────────────────────────────────────────────────────────
  
  output$data_summary <- renderPrint({
    df <- selected_data()
    switch(input$summary_style,
           "base"      = summary(df),
           "glimpse"   = tibble::glimpse(df),
           "dfsummary" = summarytools::dfSummary(df)
    )
  })
  
  
  # ── TAB MISSINGNESS ────────────────────────────────────────────────────────
  
  observe({
    df   <- selected_data()
    req(df)
    all_cols <- names(df)
    updateSelectizeInput(session, "vc_cols", choices = all_cols, selected = all_cols)
  })
  
  output$vc_plot <- renderPlot({
    req(input$vc_value, input$vc_cols)
    df  <- selected_data()
    val <- input$vc_value
    
    # count occurrences of the value in each selected column
    results <- lapply(input$vc_cols, function(col) {
      x     <- df[[col]]
      total <- length(x)
      
      # handle NA as a special keyword
      n_match <- if (toupper(val) == "NA") {
        sum(is.na(x))
      } else if (input$vc_case) {
        sum(as.character(x) == val, na.rm = TRUE)          # case sensitive
      } else {
        sum(tolower(as.character(x)) == tolower(val), na.rm = TRUE)  # case insensitive
      }
      
      data.frame(
        Column  = col,
        Count   = n_match,
        Pct     = round(n_match / total * 100, 1),
        Total   = total
      )
    })
    
    plot_df <- do.call(rbind, results)
    
    # sort if requested
    if (input$vc_sort) {
      sort_col  <- if (input$vc_metric == "count") "Count" else "Pct"
      plot_df   <- plot_df[order(plot_df[[sort_col]], decreasing = FALSE), ]
    }
    
    # keep column order for plot
    plot_df$Column <- factor(plot_df$Column, levels = plot_df$Column)
    
    y_val   <- if (input$vc_metric == "count") plot_df$Count else plot_df$Pct
    y_label <- if (input$vc_metric == "count") "Count" else "Percentage (%)"
    y_max   <- max(y_val, na.rm = TRUE) * 1.15  # headroom for labels
    
    # lollipop plot
    par(mar = c(5, 10, 4, 2))  # wide left margin for column names
    
    plot(NULL,
         xlim = c(0, y_max),
         ylim = c(0.5, nrow(plot_df) + 0.5),
         xlab = y_label,
         ylab = "",
         main = paste0('Occurrences of "', val, '" across Selected Columns'),
         yaxt = "n")
    
    # y axis labels
    axis(2, at = seq_len(nrow(plot_df)),
         labels = levels(plot_df$Column),
         las = 2, cex.axis = 0.9)
    
    # grid lines
    abline(h = seq_len(nrow(plot_df)), col = "grey90", lty = 1)
    
    # stems
    segments(x0 = 0, x1 = y_val,
             y0 = seq_len(nrow(plot_df)),
             y1 = seq_len(nrow(plot_df)),
             col = "steelblue", lwd = 2)
    
    # dots
    points(y_val, seq_len(nrow(plot_df)),
           pch = 16, cex = 2.5, col = "steelblue")
    
    # value labels on dots
    label_text <- if (input$vc_metric == "count") {
      paste0(plot_df$Count, " / ", plot_df$Total)
    } else {
      paste0(plot_df$Pct, "%")
    }
    
    text(y_val, seq_len(nrow(plot_df)),
         labels = label_text,
         pos = 4, cex = 0.85, col = "grey30")
  })
  
  
  # ── TAB RISING ORDER ───────────────────────────────────────────────────────
  # update choices
  observe({
    df <- selected_data()
    req(df)
    
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "rising_var", choices = numeric_vars, selected = numeric_vars[1])
  })
  
  output$rising_plot <- renderPlot({
    req(input$rising_var)
    df <- selected_data()
    
    n <- length(input$rising_var)
    
    # better palettes depending on number of vars
    if (n <= 8) {
      colors <- RColorBrewer::brewer.pal(max(n, 3), "Set1")[1:n]
    } else {
      colors <- rainbow(n)
    }
    
    # vary line types as well for extra differentiation
    ltypes <- rep(1:6, length.out = n)  # cycles through solid, dashed, dotted, etc.
    
    all_vals <- unlist(lapply(input$rising_var, function(v) df[[v]][!is.na(df[[v]])]))
    y_range  <- range(all_vals)
    
    for (i in seq_along(input$rising_var)) {
      v        <- input$rising_var[i]
      y        <- df[[v]]
      y        <- y[!is.na(y)]
      y_sorted <- sort(y)
      pct      <- seq_along(y_sorted) / length(y_sorted) * 100
      
      if (i == 1) {
        plot(pct, y_sorted,
             type = "l", col = colors[i], lwd = 2, lty = ltypes[i],
             xlab = "Percentile", ylab = "Value",
             main = "Rising Value Chart",
             ylim = y_range)
      } else {
        lines(pct, y_sorted, col = colors[i], lwd = 2, lty = ltypes[i])
      }
    }
    
    legend("topleft", legend = input$rising_var,
           col = colors, lwd = 2, lty = ltypes, bty = "n")
  })
  
  
  # ── TAB Q-Q PLOT ───────────────────────────────────────────────────────────
  # update variable choices for QQ Plot
  observe({
    df <- selected_data()
    req(df)
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "qq_var", choices = numeric_vars)
  })
  
  # dynamic UI for distribution parameters
  output$qq_params <- renderUI({
    req(input$qq_dist)
    
    switch(input$qq_dist,
           "norm" = tagList(
             numericInput("qq_mean", "Mean", value = 0),
             numericInput("qq_sd", "SD", value = 1, min = 0.01)
           ),
           "exp" = tagList(
             numericInput("qq_rate", "Rate (λ)", value = 1, min = 0.01)
           ),
           "lnorm" = tagList(
             numericInput("qq_meanlog", "Meanlog", value = 0),
             numericInput("qq_sdlog", "SDlog", value = 1, min = 0.01)
           ),
           "gamma" = tagList(
             numericInput("qq_shape", "Shape (α)", value = 1, min = 0.01),
             numericInput("qq_rate_g", "Rate (β)", value = 1, min = 0.01)
           ),
           "weibull" = tagList(
             numericInput("qq_shape_w", "Shape (k)", value = 1, min = 0.01),
             numericInput("qq_scale_w", "Scale (λ)", value = 1, min = 0.01)
           )
    )
  })
  
  # render the QQ Plot
  output$qq_plot <- renderPlot({
    req(input$qq_var, input$qq_dist)
    df <- selected_data()
    val <- df[[input$qq_var]]
    val <- sort(val[!is.na(val)]) # ascending order/quantile
    
    # define distribution arguments based on user input
    dist_args <- switch(input$qq_dist,
                        "norm"    = list(mean = input$qq_mean, sd = input$qq_sd),
                        "exp"     = list(rate = input$qq_rate),
                        "lnorm"   = list(meanlog = input$qq_meanlog, sdlog = input$qq_sdlog),
                        "gamma"   = list(shape = input$qq_shape, rate = input$qq_rate_g),
                        "weibull" = list(shape = input$qq_shape_w, scale = input$qq_scale_w)
    )
    
    # validate that args are not null
    if (any(sapply(dist_args, is.null))) return(NULL)
    
    # generate theoretical quantiles
    n <- length(val)
    probs <- (1:n - 0.5) / n
    
    # map string distribution names to their quantile functions (qnorm, qexp, etc)
    q_func <- match.fun(paste0("q", input$qq_dist))
    theoretical_quantiles <- do.call(q_func, c(list(p = probs), dist_args))
    
    # plot with updated axis label
    plot(theoretical_quantiles, val,
         xlab = paste("Theoretical Quantiles (", input$qq_dist, ")"),
         ylab = paste("Sample Quantiles:", input$qq_var),
         main = paste("Q-Q Plot:", input$qq_var, "vs", input$qq_dist),
         pch = 20, col = "steelblue")
    
    if (input$qq_line) {
      # Adding a reference line passing through 1st and 3rd quartiles
      # (Similar to qqline logic)
      y_q <- quantile(val, c(0.25, 0.75))
      x_q <- do.call(q_func, c(list(p = c(0.25, 0.75)), dist_args))
      slope <- diff(y_q) / diff(x_q)
      int <- y_q[1] - slope * x_q[1]
      abline(int, slope, col = "red", lwd = 2)
    }
  })
  
  
  # ── TAB MOSAIC ─────────────────────────────────────────────────────────────
  
  # mosaic part1: mosaic plot
  cat_cols <- reactive({
    df <- selected_data()
    names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  })
  
  output$mosaic_x_ui <- renderUI({
    selectInput("mosaic_x", "Variable 1 (columns):", choices = cat_cols())
  })
  
  output$mosaic_y_ui <- renderUI({
    cols <- cat_cols()
    selectInput("mosaic_y", "Variable 2 (rows):", choices = cols, selected = cols[2])
  })
  
  output$mosaic_z_ui <- renderUI({
    cols <- cat_cols()
    selectInput("mosaic_z", "Variable 3 — optional (sub-rows):",
                choices = c("None", cols), selected = "None")
  })
  
  output$mosaic_plot <- renderPlot({
    req(input$mosaic_x, input$mosaic_y, input$mosaic_z)
    df <- selected_data()
    
    # build 2-way or 3-way contingency table depending on Variable 3
    if (input$mosaic_z == "None") {
      tbl <- table(df[[input$mosaic_x]], df[[input$mosaic_y]])
      names(dimnames(tbl)) <- c(input$mosaic_x, input$mosaic_y)
    } else {
      tbl <- table(df[[input$mosaic_x]], df[[input$mosaic_y]], df[[input$mosaic_z]])
      names(dimnames(tbl)) <- c(input$mosaic_x, input$mosaic_y, input$mosaic_z)
    }
    
    title <- paste("Mosaic:", paste(
      c(input$mosaic_x, input$mosaic_y,
        if (input$mosaic_z != "None") input$mosaic_z),
      collapse = " × "))
    
    vcd::mosaic(tbl,
                shade  = input$mosaic_shade,
                legend = input$mosaic_shade,
                main   = title,
                labeling     = labeling_border(
                  rot_labels   = c(90, 0, 0, 0),      # rotate axis labels
                  gp_labels    = gpar(fontsize = 9), # smaller font
                  abbreviate   = TRUE,               # abbreaviated labels or not
                ))
  })
  
  
  # mosaic part2: pair advisor
  # (helper) compute CramerV for a 2-column contingency table
  get_cramer <- function(df, v1, v2) {
    tbl   <- table(df[[v1]], df[[v2]])
    stats <- tryCatch(vcd::assocstats(tbl), error = function(e) NULL)
    if (is.null(stats)) return(NA)
    stats$cramer
  }
  
  pairs_result <- eventReactive(input$pairs_search, {
    df   <- selected_data()
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
      
      # 3-way: score = mean CramerV across all 3 pairwise combos within the trio
      # this gives a single number summarising how interrelated the 3 variables are
      combos  <- combn(cats, 3, simplify = FALSE)
      results <- lapply(combos, function(trio) {
        v12 <- get_cramer(df, trio[1], trio[2])
        v13 <- get_cramer(df, trio[1], trio[3])
        v23 <- get_cramer(df, trio[2], trio[3])
        data.frame(
          Var1    = trio[1],
          Var2    = trio[2],
          Var3    = trio[3],
          CramerV = round(mean(c(v12, v13, v23), na.rm = TRUE), 4),  # avg of 3 pairs
          V_1_2   = round(v12, 4),   # individual pair scores shown for context
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
                    c("#155724",  "#856404",  "#856404", "#6c757d")
                  ),
                  fontWeight = "bold")
  })
  
  # clicking a row loads variables into mosaic dropdowns
  observeEvent(input$pairs_table_rows_selected, {
    res  <- pairs_result()
    sel  <- input$pairs_table_rows_selected
    var1 <- res$Var1[sel]
    var2 <- res$Var2[sel]
    var3 <- res$Var3[sel]
    
    updateSelectInput(session, "mosaic_x", selected = var1)
    updateSelectInput(session, "mosaic_y", selected = var2)
    updateSelectInput(session, "mosaic_z", selected = if (!is.na(var3)) var3 else "None")
  })
  
  
  # ── TAB GGPAIRS ────────────────────────────────────────────────────────────
  # ── TAB CORRELATION ────────────────────────────────────────────────────────
  # ── TAB BOXPLOT ────────────────────────────────────────────────────────────
  # ── TAB OTHERS ─────────────────────────────────────────────────────────────
  
  
} # end server


# =============================================================================
# Run shiny app
# =============================================================================

shinyApp(ui, server)

