# =================================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(shinyAce)  # R console ace editor
library(dplyr)
library(DT)


# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

# ·· SUCURITY LOCK ························································

# What the lock controls (all three require authentication):
#   PRIVATE_COLS  — hidden in all dataset views until unlocked (for stakeholder or authentised users)
#   R Console tab — completely hidden until unlocked (for user or developer me lol)
#   Debug Dataset — only appears in the dataset dropdown when unlocked (for developer me lol)

# # unlock pw
UNLOCK_PASSPHRASE <- "123"

# columns to hide from unauthenticated users
PRIVATE_COLS <- c("ID")


# ·· GLOBAL COL ORDERING ··················································

# ~~ pre-defined global column order ~~

# global master column order, that all datasets could follow 

# Note:
#   every time when I derived a col to enrich, I should add it here in a position
#   it will also skip cols that don't exist in that dataset

MASTER_COL_ORDER <- c(
  # target
  "Y",
  # identifiers and operators
  "IdGroup", "ID", "Operator", 
  # date and derived factors
  "Date", "Year", "Season", "Month", "Day", "WeekNum", "Weekday", 
  # cyclic encodings
  "Season_sin", "Season_cos", "Month_sin", "Month_cos",
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

# ~~ reordering helper function ~~

# reorder a df by global master order
reorder_cols <- function(df) {
  keep <- MASTER_COL_ORDER[MASTER_COL_ORDER %in% names(df)]  # only cols that exist, skipping missing cols
  df[, keep]
}



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


# ── DATA STAGE ───────────────────────────────────────────────────────────────

# ·· RAW DATASET ··························································

# load raw exactly as-is from disk
raw_dataset <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = FALSE)

# create a copied ds for further manipulations (R use copy-on-modify)
ds_copied <- raw_dataset


# ·· PROPER COL NAME DATASET ··············································

# immediately solve col naming issues to prevent further chaos
ds_renamed <- ds_copied %>% 
  rename_with(~ tools::toTitleCase(.x))  # enforce title case


# ·· TYPED DATASET ························································

# ~~ pre-defined schema ~~

schema <- list(
  
  numeric_cols = c("Y", grep("^Sensor", names(ds_copied), value = TRUE)),
  date_cols    = "Date",
  
  factor_schema = list(
    
    # nominal factors
    ID       = list(levels = NULL, ordered = FALSE),
    Operator = list(levels = NULL, ordered = FALSE),
    Location = list(levels = NULL, ordered = FALSE),
    State    = list(levels = NULL, ordered = FALSE),
    Class    = list(levels = NULL, ordered = FALSE),
    Surface  = list(levels = NULL, ordered = FALSE),
    
    # ordinal factors
    Priority = list(
      levels  = c("Low", "Medium", "High"),
      ordered = TRUE
    ),
    
    Price = list(
      levels  = c("Cheap", "Fair", "Expensive"),
      ordered = TRUE
    ),
    
    Speed = list(
      levels  = c("Slow", "Medium", "Fast"),
      ordered = TRUE
    ),
    
    Duration = list(
      levels  = c("Short", "Long", "Very Long"),
      ordered = TRUE
    ),
    
    Temp = list(
      levels  = c("Cold", "Warm", "Hot"),
      ordered = TRUE
    ),
    
    Agreed = list(
      levels  = c("No", "Yes"),
      ordered = FALSE
    )
  )
)


# ~~ assigning type with applied schema ~~

ds_typed <- ds_renamed %>%
  mutate(
    across(all_of(schema$numeric_cols), as.numeric),
    across(all_of(schema$date_cols),    as.Date),
    across(
      all_of(names(schema$factor_schema)),
      ~ {
        col  <- cur_column()
        spec <- schema$factor_schema[[col]]
        if (is.null(spec$levels)) factor(.x)
        else factor(.x, levels = spec$levels, ordered = spec$ordered)
      }
    )
  )


# ·· ENRICHED DATASET ·····················································

enriched_dataset <- ds_typed %>%
  
  # derive IdGroup (G and D)
  mutate(
    
    # derive IdGroup (G and D)
    IdGroup = as.factor(substr(as.character(ID), 1, 1)),
    
    # too few years prefer factor, purely categorical regimes
    Year   = factor(format(Date, "%Y")),
    
    # temporary integer month for deriving season, weekday and cyclic encoding
    Month  = as.integer(format(Date, "%m")),
    
    # month, cyclic encoded
    Month_sin  = sin(2 * pi * Month / 12),
    Month_cos  = cos(2 * pi * Month / 12),
    
    # temporary nominal season before cyclic encoding
    Season = factor(
      case_when(
        Month %in% 1:3   ~ "S1",
        Month %in% 4:6   ~ "S2",
        Month %in% 7:9   ~ "S3",
        Month %in% 10:12 ~ "S4"
      )),
    
    # season, cyclic encoded
    Season_sin = sin(2 * pi * as.integer(Season) / 4),  # S1=1 … S4=4
    Season_cos = cos(2 * pi * as.integer(Season) / 4),
    
    # factorise the integer month
    Month      = factor(Month),
    
    # get weekdays potentially from Monday to Sunday
    Weekday = factor(format(Date, "%A")),
    
    # ISO week number 1-53
    WeekNum = as.integer(format(Date, "%V")),
    
    # # day-of-month is likely just noise (data weekly collected on Thursdays and Fridays)
    # Day    = as.integer(format(Date, "%d")),
    
    # sensor patterns, including grouping and feature scaling
    #   potential dimensionality reduction with sensor groups
    #   based on value pattern similarities across sensors
    #   making it a perfect choice and can handle NAs
    
    # # potential sensor groups based on boxplot patterns
    # SensorGroup1 = as.numeric(rowMeans(across(Sensor1:Sensor10), na.rm = TRUE)),
    # SensorGroup2 = as.numeric(rowMeans(across(Sensor11:Sensor20), na.rm = TRUE)),
    # SensorGroup3 = as.numeric(rowMeans(across(Sensor21:Sensor30), na.rm = TRUE)),
    # 
    # # centering (subtract mean, mean becomes 0, spread preserved)
    # CenterSG1 = SensorGroup1 - mean(SensorGroup1, na.rm = TRUE),
    # CenterSG2 = SensorGroup2 - mean(SensorGroup2, na.rm = TRUE),
    # CenterSG3 = SensorGroup3 - mean(SensorGroup3, na.rm = TRUE),
    # 
    # # Z-score standardisation (mean=0, SD=1)
    # StandardisedSG1 = as.numeric(scale(SensorGroup1, center = TRUE, scale = TRUE)),
    # StandardisedSG2 = as.numeric(scale(SensorGroup2, center = TRUE, scale = TRUE)),
    # StandardisedSG3 = as.numeric(scale(SensorGroup3, center = TRUE, scale = TRUE)),
    # 
    # # min-max normalisation (rescales to [0, 1])
    # NormalisedSG1 = (SensorGroup1 - min(SensorGroup1, na.rm = TRUE)) /
    #   (max(SensorGroup1, na.rm = TRUE) - min(SensorGroup1, na.rm = TRUE)),
    # NormalisedSG2 = (SensorGroup2 - min(SensorGroup2, na.rm = TRUE)) /
    #   (max(SensorGroup2, na.rm = TRUE) - min(SensorGroup2, na.rm = TRUE)),
    # NormalisedSG3 = (SensorGroup3 - min(SensorGroup3, na.rm = TRUE)) /
    #   (max(SensorGroup3, na.rm = TRUE) - min(SensorGroup3, na.rm = TRUE))
  ) %>% # end of enriching mutation
  
  # reordering dataset after enriching
  reorder_cols()


# ·· MODEL DATASET ························································

# trim down to only the columns a model needs
model_dataset <- enriched_dataset %>%
  
  select(
    -any_of(c("ID", "Date")),       # example drops
    # -matches("^RawSensor\\d+$"),  # drop Sensor1, Sensor2, ... Sensor30
    # -matches("^Normalised"),      # drop NormalisedSG1, NormalisedSG2, NormalisedSG3
  )


# ·· DEBUGGING DATASET ····················································

# add a flag column, for exploring
debug_dataset <- enriched_dataset %>%
  mutate(
    # example: flag
    Flag = NA_character_   # empty string column — fill in manually later
  )





# =================================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =================================================================================

ui <- fluidPage(
  
  # ── GLOBAL UI CONTROLS ─────────────────────────────────────────────────────
  
  # dataset stage selector + privacy lock always visible at the top
  fluidRow(
    
    # dataset stage selector (debug stage only injected when unlocked — see server)
    column(2,
           uiOutput("dataset_selector_ui")   # rendered server-side so debug is gated
    ),
    
    # info button
    column(1,
           actionButton("dataset_info", label = NULL,
                        icon  = icon("circle-info"),
                        style = "margin-top:25px; font-size:20px;
                               color:#0d6efd; background:none;
                               border:none; padding:0;")
    ),
    
    # spacer
    column(6),
    
    # passphrase + lock toggle
    column(3,
           div(style = "margin-top:24px; display:flex; align-items:center; gap:8px;",
               passwordInput("privacy_pass", label = NULL,
                             placeholder = "Passphrase 123 to unlock",
                             width = "200px"),
               actionButton("privacy_unlock", label = NULL, icon = icon("lock"),
                            style = "padding:6px 10px;"),
               uiOutput("privacy_status_ui")
           ))
  ),
  
  hr(),
  
  
  # ── TABS ────────────────────────────────────────────────────────────────────
  
  tabsetPanel(
    
    # ── UI Data Table ─────────────────────────────────────────────────────
    
    tabPanel("Data Table",   DT::dataTableOutput("data_table")),  # end of tab panel
    
    # ── UI SUMMARY ────────────────────────────────────────────────────────
    
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(width = 2,
                            sidebar_note("Note: <br><br>Can select a style to inspect the dataset structure."),
                            hr(),
                            radioButtons("summary_style", "Style:",
                                         choices = c("base R"  = "base",
                                                     "glimpse" = "glimpse",
                                                     "dfSummary"   = "dfsummary"),
                                         selected = "glimpse")
               ),
               mainPanel(width = 10,
                         verbatimTextOutput("summary_output")
               )
             )
    ), # end of tab panel
    
    
    # ── UI PARALLEL COORDINATES ───────────────────────────────────────────
    
    tabPanel("Parallel",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Duplicate Detection: <br><br>Lines that travel the same path across 
                                     all axes are systematic duplicates. Dense overlapping strands 
                                     indicate repeated value patterns across columns."),
                            hr(),
                            
                            # column selector (numeric only — populated server-side)
                            selectizeInput("pc_cols", "Columns to include:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            
                            # colour group
                            selectInput("pc_group", "Colour lines by:",
                                        choices = NULL),
                            hr(),
                            
                            # alpha slider
                            sliderInput("pc_alpha", "Line transparency:",
                                        min = 0.01, max = 1.0, value = 0.3, step = 0.01),
                            
                            # normalise toggle
                            checkboxInput("pc_normalise", "Normalise axes to 0–1", value = TRUE),
                            hr(),
                            
                            # row sample limit (safety for performance)
                            numericInput("pc_max_rows", "Max rows to plot:",
                                         value = 360, min = 10, max = 5000)
               ),
               mainPanel(width = 9,
                         plotlyOutput("pc_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI TABLEPLOT ──────────────────────────────────────────────────────
    
    tabPanel("Tableplot",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Sequence Pattern Detection: <br><br>Rows are sorted by the chosen 
                                     variable, binned, and each column's mean is shown as a heatmap tile. 
                                     Horizontal bands of consistent colour indicate systematic patterns 
                                     or duplicates across row sequences."),
                            hr(),
                            
                            # sort by variable (all columns, default Date)
                            selectInput("tp_sort_var", "Sort rows by:",
                                        choices = NULL),
                            
                            radioButtons("tp_sort_dir", "Sort direction:",
                                         choices  = c("Ascending" = "asc", "Descending" = "desc"),
                                         selected = "asc"),
                            hr(),
                            
                            # column selector (numeric only)
                            selectizeInput("tp_cols", "Columns to include:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            
                            # bin slider
                            sliderInput("tp_bins", "Number of row bins:",
                                        min = 10, max = 200, value = 100, step = 10),
                            hr(),
                            
                            # scaling method
                            radioButtons("tp_scale", "Value scaling:",
                                         choices  = c("Raw"          = "raw",
                                                      "Normalise"    = "normalise",
                                                      "Centre"       = "centre",
                                                      "Standardise"  = "standardise"),
                                         selected = "normalise")
               ),
               mainPanel(width = 9,
                         plotlyOutput("tp_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI PLOT A EXAMPLE ─────────────────────────────────────────────────
    
    tabPanel("Plot A",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Note: <br><br>add my own controls here."),
                            hr(),
                            selectInput("plotA_var", "Variable:", choices = NULL)
               ),
               mainPanel(width = 9,
                         plotOutput("plotA_output", height = "85vh")
               )
             )
    ), # end of tab panel
    
    
    # ── COMING SOON ───────────────────────────────────────────────────────
    
    tabPanel("Coming Soon",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Note: <br><br>This feature is under development."),
                            hr(),
                            selectInput("x_var", "Variable:", choices = NULL)
               ),
               mainPanel(width = 9,
                         plotOutput("x_output", height = "85vh")
               )
             )
    ), # end of tab panel
    
    
    # ── UI R CONSOLE ───────────────────────────────────────────────────────
    
    # the entire tab body is rendered server-side to stay hidden when locked
    #   for myself use only (debugging / testing / developing)
    #   reason is I cant access R terminal when using shiny app lol
    #   so, kinda cool to make this anyway :D
    
    tabPanel("R Console", uiOutput("rconsole_body_ui")) # end of tab panel
    
    
  ) # end tabsetPanel
)   # end fluidPage





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
  
  # status badge next to the lock button
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
    choices <- c("Raw Dataset", "Enriched Dataset", "Model Dataset")
    
    # (injects "Debug Dataset" when unlocked)
    if (is_unlocked()) choices <- c(choices, "Debug Dataset")
    
    # preserve current selection
    current <- isolate(input$dataset_choice)
    if (is.null(current) || !current %in% choices) current <- "Enriched Dataset"
    
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
        tags$dt("Enriched Dataset"),
        tags$dd("Raw + derived columns (type fixes, date parts, computed features, etc.)."),
        tags$dt("Model Dataset"),
        tags$dd("Enriched with identifiers and leaky columns removed. Ready for modelling."),
        tags$dt("Debug Dataset", style = "color:#856404;"),
        tags$dd("Enriched + diagnostic flags and intermediate values. Only visible when unlocked.")
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
  
  output$data_table <- DT::renderDataTable({
    df <- head(display_data(), 1000)
    DT::datatable(df,
                  caption = paste0(input$dataset_choice,
                                   " — ", nrow(df), " rows × ", ncol(df), " cols"))
  })
  
  
  # ── SERVER SUMMARY ─────────────────────────────────────────────────────
  
  output$summary_output <- renderPrint({
    df <- display_data()
    switch(input$summary_style,
           "base"    = summary(df),
           "glimpse" = tibble::glimpse(df),
           "dfsummary" = summarytools::dfSummary(df))
  })
  
  
  # ── SERVER PARALLEL COORDINATES ────────────────────────────────────────
  
  observe({
    df       <- display_data()
    req(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    updateSelectizeInput(session, "pc_cols",  choices = num_vars, selected = num_vars)
    updateSelectInput(   session, "pc_group", choices = cat_vars, selected = cat_vars[1])
  })
  
  output$pc_plot <- renderPlotly({
    req(input$pc_cols, input$pc_group)
    df <- display_data()
    
    validate(need(length(input$pc_cols) >= 2, "Please select at least 2 numeric columns."))
    
    # ·· subset + optional row cap ··
    df_sub <- df[, c(input$pc_cols, input$pc_group), drop = FALSE]
    if (nrow(df_sub) > input$pc_max_rows) {
      df_sub <- df_sub[sample(nrow(df_sub), input$pc_max_rows), ]
    }
    
    # ·· normalise each numeric column to 0-1 ··
    df_num <- df_sub[, input$pc_cols, drop = FALSE]
    if (isTRUE(input$pc_normalise)) {
      df_num <- as.data.frame(lapply(df_num, function(x) {
        rng <- range(x, na.rm = TRUE)
        if (diff(rng) == 0) return(rep(0, length(x)))  # constant column — avoid div/0
        (x - rng[1]) / diff(rng)
      }))
    }
    
    # ·· attach group + row id for line grouping ··
    df_num$group  <- as.character(df_sub[[input$pc_group]])
    df_num$row_id <- seq_len(nrow(df_num))
    
    # ·· pivot to long format ··
    plot_df <- tidyr::pivot_longer(
      df_num,
      cols      = all_of(input$pc_cols),
      names_to  = "Column",
      values_to = "Value"
    )
    
    # preserve column order on x axis
    plot_df$Column <- factor(plot_df$Column, levels = input$pc_cols)
    
    # ·· plot ··
    p <- ggplot(plot_df, aes(x      = Column,
                             y      = Value,
                             group  = row_id,
                             colour = group,
                             text   = paste0("Row: ", row_id, "<br>Group: ", group,
                                             "<br>Column: ", Column,
                                             "<br>Value: ", round(Value, 4)))) +
      geom_line(alpha = input$pc_alpha, linewidth = 0.5) +
      labs(title  = "Parallel Coordinates — Normalised",
           x      = NULL,
           y      = if (isTRUE(input$pc_normalise)) "Normalised Value (0–1)" else "Value",
           colour = input$pc_group) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right")
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(title = list(text = input$pc_group)))
  })
  
  
  # ── SERVER TABLEPLOT ───────────────────────────────────────────────────
  
  observe({
    df       <- display_data()
    req(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    all_vars <- names(df)
    
    # default sort to Date if it exists, otherwise first column
    default_sort    <- if ("Date" %in% all_vars) "Date" else all_vars[1]
    default_sensors <- grep("^Sensor", num_vars, value = TRUE)  # only Sensor cols by default
    
    updateSelectInput(   session, "tp_sort_var", choices = all_vars,  selected = default_sort)
    updateSelectizeInput(session, "tp_cols",     choices = num_vars,  selected = default_sensors)
  })
  
  output$tp_plot <- renderPlotly({
    req(input$tp_sort_var, input$tp_cols)
    df <- display_data()
    
    validate(need(length(input$tp_cols) >= 1, "Please select at least 1 numeric column."))
    
    # ·· sort rows ··
    sort_vec <- df[[input$tp_sort_var]]
    row_order <- if (input$tp_sort_dir == "asc") {
      order(sort_vec, na.last = TRUE)
    } else {
      order(sort_vec, decreasing = TRUE, na.last = TRUE)
    }
    df_sorted <- df[row_order, , drop = FALSE]
    
    # ·· assign row bins ··
    n        <- nrow(df_sorted)
    n_bins   <- input$tp_bins
    bin_size <- ceiling(n / n_bins)
    df_sorted$bin <- ceiling(seq_len(n) / bin_size)
    
    # ·· compute mean per bin per column ··
    bin_means <- df_sorted %>%
      group_by(bin) %>%
      summarise(across(all_of(input$tp_cols), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop")
    
    # ·· scale each column according to selected method ··
    bin_means_scaled <- bin_means
    for (col in input$tp_cols) {
      x <- bin_means[[col]]
      bin_means_scaled[[col]] <- switch(input$tp_scale,
                                        "raw"         = x,
                                        "normalise"   = {
                                          rng <- range(x, na.rm = TRUE)
                                          if (diff(rng) == 0) rep(0, length(x)) else (x - rng[1]) / diff(rng)
                                        },
                                        "centre"      = x - mean(x, na.rm = TRUE),
                                        "standardise" = as.numeric(scale(x, center = TRUE, scale = TRUE))
      )
    }
    
    # ·· pivot to long ··
    plot_df <- tidyr::pivot_longer(
      bin_means_scaled,
      cols      = all_of(input$tp_cols),
      names_to  = "Column",
      values_to = "Value"
    )
    
    # preserve column order in facets
    plot_df$Column <- factor(plot_df$Column, levels = input$tp_cols)
    
    
    clim <- if (input$tp_scale == "normalise") {
      c(0, 1)
    } else {
      rng <- range(plot_df$Value, na.rm = TRUE)
      c(rng[1], rng[2])
    }
    
    # ·· plot — horizontal bars, faceted by column ··
    p <- ggplot(plot_df, aes(x    = Value,
                             y    = bin,
                             text = paste0("Bin: ",       bin,
                                           "<br>Col: ",   Column,
                                           "<br>Norm mean: ", round(Value, 3)))) +
      geom_col(aes(fill = Value), orientation = "y", width = 0.9) +
      scale_fill_viridis_c(option = "magma", limits = clim, name = "Value") +
      facet_wrap(~ Column, nrow = 1, strip.position = "bottom") +
      scale_y_reverse(expand = c(0, 0)) +
      scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      labs(title = paste0("Tableplot — sorted by ", input$tp_sort_var),
           x     = NULL,
           y     = paste0("Row bin (", bin_size, " rows each)")) +
      theme_minimal(base_size = 9) +
      theme(
        panel.spacing    = unit(0.5, "mm"),      # minimal gap between panels
        strip.text       = element_text(angle = 90, hjust = 1, size = 7),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(),
        panel.grid       = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = NA)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
  
  # ── SERVER PLOT A EXAMPLE ──────────────────────────────────────────────
  
  # user sidebar action collection layer (reactive auto-detect)
  observe({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "plotA_var", choices = num_vars, selected = num_vars[1])
  })
  
  # render layer (computation / visualization)
  output$plotA_output <- renderPlot({
    req(input$plotA_var)
    df <- display_data()
    # random plot example
    hist(df[[input$plotA_var]],
         main = paste("Histogram —", input$plotA_var),
         xlab = input$plotA_var, col = "steelblue", border = "white")
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
                     sidebar_note("Note: <br><br>Run R expressions against the current dataset. <br><br>The dataset is available as df."),
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





# =============================================================================
# Run shiny app
# =============================================================================

shinyApp(ui, server)