# =============================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =============================================================================

# Note: 
#   global file defines features related to dataset
#   UI, server files should be universally applicable

library(shiny)
library(dplyr)

# load the raw dataset
raw_dataset <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)

# create a copy of raw dataset (R use copy-on-modify)
ds_copied <- raw_dataset

# validate dataset with schema
# - predefine a schema
schema <- list(
  numeric_cols = c("Y", grep("^sensor", names(ds_copied), value = TRUE)),
  date_cols    = "Date",
  factor_cols  = c("ID", "Operator", "Priority", "Price",
                   "Speed", "Duration", "Temp", "Location",
                   "Agreed", "State", "Class", "Surface")
)

# - explicitly apply proper schema to our dataset, and solve name issues
ds_typed <- ds_copied %>%
  
  # - assigning schema
  mutate(
    across(all_of(schema$numeric_cols), as.numeric),
    across(all_of(schema$factor_cols),  as.factor),
    across(all_of(schema$date_cols),    as.Date)
  ) %>% 
  
  # - manually defining level order for potential ordinal factors
  # - (has to be strict, since no context provided)
  mutate(
    Priority = factor(Priority, levels = c("Low", "Medium", "High"),      ordered = TRUE),
    # Price    = factor(Price,    levels = c("Expensive", "Fair", "Cheap"), ordered = TRUE),
    # Speed    = factor(Speed,    levels = c("Slow", "Medium", "Fast"),     ordered = TRUE),
    # Duration = factor(Duration, levels = c("Short", "Long", "Very Long"), ordered = TRUE),
    # Temp     = factor(Temp,     levels = c("Cold", "Warm", "Hot"),        ordered = TRUE),
    Agreed   = factor(Agreed,   levels = c("No", "Yes"))
  )

# standardise all the col names with initial capital naming style
ds_renamed <- ds_typed %>% 
  rename_with(~ tools::toTitleCase(.x))  # e.g., sensors have bad names

# enrich the dataset with new cols
ds_col_enriched <- ds_renamed %>%
  
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
enriched_dataset <- ds_col_enriched

# could be subset dataset, since we don't need ID and Date for further modelling (place holder)


# =============================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =============================================================================

# UI
ui <- fluidPage(
  
  # ── GLOBAL DS DROPDOWN ─────────────────────────────────────────────────────
  
  # global dropdown choces for dataset at different stages
  selectInput("dataset_choice", 
              label = "Choose Dataset Stage:", 
              choices = c("Raw Dataset", "Enriched Dataset", "Cleaned Dataset")),
  
  
  # ── TABS ───────────────────────────────────────────────────────────────────
  
  # tabs panels
  tabsetPanel(
    
    # ── TAB TABLE ────────────────────────────────────────────────────────────
    
    tabPanel("Data Table",   DT::dataTableOutput("data_table")),
    
    
    # ── TAB SUMMARY ──────────────────────────────────────────────────────────
    
    tabPanel("Summary",      verbatimTextOutput("data_summary")),
    
    
    # ── TAB MOSAIC ───────────────────────────────────────────────────────────
    
    tabPanel("Mosaic",
             sidebarLayout(
               sidebarPanel(width = 2,   # 2/12 = ~17%, narrow sidebar
                 uiOutput("mosaic_x_ui"),
                 uiOutput("mosaic_y_ui"),
                 uiOutput("mosaic_z_ui"),
                 checkboxInput("mosaic_shade", "Shade (colour by residuals)", value = TRUE)
               ),
               mainPanel(width = 10,     # must add up to 12
                 plotOutput("mosaic_plot", height = "90vh")
               )
             )
    ),
    
    
    # ── TAB GGPAIRS ──────────────────────────────────────────────────────────
    
    tabPanel("GGPairs",      p("Coming soon")),
    
    
    # ── TAB CORRELATION ──────────────────────────────────────────────────────
    
    tabPanel("Correlation",  p("Coming soon")),
    
    
    # ── TAB MISSINGNESS ──────────────────────────────────────────────────────
    
    tabPanel("Missingness",  p("Coming soon")),
    
    
    # ── TAB BOXPLOT ──────────────────────────────────────────────────────────
    
    tabPanel("Boxplot",      p("Coming soon")),
    
    
    # ── TAB RISING ORDER ─────────────────────────────────────────────────────
    
    tabPanel("Rising Order", p("Coming soon"))
    
    
  )
)


# =============================================================================
# server.R
# =============================================================================

# server
server <- function(input, output) {
  
  # ── GLOBAL REACTIVE ────────────────────────────────────────────────────────
  
  # switch dataset based on drop down
  selected_data <- reactive({
    switch(input$dataset_choice,
           "Raw Dataset"      = raw_dataset,
           "Enriched Dataset" = enriched_dataset,
           "Cleaned Dataset"  = ds_renamed,  # care! ds_renamed is an trial, should be cleaned_dataset later
           NULL)
    })
  
  
  # ── TAB TABLE ──────────────────────────────────────────────────────────────
  # (renderTable cannot handle date format well, so changed to DT::dataTableOutput)
  output$data_table <- DT::renderDataTable({
    # instead of head(selected_data(), 1000), this new code adds col info as well
    df <- head(selected_data(), 1000)
    DT::datatable(df, caption = paste0("Dataset Dimensions: ", nrow(df), " × ", ncol(df)))
  })
  
  
  # ── TAB SUMMARY ────────────────────────────────────────────────────────────
  
  output$data_summary <- renderPrint({
    summarytools::dfSummary(selected_data())
  })
  
  
  # ── TAB MOSAIC ─────────────────────────────────────────────────────────────
  
  # only factor/ordered/character cols are valid for mosaic
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
  
  
  # ── TAB GGPAIRS ────────────────────────────────────────────────────────────
  # ── TAB CORRELATION ────────────────────────────────────────────────────────
  # ── TAB MISSINGNESS ────────────────────────────────────────────────────────
  # ── TAB BOXPLOT ────────────────────────────────────────────────────────────
  # ── TAB RISING ORDER ───────────────────────────────────────────────────────
}


# =============================================================================
# Run shiny app
# =============================================================================

shinyApp(ui, server)

