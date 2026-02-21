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
    Year   = as.integer(format(Date, "%Y")),      # get year value from date
    Month  = as.integer(format(Date, "%m")),      # get month value from date
    Day    = as.integer(format(Date, "%d")),      # get day value from date
    Season = factor(
      case_when(
        Month %in% 1:3   ~ "S1",
        Month %in% 4:6   ~ "S2",
        Month %in% 7:9   ~ "S3",
        Month %in% 10:12 ~ "S4"
      ))                            # get season value from month
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
  
  # global dropdown choces for dataset at different stages
  selectInput("dataset_choice", 
              label = "Choose Dataset Stage:", 
              choices = c("Raw Dataset", "Enriched Dataset", "Cleaned Dataset")),
  
  # tabs
  tabsetPanel(
    tabPanel("Data Table",   DT::dataTableOutput("data_table")),
    tabPanel("Summary",      verbatimTextOutput("data_summary")),
    tabPanel("Mosaic",       p("Coming soon")),
    tabPanel("GGPairs",      p("Coming soon")),
    tabPanel("Correlation",  p("Coming soon")),
    tabPanel("Missingness",  p("Coming soon")),
    tabPanel("Boxplot",      p("Coming soon")),
    tabPanel("Rising Order", p("Coming soon"))
  )
)


# =============================================================================
# server.R
# =============================================================================

# server
server <- function(input, output) {
  
  # reactive: pick dataset based on dropdown
  selected_data <- reactive({
    switch(input$dataset_choice,
           "Raw Dataset"      = raw_dataset,
           "Enriched Dataset" = enriched_dataset,
           "Cleaned Dataset"  = ds_renamed,  # care, ds_renamed is an trial, should be cleaned_dataset later
           NULL)
    })
  
  # tab: show table
  # (renderTable cannot handle date format well, so switched to DT::dataTableOutput)
  output$data_table <- DT::renderDataTable({
    # instead of head(selected_data(), 1000), new code adds col info as well
    df <- head(selected_data(), 1000)
    DT::datatable(df, caption = paste0("Dataset Dimensions: ", nrow(df), " × ", ncol(df)))
  })
  
  # tab: show summary
  output$data_summary <- renderPrint({
    summarytools::dfSummary(selected_data())
  })
  
  # other tabs

}


# =============================================================================
# Run shiny app
# =============================================================================

shinyApp(ui, server)

