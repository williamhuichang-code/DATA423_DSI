# =============================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# Data loading
# Data cleaning
# Schema definitions
# Feature engineering
# Helper functions
# Library loading
# =============================================================================

library(shiny)
library(shinythemes)
library(dplyr)

# ── 0. LOAD RAW DATASET ──────────────────────────────────────────────────────

# load the raw dataset
raw_dataset <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)


# ── 1. CREATE A COPY ─────────────────────────────────────────────────────────

# create a copy of raw dataset (R use copy-on-modify)
ds_copied <- raw_dataset


# ── 2. DECLARE ITS SCHEMA ────────────────────────────────────────────────────

# predefine a schema
schema <- list(
  numeric_cols = c("Y", grep("^sensor", names(ds_copied), value = TRUE)),
  date_cols    = "Date",
  factor_cols  = c("ID", "Operator", "Priority", "Price",
                   "Speed", "Duration", "Temp", "Location",
                   "Agreed", "State", "Class", "Surface")
)

# explicitly apply proper schema to our dataset, and solve name issues
ds_typed <- ds_copied %>%
  
  # assigning schemas
  mutate(
    across(all_of(schema$numeric_cols), as.numeric),
    across(all_of(schema$factor_cols),  as.factor),
    across(all_of(schema$date_cols),    ~ as.Date(.x, format = "%d/%m/%Y"))
    ) %>% 
  
  # manually defining level order for potential ordinal factors (has to be strict, no context provided)
  mutate(
    Priority = factor(Priority, levels = c("Low", "Medium", "High"),      ordered = TRUE),
    # Price    = factor(Price,    levels = c("Expensive", "Fair", "Cheap"), ordered = TRUE),
    # Speed    = factor(Speed,    levels = c("Slow", "Medium", "Fast"),     ordered = TRUE),
    # Duration = factor(Duration, levels = c("Short", "Long", "Very Long"), ordered = TRUE),
    # Temp     = factor(Temp,     levels = c("Cold", "Warm", "Hot"),        ordered = TRUE),
    Agreed   = factor(Agreed,   levels = c("No", "Yes"))
    )


# ── 3. DECENT VARIABLE NAMES ─────────────────────────────────────────────────

ds_renamed <- ds_typed %>% 
  # standardise all the col names with initial capital naming style
  rename_with(~ tools::toTitleCase(.x))  # e.g., sensors have bad names


# ── 4. COL ENRICHMENTS ───────────────────────────────────────────────────────

# enrich the dataset with new cols
ds_col_enriched <- ds_renamed %>%
  
  # derive IdGroup, ID values start with significant pattern of G type and D type
  mutate(
    IdGroup = as.factor(substr(as.character(ID), 1, 1)),  # ID group info subtracted from the 1st letter in ID
    ) %>% 
  
  # derive meaningful cyclic info from Date, including Year, Season, Month, Day
  mutate(
    Date   = as.Date(Date, format = "%d/%m/%Y"),   # explicitly format date in case it wasn't
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
  
  # sensor patterns, including grouping and feature scaling
  mutate(
    # sensor groups based on boxplot patterns
    SensorGroup1 = as.numeric(rowMeans(across(Sensor1:Sensor10), na.rm = TRUE)),   # dimensionality reduction with sensor groups
    SensorGroup2 = as.numeric(rowMeans(across(Sensor11:Sensor20), na.rm = TRUE)),  # based on value pattern similarities across sensors
    SensorGroup3 = as.numeric(rowMeans(across(Sensor21:Sensor30), na.rm = TRUE)),  # making it a perfect choice and can handle NAs
    # centering — subtract mean, mean becomes 0, spread preserved
    CenterSG1 = SensorGroup1 - mean(SensorGroup1, na.rm = TRUE),
    CenterSG2 = SensorGroup2 - mean(SensorGroup2, na.rm = TRUE),
    CenterSG3 = SensorGroup3 - mean(SensorGroup3, na.rm = TRUE),
    # Z-score standardisation — mean=0, SD=1
    StandardisedSG1 = as.numeric(scale(SensorGroup1, center = TRUE, scale = TRUE)),
    StandardisedSG2 = as.numeric(scale(SensorGroup2, center = TRUE, scale = TRUE)),
    StandardisedSG3 = as.numeric(scale(SensorGroup3, center = TRUE, scale = TRUE)),
    # min-max normalisation — rescales to [0, 1]
    NormalisedSG1 = (SensorGroup1 - min(SensorGroup1, na.rm = TRUE)) /
      (max(SensorGroup1, na.rm = TRUE) - min(SensorGroup1, na.rm = TRUE)),
    NormalisedSG2 = (SensorGroup2 - min(SensorGroup2, na.rm = TRUE)) /
      (max(SensorGroup2, na.rm = TRUE) - min(SensorGroup2, na.rm = TRUE)),
    NormalisedSG3 = (SensorGroup3 - min(SensorGroup3, na.rm = TRUE)) /
      (max(SensorGroup3, na.rm = TRUE) - min(SensorGroup3, na.rm = TRUE))
  )
  

# ── 5. DECENT ROWS ───────────────────────────────────────────────────────────

# enrich the dataset with proper rows
enriched_dataset <- ds_col_enriched





# colnames(enriched_dataset) # for debugging
# summarytools::dfSummary(enriched_dataset)





all_cols    <- names(raw_dataset)

# type conversion is a data integrity decision
# it should not change per user session
# so I intend it to be performed once in the global scope to ensure
# rather than through user-controlled checkboxes in the UI
 

# =============================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =============================================================================

ui <- navbarPage(
  
  # bar title
  title = "DATA423 Assignment 1 — William Hui Chang (69051925)",
  
  # shiny theme
  #   options: NULL, shinytheme("flatly"), "cosmo","lumen", "readable"
  theme = shinytheme("flatly"),
  
  # ── GLOBAL DATASET SELECTOR ─────────────────────────────────────────────────
  # placed in header so it persists across ALL tabs
  header = div(
    style = "padding: 8px 15px; background-color: #f5f5f5; border-bottom: 1px solid #ddd;",
    selectInput("selected_ds", "Active dataset:",
                choices  = c(
                  "Raw dataset" = "raw",
                  "Typed"       = "typed",
                  "Renamed"     = "renamed",
                  "Enriched"    = "enriched"
                ),
                selected = "raw",
                width    = "250px")
  ),
  
  # ── 0. DATA OVERVIEW ────────────────────────────────────────────────────────
  tabPanel("Overview",
    # note displaying option 1
    div(
      class = "alert alert-info",
      strong("Note: "),
      "This section summarises the dataset. ",
      "Use the navigation tabs above to explore visualisations."
      ),
      
    # # note displaying option 2
    # wellPanel(
    #   strong("Note:"),
    #   p("e.g., This section provides a summary of the dataset."),
    #   p("e.g., Scroll horizontally to view all variables.")
    #   ),
    #   
    # # note displaying option 3
    # tags$details(
    #   tags$summary(strong("Notes (click to expand)")),
    #   p("This section summarises the dataset."),
    #   p("Scroll horizontally if needed."),
    #   p("Use other tabs for deeper analysis.")
    #   ),
      
    # a clean divider
    tags$hr(),
    
    # fluid page is a layout that can stretch to fill the screen
    fluidPage(
      # h1(), Main title
      # h2(), Section
      # h3(), Subsection
      # h4(), Smaller subsection
      # h5(), Minor heading
      # h6(), Smallest
      
      # medium h3 format for section title
      h3("Dataset Overview"),
      
      fluidRow(
        column(12,  # use 12/12 page width
          # smaller h4 format for content title
          h4("Summary Statistics"), 
          # verbatim displays text exactly as it is, without formatting
          verbatimTextOutput("overview_summary")
          )
        )
      ),
    
    # ── DUPLICATE CHECKER ─────────────────────────────────────────────────
  #   selectInput("dup_col", "Check duplicates in column:", choices = NULL, multiple = TRUE),
  #   verbatimTextOutput("dup_result")
  #   
  #   )
  # )
    h4("Uniqueness Checker"),
    fluidRow(
      column(4,
             # multiple = TRUE allows single or multi-col combination check
             selectInput("dup_col", "Check column(s):",
                         choices  = NULL,
                         multiple = TRUE)
      ),
      column(8,
             verbatimTextOutput("dup_result")
      )
    )
  )  # end tabPanel
  )  # end navbarPage



# =============================================================================
# server.R
# =============================================================================

server <- function(input, output, session) {
  
  # ── GLOBAL REACTIVE — drives every tab ──────────────────────────────────────
  
  # all outputs should call selected_data() instead of a hardcoded dataset name
  selected_data <- reactive({
    switch(input$selected_ds,
           raw      = raw_dataset,
           typed    = ds_typed,
           renamed  = ds_renamed,
           enriched = enriched_dataset
    )
  })
  
  # when dataset changes, refresh all column-based dropdowns to match new cols
  observe({
    ds   <- selected_data()
    cols <- names(ds)
    updateSelectInput(session, "dup_col", choices = cols, selected = cols[1])
  })
  
  
  # ── 0. DATA OVERVIEW ────────────────────────────────────────────────────────

  # output summaries
  output$overview_summary <- renderPrint({
    # option 1: base R summary
    # summary(selected_data())
    # option 2: tibble::glimpse
    # tibble::glimpse(selected_data())
    # option 3: summarytools::dfSummary
    summarytools::dfSummary(selected_data())
  })
  
  
  # duplicate / uniqueness checker
  output$dup_result <- renderPrint({
    req(input$dup_col)
    ds      <- selected_data()         # uses whichever stage is active
    cols    <- input$dup_col
    subset  <- ds[, cols, drop = FALSE]
    n_total  <- nrow(subset)
    n_unique <- nrow(unique(subset))
    n_dupes  <- sum(duplicated(subset))
    
    cat("Dataset       :", input$selected_ds, "\n")
    cat("Column(s)     :", paste(cols, collapse = " + "), "\n")
    cat("Total rows    :", n_total,  "\n")
    cat("Unique combos :", n_unique, "\n")
    cat("Duplicate rows:", n_dupes,  "\n")
    
    if (n_dupes > 0) {
      cat("\nDuplicated rows:\n")
      print(subset[duplicated(subset), , drop = FALSE])
    } else {
      cat("\nAll values/combinations are unique.\n")
    }
  })
  
}



# =============================================================================
# Run shiny app
# =============================================================================

shinyApp(ui, server)

