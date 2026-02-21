# =============================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =============================================================================

library(shiny)
library(dplyr)

# load the raw dataset
raw_dataset <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)

renamed_dataset <- raw_dataset %>% 
  rename_with(~ tools::toTitleCase(.x))  # e.g., sensors have bad names


# =============================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =============================================================================

# UI
ui <- fluidPage(
  
  # global dropdown at the top
  selectInput("dataset_choice", 
              label = "Choose Dataset Stage:", 
              choices = c("Raw Dataset", "Renamed Dataset")),
  
  # two tabs
  tabsetPanel(
    tabPanel("Table", tableOutput("data_table")),
    tabPanel("Summary", verbatimTextOutput("data_summary"))
  )
)


# =============================================================================
# server.R
# =============================================================================

# server
server <- function(input, output) {
  
  # reactive: pick dataset based on dropdown
  selected_data <- reactive({
    if (input$dataset_choice == "Raw Dataset") {
      raw_dataset
    } else {
      renamed_dataset
    }
  })
  
  # tab 0: show table
  output$data_table <- renderTable({
    head(selected_data(), 1000)
  })
  
  # tab 1: show summary
  output$data_summary <- renderPrint({
    summarytools::dfSummary(selected_data())
  })
}


# =============================================================================
# Run shiny app
# =============================================================================

shinyApp(ui, server)

