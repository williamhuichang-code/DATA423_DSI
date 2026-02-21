# =============================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =============================================================================

library(shiny)
library(dplyr)

# load the raw dataset
raw_dataset <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)

# should be other steps such as schema

renamed_dataset <- raw_dataset %>% 
  rename_with(~ tools::toTitleCase(.x))  # e.g., sensors have bad names

# should be other steps such as enriching


# =============================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =============================================================================

# UI
ui <- fluidPage(
  
  # global dataset dropdown at the top
  selectInput("dataset_choice", 
              label = "Choose Dataset Stage:", 
              choices = c("Raw Dataset", "Renamed Dataset")),
  
  # tabs
  tabsetPanel(
    tabPanel("Data Table",   tableOutput("data_table")),
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
    if (input$dataset_choice == "Raw Dataset") {
      raw_dataset
    } else {
      renamed_dataset
    }
  })
  
  # tab: show table
  output$data_table <- renderTable({
    head(selected_data(), 1000)
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

