library(shiny)

# --- Load and prepare dataset stages ---
raw_dataset <- mtcars

renamed_dataset <- raw_dataset
colnames(renamed_dataset) <- c("Miles_Per_Gallon", "Cylinders", "Displacement",
                               "Horsepower", "Rear_Axle_Ratio", "Weight",
                               "Quarter_Mile_Time", "VS", "Transmission",
                               "Gears", "Carburetors")

# UI
ui <- fluidPage(
  
  # Global dropdown at the top
  selectInput("dataset_choice", 
              label = "Choose Dataset Stage:", 
              choices = c("Raw Dataset", "Renamed Dataset")),
  
  # Two tabs
  tabsetPanel(
    tabPanel("Table", tableOutput("data_table")),
    tabPanel("Summary", verbatimTextOutput("data_summary"))
  )
)

# Server
server <- function(input, output) {
  
  # Reactive: pick dataset based on dropdown
  selected_data <- reactive({
    if (input$dataset_choice == "Raw Dataset") {
      raw_dataset
    } else {
      renamed_dataset
    }
  })
  
  # Tab 1: show table
  output$data_table <- renderTable({
    head(selected_data(), 10)
  })
  
  # Tab 2: show summary
  output$data_summary <- renderPrint({
    summary(selected_data())
  })
}

shinyApp(ui, server)