library(shiny)

ui <- fluidPage(
  titlePanel("Hello Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Choose a number", 1, 100, 50)
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    paste("You chose:", input$n)
  })
}

shinyApp(ui, server)


# shiny::runApp("D:/DS/VS git code/DATA423_DSI/my_shiny_app/first_app")
# shiny::runApp("D:/DS/VS git code/DATA423_DSI/my_shiny_app")