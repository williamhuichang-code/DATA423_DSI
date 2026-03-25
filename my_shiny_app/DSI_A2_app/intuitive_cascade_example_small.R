library(shiny)

ui <- fluidPage(
  titlePanel("Simple Cascade Pipeline"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Stage 1: Filter"),
      sliderInput("max_val", "Keep numbers below:", min = 1, max = 100, value = 80),
      
      hr(),
      h4("Stage 2: Transform"),
      radioButtons("transform", "Apply to filtered numbers:",
                   choices = c("None" = "none", "Double them" = "double", "Square root" = "sqrt"),
                   selected = "none"),
      
      hr(),
      h4("Stage 3: Round"),
      checkboxInput("do_round", "Round to whole numbers", value = FALSE)
    ),
    
    mainPanel(
      h4("Raw numbers (Stage 0):"),
      verbatimTextOutput("raw"),
      
      h4("After Stage 1 — Filter:"),
      verbatimTextOutput("after_filter"),
      
      h4("After Stage 2 — Transform:"),
      verbatimTextOutput("after_transform"),
      
      h4("After Stage 3 — Round:"),
      verbatimTextOutput("after_round")
    )
  )
)

server <- function(input, output, session) {
  
  # Stage 0: raw data (never changes)
  raw <- reactive({
    c(5, 23, 67, 81, 44, 99, 12, 55, 73, 90)
  })
  
  # Stage 1: depends only on raw + input$max_val
  filtered <- reactive({
    raw()[raw() < input$max_val]
  })
  
  # Stage 2: depends only on filtered() + input$transform
  # Stage 1 does NOT re-run when transform changes
  transformed <- reactive({
    x <- filtered()
    switch(input$transform,
           "none"   = x,
           "double" = x * 2,
           "sqrt"   = sqrt(x))
  })
  
  # Stage 3: depends only on transformed() + input$do_round
  # Stages 1 & 2 do NOT re-run when do_round changes
  rounded <- reactive({
    if (input$do_round) round(transformed()) else transformed()
  })
  
  output$raw           <- renderPrint({ raw() })
  output$after_filter  <- renderPrint({ filtered() })
  output$after_transform <- renderPrint({ transformed() })
  output$after_round   <- renderPrint({ rounded() })
}

shinyApp(ui, server)