# =============================================================================
# In R console (NOT inside app.R), I recorded the package version:
# install.packages("renv")
# renv::init()
# if you have different versions of R libraries, potentially you can run
# renv::restore()
# =============================================================================





# =============================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =============================================================================
library(shiny)










# =============================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =============================================================================
ui <- navbarPage(
  title = "DATA423 Assignment 1 — William Hui Chang (69051925)",
  theme = NULL,   # swap in shinythemes::shinytheme("flatly") if you install it
)


# 
# ui <- fluidPage(
#   titlePanel("Hello Shiny"),
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("n", "Choose a number", 1, 100, 50)
#     ),
#     mainPanel(
#       textOutput("result")
#     )
#   )
# )



# =============================================================================
server <- function(input, output) {
  output$result <- renderText({
    paste("You chose:", input$n)
  })
}



# =============================================================================
shinyApp(ui, server)

