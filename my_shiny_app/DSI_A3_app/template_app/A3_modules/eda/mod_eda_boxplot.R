# =================================================================================
# mod_eda_boxplot.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

eda_boxplot_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd; min-height: 100vh; padding-left: 20px;",
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
                 padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
                 margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br> Boxplots of all numeric variables. Adjust the IQR multiplier to control outlier visibility.")
      ),
      hr(),
      
      sliderInput(
        inputId  = ns("multiplier"),
        label    = "IQR Multiplier",
        min = 0, max = 10, step = 0.1, value = 1.5
      ),
      checkboxInput(
        inputId = ns("normalise"),
        label   = "Standardise chart",
        value   = TRUE
      )
    ),
    mainPanel(
      width = 9,
      plotOutput(ns("boxplot"), height = "80vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

eda_boxplot_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    output$boxplot <- renderPlot({
      d <- get_data()
      numeric <- sapply(d, FUN = is.numeric)
      req(d, input$multiplier, length(numeric) > 0)
      d <- scale(d[, numeric], center = input$normalise, scale = input$normalise)
      boxplot(d, outline = TRUE,
              main = paste("Boxplot using IQR multiplier of", input$multiplier),
              range = input$multiplier, las = 2)
    })
    
  })
}