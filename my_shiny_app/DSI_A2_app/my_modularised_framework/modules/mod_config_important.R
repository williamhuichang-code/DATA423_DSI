# =================================================================================
# mod_config_important.R
# =================================================================================

config_important_ui <- function(id) {
  ns <- NS(id)
  box(
    title = "Domain Important Variables", width = 6,
    tags$p("These variables are protected from exclusion during missingness handling and feature selection.",
           style = "font-size:13px; color:#6c757d;"),
    selectInput(ns("important_vars"), label = "Select Important Variables:",
                choices = NULL, selected = NULL, multiple = TRUE, width = "100%")
  )
}

config_important_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(get_data())
      updateSelectInput(session, "important_vars",
                        choices  = names(get_data()),
                        selected = isolate(input$important_vars))
    })
    
    return(reactive(input$important_vars))
  })
}