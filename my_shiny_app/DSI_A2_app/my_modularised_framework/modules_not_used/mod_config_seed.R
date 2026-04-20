# =================================================================================
# mod_config_seed.R
# =================================================================================

config_seed_ui <- function(id) {
  ns <- NS(id)
  box(
    title = "Global Seed", width = 4,
    tags$p("Set a global random seed for reproducibility across all stochastic steps (e.g. ratio split, iForest).",
           style = "font-size:13px; color:#6c757d;"),
    numericInput(ns("global_seed"), label = "Seed value:",
                 value = as.integer(format(Sys.Date(), "%Y")),
                 min = 0, step = 1, width = "100%")
  )
}

config_seed_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive(input$global_seed %||% 42))
  })
}