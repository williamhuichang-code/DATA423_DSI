# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(bs4Dash)
library(dplyr)
library(summarytools)
library(plotly)
library(visdat)
library(naniar)
library(gridExtra)


# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

##### change global configs when needed

# file of interest
FILE_OF_INTEREST <- "Ass21Data.csv"

# explicit data folder
DATA_WD <- "."


# ── FILES IN THE WORK DIRECTORY ──────────────────────────────────────────────

csv_files <- list.files(DATA_WD, pattern = "\\.csv$", full.names = FALSE)
file_choices <- if (length(csv_files) == 0) c("(none)") else csv_files
default_selected <- if (FILE_OF_INTEREST %in% csv_files) FILE_OF_INTEREST else "(none)"


# ── LOAD MODULE ──────────────────────────────────────────────────────────────
list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)
print(getwd())
print(csv_files)




# =================================================================================
# ui.R
# =================================================================================

ui <- dashboardPage(
  
  # ── HEADER ─────────────────────────────────────────────────────────────────
  
  header = dashboardHeader(
    title = "My App",
    selectInput(
      inputId  = "selected_file",
      label    = NULL,
      choices  = file_choices,
      selected = default_selected,
      width    = "200px"
    ),
    tags$li(
      class = "dropdown",
      tags$div(
        style = "padding: 0 30px; font-size: 24px; font-weight: 700; color: #212529; line-height: 50px;",
        "DATA423-26S1 Assignment 2 (EDA, Strategy, Model) \u2003 | \u2003 William Hui Chang (69051925)"
      )
    )
  ),
  
  
  # ── SIDEBAR NAV ────────────────────────────────────────────────────────────
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar"),
               menuSubItem("Summary",      tabName = "eda_summary"),
               menuSubItem("Vis Miss",     tabName = "eda_vis_miss"),
               menuSubItem("Rising Value", tabName = "eda_rising"),
               menuSubItem("Boxplot",      tabName = "eda_boxplot"),
               menuSubItem("Barchart",     tabName = "eda_barchart")
               # add more subtabs here
      ),
      
      menuItem("Config", tabName = "config", icon = icon("sliders"),
               menuSubItem("Global Seed",    tabName = "config_seed"),
               menuSubItem("Split Setting",  tabName = "config_split"),
               menuSubItem("Data Roles",     tabName = "data_roles"),
               menuSubItem("Important Vars", tabName = "config_important"),
               menuSubItem("Download Data",  tabName = "data_download", icon = icon("download"))
               # add more subtabs here
      ),
      
      menuItem("Miss Strategy", tabName = "miss", icon = icon("circle-question"),
               menuSubItem("Variants",       tabName = "miss_variants"),
               menuSubItem("Shadow Vars",    tabName = "miss_shadow"),
               menuSubItem("Not Applicable", tabName = "miss_napp"),
               menuSubItem("Excessive Miss", tabName = "miss_excessive"),
               menuSubItem("Imputation",     tabName = "miss_impute"),
               menuSubItem("Transform",      tabName = "miss_transform")
               # add more subtabs here
      ),
      
      menuItem("Out Strategy", tabName = "out", icon = icon("triangle-exclamation"),
               menuSubItem("Mahalanobis", tabName = "out_mah"),
               menuSubItem("iForest",     tabName = "out_iforest")
               # add more subtabs here
      ),
      
      menuItem("Model", tabName = "model", icon = icon("brain"),
               menuSubItem("Recipe",     tabName = "pre_recipe"),
               menuSubItem("Tune",       tabName = "model_tune"),
               menuSubItem("Regression", tabName = "model_reg")
               # add more subtabs here
      )
    )
  ),
  
  
  # ── BODY ───────────────────────────────────────────────────────────────────
  
  body = dashboardBody(
    tabItems(
      
      # EDA
      tabItem(tabName = "eda_summary",  summary_ui("summary")),
      tabItem(tabName = "eda_vis_miss", vis_miss_ui("vis_miss")),
      tabItem(tabName = "eda_rising",   rising_value_ui("rising_value")),
      tabItem(tabName = "eda_boxplot",  box(title = "Boxplot",  width = 12, "coming soon")),
      tabItem(tabName = "eda_barchart", box(title = "Barchart", width = 12, "coming soon")),
      
      # Config
      tabItem(tabName = "config_seed",      config_seed_ui("config_seed")),
      tabItem(tabName = "config_split",     split_ui("split")),
      tabItem(tabName = "data_roles",       data_roles_ui("data_roles")),
      tabItem(tabName = "config_important", config_important_ui("config_important")),
      tabItem(tabName = "data_download",    data_download_ui("data_download")),
      
      # Miss Strategy
      tabItem(tabName = "miss_variants",  miss_variants_ui("miss_variants")),
      tabItem(tabName = "miss_shadow",    miss_shadow_ui("miss_shadow")),
      tabItem(tabName = "miss_napp",      miss_napp_ui("miss_napp")),
      tabItem(tabName = "miss_excessive", miss_excessive_ui("miss_excessive")),
      tabItem(tabName = "miss_impute",    miss_impute_ui("miss_impute")),
      tabItem(tabName = "miss_transform",  miss_transform_ui("miss_transform")),
      
      # Out Strategy
      tabItem(tabName = "out_mah",     box(title = "Mahalanobis", width = 12, "coming soon")),
      tabItem(tabName = "out_iforest", box(title = "iForest",     width = 12, "coming soon")),
      
      # Model
      tabItem(tabName = "pre_recipe", prep_recipe_ui("prep_recipe")),
      tabItem(tabName = "model_tune",      model_tune_ui("model_tune")),
      tabItem(tabName = "model_reg",       model_reg_ui("model_reg"))
    )
  ),
  
  controlbar = NULL,
  footer     = NULL
)





# =================================================================================
# server.R
# =================================================================================

server <- function(input, output, session) {
  
  # ── RAW DATA ─────────────────────────────────────────────────────────────
  
  get_raw <- reactive({
    req(input$selected_file)
    read.csv(file.path(DATA_WD, input$selected_file),
             header = TRUE,
             na.strings = c('NA', 'N/A'),
             stringsAsFactors = TRUE)
  }) |> bindCache(input$selected_file)
  
  
  # ── DOMAIN CONFIGS ───────────────────────────────────────────────────────
  
  global_seed    <- config_seed_server("config_seed")
  split          <- split_server("split", get_raw, global_seed = global_seed)
  roles          <- data_roles_server("data_roles", split$data)
  important_vars <- config_important_server("config_important", split$data)
  
  
  # ── PIPELINE ──────────────────────────────────────────────────────────────
  
  variant   <- miss_variants_server("miss_variants",   split$data)
  shadow    <- miss_shadow_server("miss_shadow",       variant$data)
  napp      <- miss_napp_server("miss_napp",           shadow$data)
  excessive <- miss_excessive_server("miss_excessive", napp$data, important_vars)
  impute    <- miss_impute_server("miss_impute",       excessive$data, roles)
  transform <- miss_transform_server("miss_transform", impute$data, roles)
  precipe   <- prep_recipe_server("prep_recipe",       transform$data, roles, split)
  model_tune <- model_tune_server("model_tune", transform$data, roles, precipe$recipe)
  model_reg <- model_reg_server("model_reg", transform$data, roles, precipe$recipe, model_tune, get_raw)
  
  get_data <- transform$data   # current end of pipeline
  
  
  
  # ── DOWNLOAD AT ANY STAGE ─────────────────────────────────────────────────
  
  data_download_server("data_download", stages = list(
    "Raw"       = get_raw,
    "Processed" = get_data
  ))
  
  
  # ── MODULE CALLS ──────────────────────────────────────────────────────────
  
  summary_server("summary",           get_data)
  vis_miss_server("vis_miss",         get_data, roles)
  rising_value_server("rising_value", get_data)
  
}







# =================================================================================
# Run
# =================================================================================

shinyApp(ui, server)
