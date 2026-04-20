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
library(dbscan)       # LOF
library(e1071)        # SVM
library(randomForest) # RF
library(isotree)      # Isolation Forest
library(ggrepel)      # label repelling in plots

# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

# file of interest
FILE_OF_INTEREST <- "non_exist_file.csv"

# explicit data folder
DATA_WD <- "."


# ── FILE LOADING LOGIC ───────────────────────────────────────────────────────

# all csv files as a list
csv_files <- list.files(DATA_WD, pattern = "\\.csv$", full.names = FALSE)

# add a (none) option to choices
file_choices_with_none    <- c("(none)", csv_files)

# prioritise on interested file and fallback at (none)
default_selected <- if (FILE_OF_INTEREST %in% csv_files) FILE_OF_INTEREST else "(none)"


# ── MODULE LOADING LOGIC ─────────────────────────────────────────────────────

# look inside "modules" folder and its subs, load all files with .R according to their full paths
list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)





# =================================================================================
# ui.R
# =================================================================================

ui <- dashboardPage(
  
  # ── HEADER ─────────────────────────────────────────────────────────────────
  
  header = dashboardHeader(
    title = "My DSI Studio",
    selectInput(
      inputId  = "selected_file",
      label    = NULL,
      choices  = file_choices_with_none,
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
      
      menuItem("Config", tabName = "config", icon = icon("sliders"),
               menuSubItem("Data Roles",     tabName = "data_roles"),
               menuSubItem("Download Data",  tabName = "data_download", icon = icon("download"))
               # more future subtabs here
      ),
      
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar"),
               menuSubItem("Data Table",   tabName = "eda_datatable"),
               menuSubItem("Data Summary", tabName = "eda_summary"),
               menuSubItem("Word Cloud",   tabName = "eda_cloud"),
               menuSubItem("Vis Miss",     tabName = "eda_vis_miss"),
               menuSubItem("Miss Upset",   tabName = "eda_upset"),
               menuSubItem("Rising Value", tabName = "eda_rising"),
               menuSubItem("Mosaic",       tabName = "eda_mosaic"),
               menuSubItem("Tabplot",      tabName = "eda_tabplot"),
               menuSubItem("Heatmap",      tabName = "eda_heatmap"),
               menuSubItem("GGPairs",      tabName = "eda_ggpairs"),
               menuSubItem("Bar Chart",    tabName = "eda_bar")
               # more future subtabs here
      ),
      
      menuItem("Miss Strategy", tabName = "miss", icon = icon("circle-question"),
               menuSubItem("Variants",        tabName = "miss_variants"),
               menuSubItem("Shadow Vars",     tabName = "miss_shadow"),
               menuSubItem("Not Applicable",  tabName = "miss_napp"),
               menuSubItem("Excessive Miss",  tabName = "miss_excessive"),
               menuSubItem("Diag: Imputation", tabName = "miss_impute"),
               menuSubItem("Diag: Transform",  tabName = "miss_transform"),
               menuSubItem("Diag: Rpart",      tabName = "miss_rpart"),
               menuSubItem("Diag: Importance", tabName = "miss_importance")
               # more future subtabs here
      ),
      
      menuItem("Out Strategy", tabName = "out", icon = icon("triangle-exclamation"),
               menuSubItem("Histogram",        tabName = "out_hist"),
               menuSubItem("Boxplot",          tabName = "out_boxplot"),
               menuSubItem("Bagplot",          tabName = "out_bagplot"),
               menuSubItem("Mahalanobis",      tabName = "out_mah"),
               menuSubItem("Cook's Distance",  tabName = "out_cooks"),
               menuSubItem("LOF",              tabName = "out_lof"),
               menuSubItem("SVM",              tabName = "out_svm"),
               menuSubItem("Random Forest",    tabName = "out_rf"),
               menuSubItem("Isolation Forest", tabName = "out_iforest"),
               menuSubItem("Summary",          tabName = "out_summary"),
               menuSubItem("Response",         tabName = "out_response")
               # more future subtabs here
      ),
      
      menuItem("Model", tabName = "model", icon = icon("brain"),
               menuSubItem("Recipe",     tabName = "pre_recipe"),
               menuSubItem("Tune",       tabName = "model_tune"),
               menuSubItem("Regression", tabName = "model_reg")
               # more future subtabs here
      )
    )
  ),
  
  
  # ── BODY ───────────────────────────────────────────────────────────────────
  
  body = dashboardBody(
    tabItems(
      
      # Config
      tabItem(tabName = "data_roles",       data_roles_ui("data_roles")),
      tabItem(tabName = "data_download",    data_download_ui("data_download")),
      
      # EDA
      tabItem(tabName = "eda_datatable", eda_datatable_ui("eda_datatable")),
      tabItem(tabName = "eda_summary",   eda_summary_ui("eda_summary")),
      tabItem(tabName = "eda_cloud",     eda_cloud_ui("eda_cloud")),
      tabItem(tabName = "eda_vis_miss",  eda_vis_ui("eda_vis")),
      tabItem(tabName = "eda_upset",     eda_upset_ui("eda_upset")),
      tabItem(tabName = "eda_rising",    eda_rising_ui("eda_rising")),
      tabItem(tabName = "eda_mosaic",    eda_mosaic_ui("eda_mosaic")),
      tabItem(tabName = "eda_tabplot",   eda_tabplot_ui("eda_tabplot")),
      tabItem(tabName = "eda_heatmap",   eda_heatmap_ui("eda_heatmap")),
      tabItem(tabName = "eda_ggpairs",   eda_ggpairs_ui("eda_ggpairs")),
      tabItem(tabName = "eda_bar",       eda_bar_ui("eda_bar")),
      
      # Miss Strategy
      tabItem(tabName = "miss_variants",   miss_variants_ui("miss_variants")),
      tabItem(tabName = "miss_shadow",     miss_shadow_ui("miss_shadow")),
      tabItem(tabName = "miss_napp",       miss_napp_ui("miss_napp")),
      tabItem(tabName = "miss_excessive",  miss_excessive_ui("miss_excessive")),
      tabItem(tabName = "miss_rpart",      miss_rpart_ui("miss_rpart")),
      tabItem(tabName = "miss_impute",     miss_impute_ui("miss_impute")),
      tabItem(tabName = "miss_transform",  miss_transform_ui("miss_transform")),
      tabItem(tabName = "miss_importance", miss_importance_ui("miss_importance")),
      
      # Out Strategy
      tabItem(tabName = "out_hist",    out_histogram_ui("out_hist")),
      tabItem(tabName = "out_boxplot", out_boxplot_ui("out_boxplot")),
      tabItem(tabName = "out_bagplot", out_bagplot_ui("out_bagplot")),
      tabItem(tabName = "out_mah",     out_mahalanobis_ui("out_mah")),
      tabItem(tabName = "out_cooks",   out_cooks_ui("out_cooks")),
      tabItem(tabName = "out_lof",     out_lof_ui("out_lof")),
      tabItem(tabName = "out_svm",     out_svm_ui("out_svm")),
      tabItem(tabName = "out_rf",      out_rf_ui("out_rf")),
      tabItem(tabName = "out_iforest", out_iforest_ui("out_iforest")),
      tabItem(tabName = "out_summary", out_summary_ui("out_summary")),
      tabItem(tabName = "out_response", out_response_ui("out_response")),
      
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
    req(input$selected_file != "(none)")
    read.csv(file.path(DATA_WD, input$selected_file),
             header = TRUE,
             na.strings = c('NA', 'N/A'),
             stringsAsFactors = TRUE)
  }) |> bindCache(input$selected_file)
  
  
  # ── DOMAIN CONFIGS ────────────────────────────────────────────────────────
  
  config         <- data_roles_server("data_roles", get_raw)
  config_data    <- config$data            # reactive df (raw + optional split col)
  roles          <- config$roles           # reactive named list of role assignments
  important_vars <- config$important_vars  # reactive character vector
  seed_in_use    <- config$seed            # reactive integer
  
  
  # ── PIPELINE (MANUAL) ─────────────────────────────────────────────────────
  
  # Note: 
  #   works like OOP in python (e.g., df.variants().shadow().excessive())
  #   but R doesn't have class and class methods
  #   for modularised app design, this is the best choice already
  
  # OOP for missingness and outlier strategies
  variant      <- miss_variants_server("miss_variants",   config_data)
  shadow       <- miss_shadow_server("miss_shadow",       variant$data)
  napp         <- miss_napp_server("miss_napp",           shadow$data)
  excessive    <- miss_excessive_server("miss_excessive", napp$data, important_vars)
  out_response <- out_response_server("out_response",     excessive$data, get_raw, roles)
  
  # exploratory diagnostics for imputation and standarisation choices
  impute    <- miss_impute_server("miss_impute",       out_response$data, roles)
  transform <- miss_transform_server("miss_transform", impute$data, roles)
  
  # get data for different purposes (modelling and diagnose respectively)
  get_model_data    <- out_response$data
  get_diagnose_data <- transform$data
  
  # download df at any stage
  data_download_server("data_download", stages = list(
    "Raw"       = get_raw,
    "Processed" = get_diagnose_data
  ))
  
  # EDA visualisations
  eda_datatable_server("eda_datatable", get_diagnose_data, get_raw)
  eda_summary_server("eda_summary",     get_diagnose_data)
  eda_cloud_server("eda_cloud",         get_diagnose_data)
  eda_vis_server("eda_vis",             get_diagnose_data, roles)
  eda_upset_server("eda_upset",         get_diagnose_data, roles)
  eda_rising_server("eda_rising",       get_diagnose_data, roles)
  eda_mosaic_server("eda_mosaic",       get_diagnose_data, roles)
  eda_tabplot_server("eda_tabplot",     get_diagnose_data, roles)
  eda_heatmap_server("eda_heatmap",     get_diagnose_data, roles)
  eda_ggpairs_server("eda_ggpairs",     get_diagnose_data, roles)
  eda_bar_server("eda_bar",             get_diagnose_data)
  
  # diagnostics visualisations
  miss_rpart_server("miss_rpart",                  get_diagnose_data, roles)
  miss_importance_server("miss_importance",        get_diagnose_data, roles)
  out_histogram_server("out_hist",                 get_diagnose_data, roles)
  out_boxplot_server("out_boxplot",                get_diagnose_data, get_raw, roles)
  out_bagplot_server("out_bagplot",                get_diagnose_data, get_raw, roles)
  out_mah     <- out_mahalanobis_server("out_mah", get_diagnose_data, get_raw, roles)
  out_cooks   <- out_cooks_server("out_cooks",     get_diagnose_data, get_raw, roles)
  out_lof     <- out_lof_server("out_lof",         get_diagnose_data, get_raw, roles)
  out_svm     <- out_svm_server("out_svm",         get_diagnose_data, get_raw, roles)
  out_rf      <- out_rf_server("out_rf",           get_diagnose_data, get_raw, roles)
  out_iforest <- out_iforest_server("out_iforest", get_diagnose_data, get_raw, roles)
  out_summary <- out_summary_server("out_summary", get_diagnose_data, get_raw, roles,
                                    out_mah$flagged, out_cooks$flagged,
                                    out_lof$flagged, out_svm$flagged,
                                    out_rf$flagged,  out_iforest$flagged)
  
  
  # ── PIPELINE (AUTO) ───────────────────────────────────────────────────────
  
  # OOP for recipe-based preprocessing and modelling
  precipe    <- prep_recipe_server("prep_recipe", get_model_data, roles, seed_in_use)
  model_tune <- model_tune_server("model_tune",   get_model_data, roles, precipe$recipe, seed_in_use)
  model_reg <- model_reg_server("model_reg",      get_model_data, roles, precipe$recipe, model_tune, get_raw, seed_in_use)
  
  
}





# =================================================================================
# Run
# =================================================================================

shinyApp(ui, server)
