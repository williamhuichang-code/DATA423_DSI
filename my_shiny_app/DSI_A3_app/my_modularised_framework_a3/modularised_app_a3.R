# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(bs4Dash)
library(dplyr)
library(waiter)
library(plotly)
library(ggrepel)
library(ggplot2)
library(DT)
library(recipes)
library(shinycssloaders) # busy spinner (compatibility with bs4Dash TBC)
library(cluster)
# caret, doParallel, cli, butcher, glmnet, pls, rpart, rpart.plot loaded in mod_meth_shared.R


# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

# file of interest
FILE_OF_INTEREST <- ""

# explicit data folder
DATA_WD <- "."

# sets R to display numbers with 3 significant digits globally
DIGITS = 3


# ── TASK SPECIFIC DEFAULT ────────────────────────────────────────────────────

# Assignment 3 specific starting values, might comment out for general use
FILE_OF_INTEREST <- "Ass3Data.csv"

general_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                     "other", "dummy", 
                     "zv", "nzv", "center", "scale")

glmnet_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                    "other", "YeoJohnson", "dummy", 
                    "interact", "lincomb",
                    "zv", "nzv", "center", "scale")

svmrs_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                    "other", "YeoJohnson", "dummy", 
                    "interact", "lincomb",
                    "zv", "nzv", "center", "scale")

brnn_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                   "other", "YeoJohnson", "dummy", 
                   "interact", "lincomb",
                   "zv", "nzv", "center", "scale")

ppr_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                 "other", "YeoJohnson", "dummy", 
                 "interact", "lincomb",
                 "zv", "nzv", "center", "scale")

svmpl_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                 "other", "YeoJohnson", "dummy", 
                 "interact", "lincomb",
                 "zv", "nzv", "center", "scale")

krls_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                  "other", "YeoJohnson", "dummy", 
                  "interact", "lincomb",
                  "zv", "nzv", "center", "scale")

gausspr_initial <- c("impute_bag", "dateDecimal", "month", "week", "dow",
                     "other", "YeoJohnson", "dummy", 
                     "interact", "lincomb",
                     "zv", "nzv", "center", "scale")


task_specific_roles <- list(
  Patient         = "obs_id",
  Response        = "outcome",
  ObservationDate = "date"
)

SPLIT_SEED   <- 199
MODEL_SEED   <- 673
AUTO_SPLIT   <- TRUE
SPLIT_RATIO  <- 0.8

# Available Methods explorer — tags to exclude from the table/map by default,
# and tags to highlight as literature-informed choices.
# Fall back to character(0) (nothing pre-selected) when not defined.
av_exclude_tags <- c(
  "Two Class Only", "ROC Curves", "Text Mining", "String Kernel",
  "Self-Organising Maps", "Binary Predictors Only",
  "Categorical Predictors Only", "Cost Sensitive Learning",
  "Ordinal Outcomes"
)

av_highlight_tags <- c(
  "Regularization", "L1 Regularization", "L2 Regularization",
  "Implicit Feature Selection", "Feature Selection Wrapper",
  "Handle Missing Predictor Data", "Robust Methods",
  "Multivariate Adaptive Regression Splines"
)

A3_omit_ids <- c(
  "tid-57748", "tid-57237", "tid-57537", "tid-57651",
  "tid-57689", "tid-57787", "tid-57761", "tid-57431",
  "tid-57479", "tid-57487", "tid-57600", "tid-57732",
  "tid-57739", "tid-57808", "tid-57845", "tid-57859",
  "tid-57921", "tid-57928", "tid-58050", "tid-58055",
  "tid-57470", "tid-57569", "tid-57580", "tid-57899"
)


# ── FILE LOADING LOGIC ───────────────────────────────────────────────────────

# all csv files as a list
csv_files <- list.files(DATA_WD, pattern = "\\.csv$", full.names = FALSE)

# add a (none) option to choices
file_choices_with_none    <- c("(none)", csv_files)

# prioritise on interested file and fallback at (none)
default_selected <- if (FILE_OF_INTEREST %in% csv_files) FILE_OF_INTEREST else "(none)"


# ── PREPROCESSING UTILITIES ──────────────────────────────────────────────────
# defined before module loading so category UIs can reference ppchoices at build time
# dynamicSteps() is defined in modules/mod_meth_tune.R (sourced into globalenv)

ppchoices <- c(
  "impute_knn", "impute_bag", "impute_median", "impute_mode",
  "YeoJohnson", "BoxCox", "log", "sqrt",
  "naomit",
  "pca", "pls", "ica",
  "center", "scale", "range", "spatialsign",
  "year", "quarter", "month", "week", "dow", "dateDecimal",
  "nzv", "zv", "other", "dummy",
  "poly", "interact", "lincomb",
  "indicate_na", "corr"
)

# ── MODULE LOADING LOGIC ─────────────────────────────────────────────────────

# look inside "modules" folder and its subs, load all files with .R according to their full paths
list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)


# ── AESTHETIC LOGIC ──────────────────────────────────────────────────────────

# sets R to display numbers with n significant digits globally (set it in global config)
options(digits = DIGITS)




# =================================================================================
# ui.R
# =================================================================================

ui <- dashboardPage(
  
  preloader = list(
    html  = tagList(
      waiter::spin_flower(),
      h4("Loading My DSI Studio...", style = "color:#D6DEE6; margin-top:10px;")
    ),
    color = "#49545C"
  ),
  
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
        "DATA423-26S1 Assignment 3 (Model Discovery & Selection) \u2003 | \u2003 William Hui Chang (69051925)"
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
               menuSubItem("Bar Chart",    tabName = "eda_bar"),
               menuSubItem("Box Plot",     tabName = "eda_boxplot"),
               menuSubItem("Interaction", tabName = "eda_interaction")
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
      
      menuItem("Available Methods", icon = icon("list-check"),
               menuSubItem("Method Table", tabName = "meth_av_table", icon = icon("table")),
               menuSubItem("Method Map",   tabName = "meth_av_map",   icon = icon("map"))
      ),

      menuItem("Methods", icon = icon("flask"),
               menuSubItem("Null",            tabName = "meth_null",     icon = icon("minus")),
               menuSubItem("OLS",             tabName = "meth_ols",      icon = icon("chart-line")),
               menuSubItem("Tree Based",      tabName = "meth_tree",     icon = icon("tree")),
               menuSubItem("Kernel Methods",  tabName = "meth_kernel",   icon = icon("circle-nodes")),
               menuSubItem("Ensembles",       tabName = "meth_ensemble", icon = icon("layer-group")),
               menuSubItem("Neural Networks", tabName = "meth_nn",       icon = icon("brain")),
               menuSubItem("WildCards",       tabName = "meth_wildcard", icon = icon("wand-magic-sparkles"))
      ),

      menuItem("Model Selection", tabName = "meth_select", icon = icon("trophy")),
      menuItem("Performance",    tabName = "meth_perf",   icon = icon("gauge-high"))

    )
  ),
  
  
  # ── BODY ───────────────────────────────────────────────────────────────────
  
  body = dashboardBody(
    tabItems(
      
      # Config
      tabItem(tabName = "data_roles",       data_roles_ui("data_roles",
                                            default_seed = if (exists("SPLIT_SEED"))  SPLIT_SEED  else NULL,
                                            split_ratio  = if (exists("SPLIT_RATIO")) SPLIT_RATIO else NULL)),
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
      tabItem(tabName = "eda_boxplot",   eda_boxplot_ui("eda_boxplot")),
      tabItem(tabName = "eda_interaction", eda_interaction_ui("eda_interaction")),
      
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
      
      # Available Methods explorer — two sidebar sub-items, one shared server
      tabItem(tabName = "meth_av_table",
              meth_available_table_ui("meth_available")),
      tabItem(tabName = "meth_av_map",
              meth_available_map_ui("meth_available")),

      # Methods — one tabItem per category
      tabItem(tabName = "meth_null",
              meth_null_ui("meth_null",
                           pp_choices         = ppchoices,
                           default_preprocess = if (exists("general_initial")) general_initial else character(0),
                           model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),

      tabItem(tabName = "meth_ols",
              meth_ols_ui("meth_ols",
                          pp_choices         = ppchoices,
                          default_preprocess = if (exists("general_initial")) general_initial else character(0),
                          model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),

      tabItem(tabName = "meth_tree",
              meth_tree_ui("meth_tree",
                           pp_choices         = ppchoices,
                           default_preprocess = if (exists("general_initial")) general_initial else character(0),
                           model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),

      tabItem(tabName = "meth_nn",
              meth_nn_ui("meth_nn",
                         pp_choices         = ppchoices,
                         default_preprocess = if (exists("general_initial")) general_initial else character(0),
                         model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      tabItem(tabName = "meth_kernel",
              meth_kernel_ui("meth_kernel",
                             pp_choices         = ppchoices,
                             default_preprocess = if (exists("general_initial")) general_initial else character(0),
                             model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      tabItem(tabName = "meth_ensemble",
              meth_ensemble_ui("meth_ensemble",
                               pp_choices         = ppchoices,
                               default_preprocess = if (exists("general_initial")) general_initial else character(0),
                               model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),
      tabItem(tabName = "meth_wildcard",
              meth_wildcard_ui("meth_wildcard",
                               pp_choices         = ppchoices,
                               default_preprocess = if (exists("general_initial")) general_initial else character(0),
                               model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL)),

      tabItem(tabName = "meth_select",
              meth_select_ui("meth_select")),

      tabItem(tabName = "meth_perf",
              meth_perf_ui("meth_perf"))

    )
  ),
  
  controlbar = NULL,
  footer     = NULL
)





# =================================================================================
# server.R
# =================================================================================

server <- function(input, output, session) {

  if (!dir.exists("./SavedModels")) dir.create("./SavedModels")
  shiny::onSessionEnded(stopApp)

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
  
  config         <- data_roles_server("data_roles", get_raw,
                                     default_roles = if (exists("task_specific_roles")) task_specific_roles else NULL,
                                     auto_split    = if (exists("AUTO_SPLIT"))  AUTO_SPLIT  else FALSE,
                                     split_ratio   = if (exists("SPLIT_RATIO")) SPLIT_RATIO else NULL)
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
  out_response <- out_response_server("out_response",     excessive$data, get_raw, roles,
                                     default_omit_ids = if (exists("A3_omit_ids")) A3_omit_ids else NULL)
  
  # exploratory diagnostics for imputation and standarisation choices
  impute    <- miss_impute_server("miss_impute",       out_response$data, roles, seed_in_use)
  transform <- miss_transform_server("miss_transform", impute$data,       roles, seed_in_use)
  
  # get data for different purposes (modelling and diagnose respectively)
  get_model_data    <- out_response$data
  get_diagnose_data <- transform$data
  
  # download df at any stage
  data_download_server("data_download", stages = list(
    "Raw"       = get_raw,
    "Processed" = get_diagnose_data
  ))
  
  # EDA visualisations
  eda_datatable_server("eda_datatable",     get_diagnose_data, get_raw)
  eda_summary_server("eda_summary",         get_diagnose_data)
  eda_cloud_server("eda_cloud",             get_diagnose_data)
  eda_vis_server("eda_vis",                 get_diagnose_data, roles)
  eda_upset_server("eda_upset",             get_diagnose_data, roles)
  eda_rising_server("eda_rising",           get_diagnose_data, roles)
  eda_mosaic_server("eda_mosaic",           get_diagnose_data, roles)
  eda_tabplot_server("eda_tabplot",         get_diagnose_data, roles)
  eda_heatmap_server("eda_heatmap",         get_diagnose_data, roles)
  eda_ggpairs_server("eda_ggpairs",         get_diagnose_data, roles)
  eda_bar_server("eda_bar",                 get_diagnose_data)
  eda_boxplot_server("eda_boxplot",        get_diagnose_data)
  eda_interaction_server("eda_interaction", get_diagnose_data, roles)
  
  # diagnostics visualisations
  miss_rpart_server("miss_rpart",                  get_diagnose_data, roles)
  miss_importance_server("miss_importance",        get_diagnose_data, roles)
  out_histogram_server("out_hist",                 get_diagnose_data, roles)
  out_boxplot_server("out_boxplot",                get_diagnose_data, get_raw, roles)
  out_bagplot_server("out_bagplot",                get_diagnose_data, get_raw, roles)
  out_mah     <- out_mahalanobis_server("out_mah", get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_cooks   <- out_cooks_server("out_cooks",     get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_lof     <- out_lof_server("out_lof",         get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_svm     <- out_svm_server("out_svm",         get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_rf      <- out_rf_server("out_rf",           get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_iforest <- out_iforest_server("out_iforest", get_diagnose_data, get_raw, roles, seed = seed_in_use)
  out_summary <- out_summary_server("out_summary", get_diagnose_data, get_raw, roles,
                                    out_mah$flagged, out_cooks$flagged,
                                    out_lof$flagged, out_svm$flagged,
                                    out_rf$flagged,  out_iforest$flagged,
                                    seed = seed_in_use)
  
  
  # ── AVAILABLE METHODS ────────────────────────────────────────────────────
  meth_available_server("meth_available",
    exclude_tags   = if (exists("av_exclude_tags"))   av_exclude_tags   else NULL,
    highlight_tags = if (exists("av_highlight_tags")) av_highlight_tags else NULL
  )

  # ── PIPELINE (AUTO) ───────────────────────────────────────────────────────

  # Methods modules — one server instance per active category
  meth_null <- meth_null_server("meth_null", get_model_data, roles,
                   seed               = seed_in_use,
                   model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                   general_preprocess = if (exists("general_initial")) general_initial else NULL,
                   pp_choices         = ppchoices)

  meth_ols  <- meth_ols_server("meth_ols",  get_model_data, roles,
                   seed               = seed_in_use,
                   model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                   general_preprocess = if (exists("general_initial")) general_initial else NULL,
                   glmnet_preprocess  = if (exists("glmnet_initial"))  glmnet_initial  else NULL,
                   rlm_preprocess     = if (exists("rlm_initial"))     rlm_initial     else NULL,
                   lm_preprocess      = if (exists("lm_initial"))      lm_initial      else NULL,
                   pp_choices         = ppchoices)

  meth_tree <- meth_tree_server("meth_tree", get_model_data, roles,
                   seed               = seed_in_use,
                   model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                   general_preprocess = if (exists("general_initial")) general_initial else NULL,
                   rpart_preprocess   = if (exists("rpart_initial"))   rpart_initial   else NULL,
                   pp_choices         = ppchoices)

  meth_kernel <- meth_kernel_server("meth_kernel", get_model_data, roles,
                   seed                     = seed_in_use,
                   model_seed               = if (exists("MODEL_SEED"))       MODEL_SEED      else NULL,
                   general_preprocess       = if (exists("general_initial"))  general_initial else NULL,
                   svm_preprocess           = if (exists("svmrs_initial"))    svmrs_initial   else NULL,
                   svmpoly_preprocess       = if (exists("svmpl_initial"))    svmpl_initial   else NULL,
                   krlspoly_preprocess      = if (exists("krls_initial"))     krls_initial    else NULL,
                   gp_preprocess            = if (exists("gausspr_initial"))  gausspr_initial else NULL,
                   gaussprpoly_preprocess   = if (exists("gausspr_initial"))  gausspr_initial else NULL,
                   gaussprlinear_preprocess = if (exists("gausspr_initial"))  gausspr_initial else NULL,
                   pp_choices               = ppchoices)

  meth_ensemble <- meth_ensemble_server("meth_ensemble", get_model_data, roles,
                   seed               = seed_in_use,
                   model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                   general_preprocess = if (exists("general_initial")) general_initial else NULL,
                   cubist_preprocess  = if (exists("cubist_initial"))  cubist_initial  else NULL,
                   ranger_preprocess  = if (exists("ranger_initial"))  ranger_initial  else NULL,
                   pp_choices         = ppchoices)

  meth_nn <- meth_nn_server("meth_nn", get_model_data, roles,
                   seed               = seed_in_use,
                   model_seed         = if (exists("MODEL_SEED"))        MODEL_SEED       else NULL,
                   general_preprocess = if (exists("general_initial"))   general_initial  else NULL,
                   qrnn_preprocess    = if (exists("qrnn_initial"))      qrnn_initial     else NULL,
                   brnn_preprocess    = if (exists("brnn_initial"))      brnn_initial     else NULL,
                   pcannet_preprocess = if (exists("pcannet_initial"))   pcannet_initial  else NULL,
                   mlpwd_preprocess   = if (exists("mlpwd_initial"))     mlpwd_initial    else NULL,
                   mlpml_preprocess   = if (exists("mlpml_initial"))     mlpml_initial    else NULL,
                   monmlp_preprocess  = if (exists("monmlp_initial"))    monmlp_initial   else NULL,
                   pp_choices         = ppchoices)

  meth_wildcard <- meth_wildcard_server("meth_wildcard", get_model_data, roles,
                   seed               = seed_in_use,
                   model_seed         = if (exists("MODEL_SEED")) MODEL_SEED else NULL,
                   general_preprocess = if (exists("general_initial")) general_initial else NULL,
                   earth_preprocess   = if (exists("earth_initial"))   earth_initial   else NULL,
                   m5_preprocess      = if (exists("m5_initial"))      m5_initial      else NULL,
                   ppr_preprocess     = if (exists("ppr_initial"))     ppr_initial     else NULL,
                   pp_choices         = ppchoices)

  # ── Aggregate all trained models for Model Selection ──────────────────────
  # Each method module returns $models (reactiveValues). Merge here in the app
  # file so neither module depends on the other.
  get_all_models <- reactive({
    all <- c(
      reactiveValuesToList(meth_null$models),
      reactiveValuesToList(meth_ols$models),
      reactiveValuesToList(meth_tree$models),
      reactiveValuesToList(meth_kernel$models),
      reactiveValuesToList(meth_ensemble$models),
      reactiveValuesToList(meth_nn$models),
      reactiveValuesToList(meth_wildcard$models)
    )
    Filter(Negate(is.null), all)
  })

  meth_select <- meth_select_server("meth_select", get_models = get_all_models)

  meth_perf_server("meth_perf",
                   get_data   = get_model_data,
                   roles      = roles,
                   get_models = get_all_models,
                   choice     = meth_select$choice)

  
  
}





# =================================================================================
# Run
# =================================================================================

shinyApp(ui, server)