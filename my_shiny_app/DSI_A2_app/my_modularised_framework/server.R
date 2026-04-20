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




