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
  
  # manual ones
  # missingness ones
  variant   <- miss_variants_server("miss_variants",   split$data)
  shadow    <- miss_shadow_server("miss_shadow",       variant$data)
  napp      <- miss_napp_server("miss_napp",           shadow$data)
  excessive <- miss_excessive_server("miss_excessive", napp$data, important_vars)
  # outlier one
  out_response <- out_response_server("out_response",  excessive$data, get_raw, roles)
  # explore only
  impute    <- miss_impute_server("miss_impute",       out_response$data, roles)
  transform <- miss_transform_server("miss_transform", impute$data, roles)
  
  
  # recipe ones
  # preprocessing one
  precipe   <- prep_recipe_server("prep_recipe",       out_response$data, roles, split)
  # model ones
  model_tune <- model_tune_server("model_tune", out_response$data, roles, precipe$recipe)
  model_reg <- model_reg_server("model_reg", out_response$data, roles, precipe$recipe, model_tune, get_raw)
  
  
  get_data <- impute$data   # current end of pipeline
  
  
  
  # ── DOWNLOAD AT ANY STAGE ─────────────────────────────────────────────────
  
  data_download_server("data_download", stages = list(
    "Raw"       = get_raw,
    "Processed" = get_data
  ))
  
  
  # ── MODULE CALLS ──────────────────────────────────────────────────────────
  
  eda_datatable_server("eda_datatable", get_data, get_raw)
  eda_summary_server("eda_summary",     get_data)
  eda_cloud_server("eda_cloud",         get_data)
  eda_vis_server("eda_vis",             get_data, roles)
  eda_upset_server("eda_upset",         get_data, roles)
  eda_rising_server("eda_rising",       get_data, roles)
  eda_mosaic_server("eda_mosaic",       get_data, roles)
  eda_tabplot_server("eda_tabplot",     get_data, roles)
  eda_heatmap_server("eda_heatmap",     get_data, roles)
  eda_ggpairs_server("eda_ggpairs",     get_data, roles)
  eda_bar_server("eda_bar",             get_data)
  
  
  miss_rpart_server("miss_rpart",           get_data, roles)
  miss_importance_server("miss_importance", get_data, roles)
  
  
  out_histogram_server("out_hist",                   get_data, roles)
  out_boxplot_server("out_boxplot",                  get_data, get_raw, roles)
  out_bagplot_server("out_bagplot",                  get_data, get_raw, roles)
  out_mah     <- out_mahalanobis_server("out_mah",   get_data, get_raw, roles)
  out_cooks   <- out_cooks_server("out_cooks",       get_data, get_raw, roles)
  out_lof     <- out_lof_server("out_lof",           get_data, get_raw, roles)
  out_svm     <- out_svm_server("out_svm",           get_data, get_raw, roles)
  out_rf      <- out_rf_server("out_rf",             get_data, get_raw, roles)
  out_iforest <- out_iforest_server("out_iforest",   get_data, get_raw, roles)
  out_summary <- out_summary_server("out_summary",   get_data, get_raw, roles,
                                    out_mah$flagged, out_cooks$flagged,
                                    out_lof$flagged, out_svm$flagged,
                                    out_rf$flagged,  out_iforest$flagged)
}




