# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(shinyBS) # Additional Bootstrap Controls (tooltips)
library(DT)
library(corrgram)
library(visdat)
library(shinycssloaders) # busy spinner
library(recipes) # The recipes package is central to preprocessing
library(doParallel) # We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(caret) # This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(rlang)
#library(rJava)
library(devtools)
library(BiocManager)
library(plyr)
library(dplyr)
library(cli)
if (!library("mixOmics", logical.return = TRUE)) {
  BiocManager::install("mixOmics", update = FALSE, ask = FALSE)  # This has moved from CRAN to Bioconductor
}
library(mixOmics)
library(rlist)
library(ggplot2)
library(butcher)

# added libraries
library(cluster)
library(ggrepel)



# ── CONFIGS ──────────────────────────────────────────────────────────────────

# sets R to display numbers with 3 significant digits globally
options(digits = 3)


# default outlier strategy

potential_outliers <- c(
  "tid-57748", "tid-57237", "tid-57537", "tid-57651", "tid-57689",
  "tid-57361", "tid-57431", "tid-57479", "tid-57487", "tid-57500",
  "tid-57732", "tid-57739", "tid-57877", "tid-57808", "tid-57845",
  "tid-57859", "tid-57921", "tid-58028", "tid-58060", "tid-58055",
  "tid-57470", "tid-57580", "tid-57899"
)

# potential_outliers <- c(
#   "tid-57748", "tid-57537", "tid-57689", "tid-57732", 
#   "tid-57449", "tid-57651", "tid-57787"
# )


# default preprocessing selections shown in each method's dropdown (make sure to set these to my best recommendation)
general_initial <- c("impute_bag", "dateDecimal", "quarter", "month", "week", "dow",
                     "other", "YeoJohnson", "dummy", "zv", "nzv", "center", "scale")

glmnet_initial <- c("impute_bag", "dateDecimal", "quarter", "month", "week", "dow",
                    "other", "YeoJohnson", "dummy", "interact", "lincomb",
                    "zv", "nzv", "center", "scale")

nn_initial <- c("impute_bag", "dateDecimal", "quarter", "month", "week", "dow",
                "other", "YeoJohnson", "dummy", "zv", "nzv", "center", "scale")

pls_initial <- c("impute_median", "month", "dow", "dateDecimal",
                 "other", "dummy", "zv", "nzv", "YeoJohnson", "center", "scale")

rpart_initial <- c("impute_median", "month", "dow", "dateDecimal",
                   "other", "zv", "nzv")


# maintenance point ---------------------------------------------------------------------------------------------------------------------------
# add further preprocessing choices for the new methods here


# ── PARALLEL LOGIC ───────────────────────────────────────────────────────────

startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    outfile <- tempfile(pattern = "output")
    unlink(outfile)
    clus <- makeCluster(min(c(3,detectCores(all.tests = FALSE, logical = TRUE))), outfile = outfile)
    registerDoParallel(clus)
    list("cluster" = clus, "outfile" = outfile)
  } else {
    NULL
  }
}

stopMode <- function(obj) {
  if (!is.null(obj)) {
    stopCluster(obj$cluster)
    lines <- readLines(con = obj$outfile)
    lapply(paste0(lines, "\n"), FUN = cat)
    unlink(obj$outfile)
    registerDoSEQ()
  }
}



# ── PREPROCESSING LOGIC ──────────────────────────────────────────────────────

ppchoices <- c("impute_knn", "impute_bag", "impute_median", "impute_mode", "YeoJohnson", "BoxCox", "log", "sqrt", "naomit", 
               "pca", "pls", "ica", "center", "scale", "range", "spatialsign", "year", "quarter","month", "week", "dow", "dateDecimal",
               "nzv", "zv", "other", "dummy", "poly", "interact", "lincomb", "indicate_na", "corr")

# This function turns the method's selected preprocessing into a recipe that honours the same order. 
# You are allowed to add more recipe steps to this.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else if (length(x) == 1 && is.na(x)) {
    y
  } else {
    x
  }
}


dynamicSteps <- function(recipe, preprocess, cfg = list()) {
  if (is.null(preprocess)) {
    stop("The preprocess list is NULL - check that you are using the correct control identifier")
  }
  
  for (s in preprocess) {
    if (s == "impute_knn") {
      recipe <- step_impute_knn(recipe, all_numeric_predictors(), all_nominal_predictors(),
                                neighbors = cfg$impute_knn_neighbors %||% 2)
      
    } else if (s == "impute_bag") {
      recipe <- step_impute_bag(recipe, all_numeric_predictors(), all_nominal_predictors(),
                                trees = cfg$impute_bag_trees %||% 4)
      
    } else if (s == "impute_median") {
      recipe <- recipes::step_impute_median(recipe, all_numeric_predictors())
      
    } else if (s == "impute_mode") {
      recipe <- recipes::step_impute_mode(recipe, all_nominal_predictors())
      
    } else if (s == "YeoJohnson") {
      recipe <- recipes::step_YeoJohnson(recipe, all_numeric_predictors())
      
    } else if (s == "BoxCox") {
      recipe <- recipes::step_BoxCox(recipe, all_numeric_predictors())
      
    } else if (s == "log") {
      recipe <- recipes::step_log(recipe, all_numeric_predictors())
      
    } else if (s == "sqrt") {
      recipe <- recipes::step_sqrt(recipe, all_numeric_predictors())
      
    } else if (s == "naomit") {
      recipe <- recipes::step_naomit(recipe, all_predictors(), skip = TRUE)
      
    } else if (s == "pca") {
      recipe <- recipes::step_pca(recipe, all_numeric_predictors(),
                                  num_comp = cfg$pca_num_comp %||% 25)
      
    } else if (s == "pls") {
      recipe <- recipes::step_pls(recipe, all_numeric_predictors(), outcome = "Response",
                                  num_comp = cfg$pls_num_comp %||% 25)
      
    } else if (s == "ica") {
      recipe <- recipes::step_ica(recipe, all_numeric_predictors(),
                                  num_comp = cfg$ica_num_comp %||% 25)
      
    } else if (s == "center") {
      recipe <- recipes::step_center(recipe, all_numeric_predictors())
      
    } else if (s == "scale") {
      recipe <- recipes::step_scale(recipe, all_numeric_predictors())
      
    } else if (s == "range") {
      recipe <- recipes::step_range(recipe, all_numeric_predictors())
      
    } else if (s == "spatialsign") {
      recipe <- recipes::step_spatialsign(recipe, all_numeric_predictors())
      
    } else if (s == "year") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("year"), ordinal = FALSE)
      
    } else if (s == "quarter") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("quarter"), ordinal = FALSE)
      
    } else if (s == "month") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("month"), ordinal = FALSE)
      
    } else if (s == "week") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("week"), ordinal = FALSE)
      
    } else if (s == "dow") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("dow"), ordinal = FALSE)
      
    } else if (s == "dateDecimal") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)
      
    } else if (s == "zv") {
      recipe <- recipes::step_zv(recipe, all_predictors())
      
    } else if (s == "nzv") {
      recipe <- recipes::step_nzv(recipe, all_predictors(),
                                  freq_cut = cfg$nzv_freq_cut %||% 95/5,
                                  unique_cut = cfg$nzv_unique_cut %||% 10)
      
    } else if (s == "other") {
      recipe <- recipes::step_other(recipe, all_nominal_predictors(),
                                    threshold = cfg$other_threshold %||% 0.05,
                                    other = cfg$other_label %||% "other")
      
    } else if (s == "dummy") {
      recipe <- recipes::step_dummy(recipe, all_nominal_predictors(), one_hot = FALSE)
      
    } else if (s == "poly") {
      recipe <- recipes::step_poly(recipe, all_numeric_predictors(),
                                   degree = cfg$poly_degree %||% 2)
      
    } else if (s == "interact") {
      recipe <- recipes::step_interact(recipe, terms = ~ all_numeric_predictors():all_numeric_predictors())
      
    } else if (s == "lincomb") {
      recipe <- recipes::step_lincomb(recipe, all_numeric_predictors())
      
    } else if (s == "indicate_na") {
      recipe <- recipes::step_indicate_na(recipe, all_predictors())
      
    } else if (s == "corr") {
      recipe <- recipes::step_corr(recipe, all_numeric_predictors(),
                                   threshold = cfg$corr_threshold %||% 0.9)
      
    } else if (s == "rm") {
      # intentionally blank
      
    } else {
      stop(paste("Attempting to use an unknown recipe step:", s))
    }
  }
  
  recipe
}




# ── MODEL HELPER (DESRIPTION) ────────────────────────────────────────────────

# takes a model name and returns a clean human-readable summary of what that model does
description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}


# ── MODEL HELPER (SAVE/LOAD/DELETE) ──────────────────────────────────────────

# model saver
saveToRds <- function(model, name) {
  # attempts to keep the model file size small by not saving the global environment with each model
  try(
    # ensure that WEKA based models can be restored
    if (!is.null(model$finalModel$classifier)) {
      rJava::.jcache(model$finalModel$classifier)
    }, silent = TRUE
  )
  
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  model2 <- butcher::axe_env(model, verbose = TRUE) # strip environments from the object to keep its size small
  #print(butcher::weigh(model2, threshold = 5, units = "MB"))
  saveRDS(model2, file)
}


# model loader
loadRds <- function(name, session) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (!file.exists(rdsfile)) {
    showNotification("Model needs to be trained first", session = session, duration = 3)
    return(NULL)
  }
  showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
  model <- readRDS(file = rdsfile)
  
  # try to update the preprocessing steps with the ones that were used
  steps <- model$recipe$steps
  seld <- c()
  for (step in steps) {
    s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
    if (s == "date") {
      s <- step$features[1]
    }
    seld <- c(seld, s)
  }
  preprocessingInputId <- paste0(name, "_Preprocess")
  updateSelectizeInput(session = session, inputId = preprocessingInputId, choices = ppchoices, selected = seld)
  if (length(seld) > 0) {
    showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 5)
  }
  model
}


# model deleter
deleteRds <- function(name) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (file.exists(rdsfile)) {
    ok <- unlink(rdsfile, force = TRUE)
  } else {
    ok <- TRUE
  }
  ok
}


# ── UI HELPER: GENERIC MODEL TAB ─────────────────────────────────────────────
model_tab_panel <- function(label, value, prefix = value) {
  tabPanel(label, value = value,
           br(),
           tabsetPanel(
             type = "tabs",
             tabPanel("Summary",
                      verbatimTextOutput(outputId = paste0(prefix, "_MethodSummary")),
                      h4("Resampled performance", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                      tableOutput(outputId = paste0(prefix, "_Metrics"))
             ),
             tabPanel("Tuning",
                      plotOutput(outputId = paste0(prefix, "_ModelTune"), height = "650px")
             ),
             tabPanel("Recipe",
                      htmlOutput(outputId = paste0(prefix, "_RecipePrint")),
                      tableOutput(outputId = paste0(prefix, "_RecipeOutput"))
             ),
             tabPanel("Model Output",
                      h4("Training summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                      verbatimTextOutput(outputId = paste0(prefix, "_TrainSummary"))
             )
           )
  )
}




# =================================================================================
# Strategy module: outlier response
# =================================================================================
# =================================================================================
# mod_out_response.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

out_response_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
         min-height:100vh; padding-left:20px; padding-right:20px;
         overflow-x:hidden;",
      
      div(
        style = "font-size:13px; color:#343a40; background:white; padding:10px;
                 border-left:4px solid #0d6efd; border-radius:6px; margin-bottom:12px;
                 box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; <b>Outlier Response</b><br><br>
              Statistical methods alone cannot justify treating an observation
              as an outlier. <b>Always consult a domain expert</b> before
              taking any action.")
      ),
      div(
        style = "font-size:12px; color:#842029; background:#f8d7da;
                 border-left:3px solid #f5c2c7; padding:8px 10px;
                 border-radius:4px; margin-bottom:12px;",
        icon("user-doctor", style = "color:#842029;"),
        HTML(" <b>Domain expert confirmation required</b> before modifying
              or omitting any observation.")
      ),
      hr(),
      
      # ── Omit by ID ────────────────────────────────────────────────────
      tags$label("Omit Observations by ID:",
                 style = "font-size:13px; font-weight:600; color:#343a40;
                          display:block; margin-bottom:4px;"),
      div(
        style = "font-size:11px; color:#842029; background:#f8d7da;
                 border-left:3px solid #f5c2c7; padding:6px 8px;
                 border-radius:4px; margin-bottom:8px;",
        icon("triangle-exclamation", style = "color:#842029;"),
        HTML(" Only omit if not expected in future unseen data.")
      ),
      selectInput(ns("id_col"), "ID / label column:", choices = NULL),
      selectizeInput(ns("omit_ids"), label = NULL,
                     choices  = NULL, multiple = TRUE,
                     options  = list(placeholder = "Select IDs to omit...")),
      hr(),
      
      # ── Global Rules ──────────────────────────────────────────────────
      tags$label("Global Rules:",
                 style = "font-size:13px; font-weight:600; color:#343a40;
                          display:block; margin-bottom:6px;"),
      helpText("Bad values here are replaced across ALL columns where they appear.
               Applied before column-specific rules."),
      div(
        style = "font-size:11px; color:#856404; background:#fff3cd;
                 border-left:3px solid #ffc107; padding:5px 8px;
                 border-radius:4px; margin-bottom:8px;",
        icon("triangle-exclamation", style = "color:#ffc107;"),
        HTML(" To set values to <code>NA</code> globally, go to
              <b>Miss Strategy &gt; Variants</b> instead.")
      ),
      tags$div(id = ns("global_rules_container")),
      actionButton(ns("add_global_rule"), "+ Add Global Rule", icon = icon("plus"),
                   width = "100%",
                   style = "margin-top:6px; margin-bottom:6px;"),
      hr(),
      
      # ── Column-Specific Rules ──────────────────────────────────────────
      tags$label("Column-Specific Rules:",
                 style = "font-size:13px; font-weight:600; color:#343a40;
                          display:block; margin-bottom:6px;"),
      helpText("Select a column, enter bad values (comma-sep), then choose
               how to replace them."),
      div(
        style = "font-size:11px; color:#856404; background:#fff3cd;
                 border-left:3px solid #ffc107; padding:5px 8px;
                 border-radius:4px; margin-bottom:8px;",
        icon("triangle-exclamation", style = "color:#ffc107;"),
        HTML(" To set column values to <code>NA</code>, go to
              <b>Miss Strategy &gt; Variants</b> and define a
              column-specific NA rule there instead.")
      ),
      tags$div(id = ns("col_rules_container")),
      actionButton(ns("add_rule"), "+ Add Column Rule", icon = icon("plus"),
                   width = "100%",
                   style = "margin-top:6px; margin-bottom:6px;"),
      hr(),
      
      actionButton(ns("reset"), "Reset All", icon = icon("rotate-left"),
                   width = "100%")
    ),
    
    mainPanel(
      width = 9,
      style = "overflow-x: auto;",
      uiOutput(ns("main_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

out_response_server <- function(id, get_data, get_raw, roles) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    default_omit_ids <- potential_outliers
    
    # ── state ──────────────────────────────────────────────────────────────
    n_global_rules <- reactiveVal(0)
    n_rules        <- reactiveVal(0)
    
    # ── helper: build one global rule block ────────────────────────────────
    make_global_rule_ui <- function(i) {
      tags$div(
        id    = paste0("grule_block_", i),
        style = "background:#fff8e1; border-radius:6px; padding:10px;
                 margin-bottom:10px; border:1px solid #ffe082;",
        tags$label(paste("Global Rule", i),
                   style = "font-size:11px; font-weight:600;
                            color:#6c757d; margin-bottom:6px; display:block;"),
        textInput(ns(paste0("grule_bad_", i)),
                  "Bad values (comma-sep):",
                  placeholder = "e.g. -99, -999, 9999", width = "100%"),
        textInput(ns(paste0("grule_newval_", i)),
                  "Replace with (domain-correct value):",
                  placeholder = "e.g. 0", width = "100%")
      )
    }
    
    # ── helper: build one column rule block ────────────────────────────────
    make_col_rule_ui <- function(i, cols) {
      method_id <- ns(paste0("rule_method_", i))
      tags$div(
        id    = paste0("rule_block_", i),
        style = "background:#f8f9fa; border-radius:6px; padding:10px;
                 margin-bottom:10px; border:1px solid #dee2e6;",
        tags$label(paste("Rule", i),
                   style = "font-size:11px; font-weight:600;
                            color:#6c757d; margin-bottom:6px; display:block;"),
        selectInput(ns(paste0("rule_col_", i)), "Column:",
                    choices = cols, width = "100%"),
        textInput(ns(paste0("rule_bad_", i)),
                  "Bad values (comma-sep):",
                  placeholder = "e.g. -99, 14", width = "100%"),
        selectInput(ns(paste0("rule_method_", i)), "Replace with:",
                    choices  = c("Value"     = "value",
                                 "Winsorise" = "winsorise"),
                    selected = "value", width = "100%"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'value'", method_id),
          textInput(ns(paste0("rule_newval_", i)),
                    "New value:", placeholder = "domain-correct value",
                    width = "100%")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'winsorise'", method_id),
          selectInput(ns(paste0("rule_winmethod_", i)), "Cap by:",
                      choices  = c("IQR whisker"  = "iqr",
                                   "Percentile"   = "percentile",
                                   "Manual value" = "manual"),
                      width = "100%"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'iqr'",
                                ns(paste0("rule_winmethod_", i))),
            numericInput(ns(paste0("rule_iqrk_", i)), "IQR k:",
                         value = 1.5, min = 0.1, step = 0.5, width = "100%")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'percentile'",
                                ns(paste0("rule_winmethod_", i))),
            sliderInput(ns(paste0("rule_pct_", i)), "Tail %:",
                        min = 1, max = 20, value = 5, step = 1, width = "100%")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'",
                                ns(paste0("rule_winmethod_", i))),
            textInput(ns(paste0("rule_manval_", i)), "Cap value:",
                      placeholder = "e.g. 1.0", width = "100%")
          )
        )
      )
    }
    
    # ── add / reset observers ───────────────────────────────────────────────
    observeEvent(input$add_global_rule, {
      i <- n_global_rules() + 1
      n_global_rules(i)
      insertUI(
        selector = paste0("#", ns("global_rules_container")),
        where    = "beforeEnd",
        ui       = make_global_rule_ui(i)
      )
    })
    
    observeEvent(input$add_rule, {
      req(get_data())
      i    <- n_rules() + 1
      cols <- c("(select)" = "", names(get_data()))
      n_rules(i)
      insertUI(
        selector = paste0("#", ns("col_rules_container")),
        where    = "beforeEnd",
        ui       = make_col_rule_ui(i, cols)
      )
    })
    
    observeEvent(input$reset, {
      for (i in seq_len(n_global_rules()))
        removeUI(selector = paste0("#grule_block_", i), immediate = TRUE)
      for (i in seq_len(n_rules()))
        removeUI(selector = paste0("#rule_block_",  i), immediate = TRUE)
      n_global_rules(0)
      n_rules(0)
      updateSelectizeInput(session, "omit_ids", selected = character(0))
    })
    
    # ── seed one rule of each type on load ──────────────────────────────────
    observe({
      req(get_data())
      if (n_global_rules() == 0) {
        n_global_rules(1)
        insertUI(
          selector = paste0("#", ns("global_rules_container")),
          where    = "beforeEnd",
          ui       = make_global_rule_ui(1)
        )
      }
      if (n_rules() == 0) {
        cols <- c("(select)" = "", names(get_data()))
        n_rules(1)
        insertUI(
          selector = paste0("#", ns("col_rules_container")),
          where    = "beforeEnd",
          ui       = make_col_rule_ui(1, cols)
        )
      }
    }) |> bindEvent(get_data(), once = TRUE)
    
    # ── populate selectors ──────────────────────────────────────────────────
    observe({
      df     <- get_data(); req(df)
      r      <- roles()
      id_col <- names(r)[r == "obs_id"]
      updateSelectInput(session, "id_col",
                        choices  = c("Patient" = ".rownames", "Row index" = ".rowindex", names(df)),
                        selected = if (length(id_col) > 0) id_col[1] else ".rownames")
    })
    
    observe({
      df     <- get_data(); req(df)
      id_col <- input$id_col
      req(!is.null(id_col))
      ids <- if (id_col == ".rownames") {
        rownames(df)
      } else if (id_col == ".rowindex") {
        as.character(seq_len(nrow(df)))
      } else {
        req(id_col %in% names(df))
        as.character(unique(df[[id_col]]))
      }
      updateSelectizeInput(session, "omit_ids", choices = ids, selected = intersect(default_omit_ids, ids),
                           server = TRUE)
    })
    
    # ── collect global rules ────────────────────────────────────────────────
    r_global_rules <- reactive({
      rules <- list()
      for (i in seq_len(n_global_rules())) {
        bad_str <- trimws(input[[paste0("grule_bad_",    i)]] %||% "")
        new_val <- trimws(input[[paste0("grule_newval_", i)]] %||% "")
        if (nchar(bad_str) == 0) next
        bad_vals <- trimws(strsplit(bad_str, ",")[[1]])
        rules[[length(rules) + 1]] <- list(
          bad_vals = bad_vals,
          new_val  = new_val
        )
      }
      rules
    })
    
    # ── collect column rules ────────────────────────────────────────────────
    r_col_rules <- reactive({
      rules <- list()
      for (i in seq_len(n_rules())) {
        col      <- input[[paste0("rule_col_",    i)]] %||% ""
        bad_str  <- trimws(input[[paste0("rule_bad_",    i)]] %||% "")
        method   <- input[[paste0("rule_method_", i)]] %||% "value"
        if (col == "" || col == "(select)") next
        if (nchar(bad_str) == 0) next
        bad_vals <- trimws(strsplit(bad_str, ",")[[1]])
        rules[[length(rules) + 1]] <- list(
          col        = col,
          bad_vals   = bad_vals,
          method     = method,
          new_val    = trimws(input[[paste0("rule_newval_",   i)]] %||% ""),
          win_method = input[[paste0("rule_winmethod_", i)]] %||% "iqr",
          iqr_k      = as.numeric(input[[paste0("rule_iqrk_",    i)]] %||% 1.5),
          pct        = as.numeric(input[[paste0("rule_pct_",     i)]] %||% 5),
          manual_val = trimws(input[[paste0("rule_manval_",  i)]] %||% "")
        )
      }
      rules
    })
    
    # ── processed data ──────────────────────────────────────────────────────
    processed <- reactive({
      df      <- get_data(); req(df)
      g_rules <- r_global_rules()
      rules   <- r_col_rules()
      id_col  <- input$id_col
      omit    <- input$omit_ids %||% character(0)
      
      # 1. omit rows
      if (length(omit) > 0) {
        if (id_col == ".rownames") {
          df <- df[!rownames(df) %in% omit, , drop = FALSE]
        } else if (id_col == ".rowindex") {
          omit_idx <- suppressWarnings(as.integer(omit))
          omit_idx <- omit_idx[!is.na(omit_idx) & omit_idx <= nrow(df)]
          if (length(omit_idx) > 0)
            df <- df[-omit_idx, , drop = FALSE]
        } else if (id_col %in% names(df)) {
          df <- df[!as.character(df[[id_col]]) %in% omit, , drop = FALSE]
        }
      }
      
      # 2. global rules — replace bad values across ALL columns
      for (rule in g_rules) {
        if (nchar(rule$new_val) == 0) next
        bad_num <- suppressWarnings(as.numeric(rule$bad_vals))
        bad_num <- bad_num[!is.na(bad_num)]
        for (col in names(df)) {
          x <- df[[col]]
          target <- if (is.numeric(x) && length(bad_num) > 0)
            x %in% bad_num
          else
            as.character(x) %in% rule$bad_vals
          if (!any(target, na.rm = TRUE)) next
          typed <- tryCatch(
            if (is.numeric(x)) as.numeric(rule$new_val) else rule$new_val,
            error = function(e) rule$new_val)
          df[[col]][target] <- typed
        }
      }
      
      # 3. column-specific rules
      for (rule in rules) {
        col <- rule$col
        if (!col %in% names(df)) next
        x <- df[[col]]
        
        target <- if (is.numeric(x)) {
          bad_num <- suppressWarnings(as.numeric(rule$bad_vals))
          x %in% bad_num[!is.na(bad_num)]
        } else {
          as.character(x) %in% rule$bad_vals
        }
        if (!any(target, na.rm = TRUE)) next
        
        if (rule$method == "value") {
          if (nchar(rule$new_val) == 0) next
          typed <- tryCatch(
            if (is.numeric(x)) as.numeric(rule$new_val) else rule$new_val,
            error = function(e) rule$new_val)
          df[[col]][target] <- typed
          
        } else if (rule$method == "winsorise") {
          x_ref <- x[!target & !is.na(x)]
          cap <- switch(rule$win_method,
                        "iqr" = {
                          lims <- boxplot.stats(x_ref, coef = rule$iqr_k)$stats
                          list(lo = lims[1], hi = lims[5])
                        },
                        "percentile" = {
                          p <- rule$pct / 100
                          list(lo = quantile(x_ref, p,     na.rm = TRUE),
                               hi = quantile(x_ref, 1 - p, na.rm = TRUE))
                        },
                        "manual" = {
                          v <- suppressWarnings(as.numeric(rule$manual_val))
                          list(lo = -Inf, hi = if (is.na(v)) max(x_ref, na.rm = TRUE) else v)
                        }
          )
          df[[col]][target] <- pmin(pmax(x[target], cap$lo), cap$hi)
        }
      }
      
      df
    })
    
    # ── change log ──────────────────────────────────────────────────────────
    change_log <- reactive({
      df_orig <- get_data(); req(df_orig)
      df_new  <- processed()
      shared  <- intersect(rownames(df_orig), rownames(df_new))
      rows <- list()
      for (col in names(df_orig)) {
        if (!col %in% names(df_new)) next
        orig <- df_orig[shared, col]
        new  <- df_new[shared,  col]
        chg  <- which(
          (is.na(orig) != is.na(new)) |
            (!is.na(orig) & !is.na(new) &
               as.character(orig) != as.character(new))
        )
        if (length(chg) == 0) next
        rows[[length(rows) + 1]] <- data.frame(
          Row    = shared[chg],
          Column = col,
          Before = as.character(orig[chg]),
          After  = as.character(new[chg]),
          stringsAsFactors = FALSE
        )
      }
      if (length(rows) == 0) return(data.frame())
      log_df <- do.call(rbind, rows)
      log_df[order(log_df$Row, log_df$Column), ]
    })
    
    # ── main UI ─────────────────────────────────────────────────────────────
    output$main_ui <- renderUI({
      req(get_data())
      log_df   <- change_log()
      n_change <- nrow(log_df)
      n_omit   <- length(input$omit_ids %||% character(0))
      
      card <- function(label, value, color) {
        tags$div(
          style = paste0("flex:1; min-width:130px; background:white;
                          border-radius:10px; border:0.5px solid #dee2e6;
                          padding:14px 18px; box-shadow:0 1px 3px rgba(0,0,0,0.06);"),
          tags$div(style = "font-size:11px; color:#6c757d; font-weight:500;
                            text-transform:uppercase; letter-spacing:.5px;", label),
          tags$div(style = paste0("font-size:28px; font-weight:700; color:", color, ";"), value)
        )
      }
      
      tagList(
        tags$div(
          style = "display:flex; gap:12px; margin-bottom:20px; flex-wrap:wrap;",
          card("Cells Modified", n_change,
               if (n_change > 0) "#185FA5" else "#6c757d"),
          card("Rows Affected",
               if (n_change > 0) length(unique(log_df$Row)) else 0,
               if (n_change > 0) "#0F6E56" else "#6c757d"),
          card("Cols Affected",
               if (n_change > 0) length(unique(log_df$Column)) else 0,
               if (n_change > 0) "#534AB7" else "#6c757d"),
          card("Rows Omitted", n_omit,
               if (n_omit > 0) "#C41E3A" else "#6c757d")
        ),
        
        tags$div(
          style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                   padding:16px; margin-bottom:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
          tags$h5("Change Log",
                  style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
          if (n_change == 0)
            tags$p("No values modified yet.", style = "color:#adb5bd; font-size:13px;")
          else
            DT::dataTableOutput(ns("change_log_tbl"))
        ),
        
        if (n_change > 0) tags$div(
          style = "display:grid; grid-template-columns:1fr; gap:16px;",
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#6c757d; margin-right:6px;"),
                    "Before",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            DT::dataTableOutput(ns("before_tbl"))
          ),
          tags$div(
            style = "background:white; border-radius:10px; border:0.5px solid #dee2e6;
                     padding:16px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
            tags$h5(icon("table", style = "color:#185FA5; margin-right:6px;"),
                    "After",
                    style = "font-weight:600; color:#343a40; margin-bottom:12px;"),
            DT::dataTableOutput(ns("after_tbl"))
          )
        )
      )
    })
    
    # ── tables ──────────────────────────────────────────────────────────────
    dt_opts <- list(pageLength = 10, scrollX = TRUE, dom = "tip")
    
    output$change_log_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      DT::datatable(change_log(), options = dt_opts, rownames = FALSE) |>
        DT::formatStyle("After", color = "#185FA5", fontWeight = "bold")
    })
    
    output$before_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      rows <- unique(change_log()$Row)
      raw  <- tryCatch(get_raw(), error = function(e) get_data())
      show <- intersect(rows, rownames(raw))
      DT::datatable(raw[show, , drop = FALSE], options = dt_opts, rownames = TRUE)
    })
    
    output$after_tbl <- DT::renderDataTable({
      req(nrow(change_log()) > 0)
      rows <- unique(change_log()$Row)
      proc <- processed()
      show <- intersect(rows, rownames(proc))
      DT::datatable(proc[show, , drop = FALSE], options = dt_opts, rownames = TRUE)
    })
    
    # ── return ──────────────────────────────────────────────────────────────
    return(list(data = processed))
  })
}


# =================================================================================
# ui.R
# =================================================================================

# shinyUI(fluidPage(
ui <- fluidPage(
  
  # Application title
  fluidRow(
    column(width = 8, tags$h2("Assignment 3 - William Hui Chang (69051925)", style = "margin-top:0;")),
    column(width = 4,
           div(style = "display:flex;justify-content:flex-end;align-items:flex-start;padding-top:4px;",
               div(style = "display:flex; gap:10px; width:380px;",
                   numericInput(inputId = "SplitSeed", label = "Split seed", value = 199, min = 1, width = "100%"),
                   numericInput(inputId = "TrainSeed", label = "Train/resampling seed", value = 673, min = 1, width = "100%")
               )
           )
    )
  ),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             tagList(
               tags$style(HTML("
      .shiny-input-container label,
      .radio label, .checkbox label {
        font-weight: 400 !important;
        font-size: 13px;
      }
    ")),
               tabsetPanel(
                 id   = "split_active_tab",
                 type = "pills",
                 
                 tabPanel("2-partition (train + test)",
                          br(),
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              width = 3,
                              style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                              div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                                  icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                                  HTML("<strong>Tab note:</strong><br>Compares simple random vs stratified sampling. Stratification preserves outcome distribution.")),
                              hr(),
                              tags$label("Train proportion", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p1_train", label=NULL, min=0.5, max=0.95, value=0.8, step=0.05, width="100%"),
                              hr(),
                              tags$label("Stratify by (y =)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p1_y", label=NULL, choices=NULL),
                              hr(),
                              span("Independent observations", style="font-size:11px;padding:2px 8px;border-radius:99px;display:inline-block;margin-bottom:6px;background:#EEEDFE;color:#3C3489;"),
                              div(style="font-size:11px;color:#6c757d;margin-top:4px;", "Uses ", code("base::sample()"), " and ", code("caret::createDataPartition()")),
                              hr(),
                              actionButton("p1_apply", "Apply this split", icon=icon("check"), width="100%",
                                           style="background:#534AB7;color:white;border:none;font-size:13px;")
                            ),
                            mainPanel(
                              width = 9,
                              uiOutput("p1_applied"),
                              h4("Full data — outcome frequency table", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p1_full_table"),
                              hr(),
                              h4("Simple random sampling — train frequency table", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p1_simple_table"),
                              hr(),
                              h4("Stratified random sampling — train frequency table", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p1_strat_table"),
                              hr(),
                              h4("Partition sizes", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              tableOutput("p1_sizes")
                            )
                          )
                 ),
                 
                 tabPanel("3-partition (train + val + test)",
                          br(),
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              width = 3,
                              style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                              div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                                  icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                                  HTML("<strong>Tab note:</strong><br>Two-stage stratified split: train, then val + test from remainder.")),
                              hr(),
                              tags$label("Train proportion", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p2_train", label=NULL, min=0.4, max=0.8, value=0.6, step=0.05, width="100%"),
                              tags$label("Validation proportion (of remainder)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p2_val", label=NULL, min=0.1, max=0.9, value=0.5, step=0.05, width="100%"),
                              hr(),
                              tags$label("Stratify by (y =)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p2_y", label=NULL, choices=NULL),
                              hr(),
                              span("Independent observations", style="font-size:11px;padding:2px 8px;border-radius:99px;display:inline-block;margin-bottom:6px;background:#EEEDFE;color:#3C3489;"),
                              div(style="font-size:11px;color:#6c757d;margin-top:4px;", "Uses ", code("caret::createDataPartition()"), " twice"),
                              hr(),
                              actionButton("p2_apply", "Apply this split", icon=icon("check"), width="100%",
                                           style="background:#534AB7;color:white;border:none;font-size:13px;")
                            ),
                            mainPanel(
                              width = 9,
                              uiOutput("p2_applied"),
                              h4("Partition observation counts", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p2_counts"),
                              hr(),
                              h4("Partition sizes summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              tableOutput("p2_sizes")
                            )
                          )
                 ),
                 
                 tabPanel("Stratified bootstrap",
                          br(),
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              width = 3,
                              style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                              div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                                  icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                                  HTML("<strong>Tab note:</strong><br>Bootstrap draws with replacement. Shows str() and calcPerc() % unique obs.")),
                              hr(),
                              tags$label("Number of resamples", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p3_times", label=NULL, min=5, max=100, value=25, step=5, width="100%"),
                              hr(),
                              tags$label("Stratify by (y =)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p3_y", label=NULL, choices=NULL),
                              hr(),
                              span("Independent observations", style="font-size:11px;padding:2px 8px;border-radius:99px;display:inline-block;margin-bottom:6px;background:#EEEDFE;color:#3C3489;"),
                              div(style="font-size:11px;color:#6c757d;margin-top:4px;", "Uses ", code("caret::createResample()")),
                              hr(),
                              actionButton("p3_apply", "Apply this split", icon=icon("check"), width="100%",
                                           style="background:#534AB7;color:white;border:none;font-size:13px;")
                            ),
                            mainPanel(
                              width = 9,
                              uiOutput("p3_applied"),
                              h4("str(resamples) — resample structure", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p3_str"),
                              hr(),
                              h4("calcPerc() — % unique observations per resample", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p3_perc")
                            )
                          )
                 ),
                 
                 tabPanel("Leave group out",
                          br(),
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              width = 3,
                              style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                              div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                                  icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                                  HTML("<strong>Tab note:</strong><br>Ensures no group appears in both train and test. Shows fold structure and Fold 1 group membership.")),
                              hr(),
                              tags$label("Group variable", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p4_group", label=NULL, choices=NULL),
                              hr(),
                              tags$label("Number of folds (k)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p4_k", label=NULL, min=2, max=3, value=2, step=1, width="100%"),
                              hr(),
                              span("Group dependent observations", style="font-size:11px;padding:2px 8px;border-radius:99px;display:inline-block;margin-bottom:6px;background:#E1F5EE;color:#085041;"),
                              div(style="font-size:11px;color:#6c757d;margin-top:4px;", "Uses ", code("caret::groupKFold()")),
                              hr(),
                              actionButton("p4_apply", "Apply this split", icon=icon("check"), width="100%",
                                           style="background:#0F6E56;color:white;border:none;font-size:13px;")
                            ),
                            mainPanel(
                              width = 9,
                              uiOutput("p4_applied"),
                              h4("Group frequency table", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p4_freq_table"),
                              hr(),
                              h4("Fold structure — folds list", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p4_folds_print"),
                              hr(),
                              h4("Fold 1 — groups in train (inGroups)", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p4_in_groups"),
                              verbatimTextOutput("p4_in_distinct"),
                              hr(),
                              h4("Fold 1 — groups held out (outGroups)", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p4_out_groups"),
                              verbatimTextOutput("p4_out_distinct")
                            )
                          )
                 ),
                 
                 tabPanel("Time series",
                          br(),
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              width = 3,
                              style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                              div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                                  icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                                  HTML("<strong>Tab note:</strong><br>Train always precedes test in time. Shows str() of train and test slice lists.")),
                              hr(),
                              tags$label("Time variable (sort order)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p5_time", label=NULL, choices=NULL),
                              hr(),
                              tags$label("Initial window", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p5_init", label=NULL, min=2, max=50, value=5, step=1, width="100%"),
                              tags$label("Horizon", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p5_horizon", label=NULL, min=1, max=20, value=3, step=1, width="100%"),
                              hr(),
                              tags$label("Fixed window", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p5_fixed", label=NULL,
                                          choices=c("FALSE (expanding)"="FALSE", "TRUE (fixed)"="TRUE")),
                              hr(),
                              tags$label("Skip (slices between each window)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p5_skip", label=NULL, min=0, max=10, value=0, step=1, width="100%"),
                              hr(),
                              span("Time dependent observations", style="font-size:11px;padding:2px 8px;border-radius:99px;display:inline-block;margin-bottom:6px;background:#FAEEDA;color:#633806;"),
                              div(style="font-size:11px;color:#6c757d;margin-top:4px;", "Uses ", code("caret::createTimeSlices()")),
                              hr(),
                              actionButton("p5_apply", "Apply this split", icon=icon("check"), width="100%",
                                           style="background:#BA7517;color:white;border:none;font-size:13px;")
                            ),
                            mainPanel(
                              width = 9,
                              uiOutput("p5_applied"),
                              h4("str(tsSamples$train) — train slice structure", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p5_train_str"),
                              hr(),
                              h4("str(tsSamples$test) — test slice structure", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              verbatimTextOutput("p5_test_str")
                            )
                          )
                 ),
                 
                 tabPanel("Diversity down-sampling",
                          br(),
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              width = 3,
                              style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                              div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                                  icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                                  HTML("<strong>Tab note:</strong><br>K-medoids selects one medoid per cluster as training sample, maximising feature-space coverage.")),
                              hr(),
                              tags$label("Number of clusters (k)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              sliderInput("p6_k", label=NULL, min=10, max=300, value=80, step=10, width="100%"),
                              hr(),
                              tags$label("Clustering features", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectizeInput("p6_features", label=NULL, choices=NULL, multiple=TRUE,
                                             options=list(placeholder="All numeric by default")),
                              hr(),
                              tags$label("X axis variable (plot only)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p6_x", label=NULL, choices=NULL),
                              tags$label("Y axis variable (plot only)", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p6_y", label=NULL, choices=NULL),
                              hr(),
                              tags$label("Distance metric", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p6_metric", label=NULL, choices=c("euclidean","manhattan")),
                              tags$label("Standardise features", style="font-weight:600;font-size:13px;color:#343a40;"),
                              selectInput("p6_stand", label=NULL, choices=c("TRUE","FALSE")),
                              hr(),
                              span("Diversity down-sampling", style="font-size:11px;padding:2px 8px;border-radius:99px;display:inline-block;margin-bottom:6px;background:#FAECE7;color:#712B13;"),
                              div(style="font-size:11px;color:#6c757d;margin-top:4px;", "Uses ", code("cluster::pam()")),
                              hr(),
                              actionButton("p6_apply", "Apply this split", icon=icon("check"), width="100%",
                                           style="background:#993C1D;color:white;border:none;font-size:13px;")
                            ),
                            mainPanel(
                              width = 9,
                              uiOutput("p6_applied"),
                              h4("Sampled vs not-sampled — scatter plot", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              plotOutput("p6_plot", height="450px"),
                              hr(),
                              h4("Down-sample summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              tableOutput("p6_table")
                            )
                          )
                 )
               )
             )
    ),
    
    tabPanel("Strategy",
             tagList(
               tags$style(HTML("
      .shiny-input-container label,
      .radio label, .checkbox label {
        font-weight: 400 !important;
        font-size: 13px;
      }
    ")),
               tabsetPanel(
                 id = "strategy_active_tab",
                 type = "pills",
                 tabPanel("Outlier response",
                          br(),
                          out_response_ui("out_response")
                 )
               )
             )
    ),
        tabPanel("Available methods",
             tagList(
               tags$style(HTML("
      .shiny-input-container label,
      .radio label, .checkbox label {
        font-weight: 400 !important;
        font-size: 13px;
      }
    ")),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                   
                   # Info box
                   div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                       icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                       HTML("<strong>How to use:</strong><br>
              Filter methods using the controls below.
              The table updates instantly. Switch to the map
              tab to see coloured clusters.")),
                   
                   # ── 1st: Model Constraints ────────────────────────────────────────────────
                   div(style="background:#f8d7da;border-left:3px solid #dc3545;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("1st — Model Constraints",
                                  style="font-weight:700;font-size:13px;color:#842029;")),
                   div(style="font-size:11px;color:#6c757d;margin-bottom:6px;",
                       "Determines which model points appear on the map"),
                   
                   tags$label("Method type", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   radioButtons("av_type", label=NULL,
                                choices=c("Regression only"="reg",
                                          "Classification only"="cls",
                                          "Both"="both"),
                                selected="reg"),
                   
                   tags$label("Exclude tags (ANY disqualifies)", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   selectizeInput("av_flt_exclude", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Two Class Only")),
                   hr(),
                   
                   # ── 2nd: Literature-Informed Highlights ──────────────────────────────────
                   div(style="background:#fff3cd;border-left:3px solid #ffc107;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("2nd — Literature-Informed Highlights",
                                  style="font-weight:700;font-size:13px;color:#664d03;")),
                   div(style="font-size:11px;color:#6c757d;margin-bottom:6px;",
                       "Or logic for bold highlighting based on domain research"),
                   selectizeInput("av_flt_any", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Regularization")
                                  ),
                   hr(),
                   
                   # ── 3rd: Sample Model Flavours ────────────────────────────────────────────
                   div(style="background:#d1ecf1;border-left:3px solid #0dcaf0;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("3rd — Sample Model Flavours",
                                  style="font-weight:700;font-size:13px;color:#055160;")),
                   div(style="font-size:11px;color:#6c757d;margin-bottom:6px;",
                       "Colours model points on the map by group"),
                   
                   # Group 1 — Purple
                   div(style="border-left:3px solid #534AB7;padding-left:8px;margin-bottom:2px;margin-top:10px;",
                       tags$label("Group 1", style="font-weight:600;font-size:13px;color:#534AB7;")),
                   selectizeInput("av_g1", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Neural Network")),
                   
                   # Group 2 — Teal
                   div(style="border-left:3px solid #0F6E56;padding-left:8px;margin-bottom:2px;margin-top:10px;",
                       tags$label("Group 2", style="font-weight:600;font-size:13px;color:#0F6E56;")),
                   selectizeInput("av_g2", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Linear Regression")),
                   
                   # Group 3 — Amber
                   div(style="border-left:3px solid #BA7517;padding-left:8px;margin-bottom:2px;margin-top:10px;",
                       tags$label("Group 3", style="font-weight:600;font-size:13px;color:#BA7517;")),
                   selectizeInput("av_g3", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Tree-Based Model")),
                   
                   # Group 4 — Coral
                   div(style="border-left:3px solid #993C1D;padding-left:8px;margin-bottom:2px;margin-top:10px;",
                       tags$label("Group 4", style="font-weight:600;font-size:13px;color:#993C1D;")),
                   selectizeInput("av_g4", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Kernel Method")),
                   
                   # Group 5 — Blue
                   div(style="border-left:3px solid #1a6ebd;padding-left:8px;margin-bottom:2px;margin-top:10px;",
                       tags$label("Group 5", style="font-weight:600;font-size:13px;color:#1a6ebd;")),
                   selectizeInput("av_g5", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Ensemble Model")),
                   
                   # Group 6 — Pink (wildcard)
                   div(style="border-left:3px solid #d63384;padding-left:8px;margin-bottom:2px;margin-top:10px;",
                       tags$label("Group 6 — Wildcard", style="font-weight:600;font-size:13px;color:#d63384;")),
                   selectizeInput("av_g6", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. anything...")),
                   hr(),
                   
                   # ── Map Configs ───────────────────────────────────────────────────────────
                   div(style="background:#e2e3e5;border-left:3px solid #6c757d;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("Map Configs",
                                  style="font-weight:700;font-size:13px;color:#41464b;")),
                   tags$label("Map distance metric", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   selectInput("av_map_dist", label=NULL,
                               choices=c("euclidean","manhattan","binary","canberra"),
                               selected="manhattan"),
                   tags$label("Label size", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   sliderInput("av_map_label_size", label=NULL, min=1, max=5, value=5, step=0.5, width="100%"),
                   tags$label("Max label overlaps", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   sliderInput("av_map_overlaps", label=NULL, min=10, max=100, value=50, step=5, width="100%"),
                   hr(),
                   
                   # ── 6. Matching summary ───────────────────────────────────────────
                   tags$label("Matching methods", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   verbatimTextOutput("av_filter_summary")
                 ),
                 
                 mainPanel(
                   width = 9,
                   tabsetPanel(
                     type = "pills",
                     id   = "av_subtabs",
                     
                     tabPanel("Method table",
                              br(),
                              h4("Filtered caret methods",
                                 style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              shinycssloaders::withSpinner(DT::dataTableOutput("av_method_table"))
                     ),
                     
                     tabPanel("Method map",
                              br(),
                              h4("Similarity map — colour groups highlighted",
                                 style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              shinycssloaders::withSpinner(plotOutput("av_map_plot", height="80vh"))
                     )
                   )
                 )
               )
             )
    ),
    
    tabPanel("Methods",
             tagList(
               tags$style(HTML("
      .shiny-input-container label,
      .radio label, .checkbox label {
        font-weight: 400 !important;
        font-size: 13px;
      }
    ")),
               sidebarLayout(
                 position = "right",
                 
                 sidebarPanel(
                   width = 3,
                   style = "background-color:#f4f6fb; border-left:3px solid #6a9fd8; min-height:100vh; padding:16px 14px;",
                   
                   div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                       icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                       HTML("<strong>Training controls:</strong><br>Choose preprocessing, tune settings, then train or load the selected model.")),
                   
                   div(style="background:#e2e3e5;border-left:3px solid #6c757d;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("General Configs", style="font-weight:700;font-size:13px;color:#41464b;")),
                   
                   checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
                   bsTooltip(id = "Parallel", title = paste("This will utilise all", detectCores(), "available CPUs during training")),
                   
                   selectInput("cfg_resampling", label = "Resampling method",
                               choices = c("Bootstrap" = "boot",
                                           "Cross-validation" = "cv",
                                           "Repeated cross-validation" = "repeatedcv"),
                               selected = "boot"),
                   conditionalPanel(
                     condition = "input.cfg_resampling == 'boot'",
                     sliderInput("cfg_boot_n", label = "Bootstrap resamples", min = 5, max = 50, value = 25, step = 5, width = "100%")
                   ),
                   conditionalPanel(
                     condition = "input.cfg_resampling == 'cv'",
                     sliderInput("cfg_cv_folds", label = "CV folds", min = 3, max = 25, value = 10, step = 1, width = "100%")
                   ),
                   conditionalPanel(
                     condition = "input.cfg_resampling == 'repeatedcv'",
                     sliderInput("cfg_repeatedcv_folds", label = "Folds per CV run", min = 3, max = 10, value = 4, step = 1, width = "100%"),
                     sliderInput("cfg_repeatedcv_repeats", label = "Number of CV runs", min = 1, max = 10, value = 6, step = 1, width = "100%")
                   ),
                   selectInput("cfg_search", label = "Search type",
                               choices = c("Grid search" = "grid", "Random search" = "random"),
                               selected = "grid"),
                   
                   hr(),
                   
                   div(style="background:#fff3cd;border-left:3px solid #ffc107;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("Config Mode", style="font-weight:700;font-size:13px;color:#664d03;")),
                   
                   radioButtons("method_config_mode", label = NULL,
                                choices = c("General configs" = "general",
                                            "Model specific configs" = "specific"),
                                selected = "general"),
                   
                   hr(),
                   
                   div(style="background:#d1ecf1;border-left:3px solid #0dcaf0;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("Preprocessing", style="font-weight:700;font-size:13px;color:#055160;")),
                   
                   uiOutput("method_preprocess_ui"),
                   uiOutput("preprocess_config_ui"),
                   
                   hr(),
                   
                   div(style="background:#f8d7da;border-left:3px solid #dc3545;padding:6px 10px;border-radius:4px;margin-bottom:6px;",
                       tags$label("Model Specific Configs", style="font-weight:700;font-size:13px;color:#842029;")),
                   
                   uiOutput("model_specific_config_ui"),
                   
                   hr(),
                   
                   uiOutput("method_action_buttons")
                 ),
                 
                 mainPanel(
                   width = 9,
                   
                   tabsetPanel(
                     id = "active_method",
                     type = "pills",
                     
                     tabPanel("NULL", value = "null",
                              br(),
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Summary",
                                         h4("Resampled performance", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         tableOutput(outputId = "null_Metrics")
                                ),
                                tabPanel("Tuning",
                                         h4("Hyperparameter tuning", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         verbatimTextOutput("null_TuningNote")
                                ),
                                tabPanel("Recipe",
                                         h4("Recipe output", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         tableOutput(outputId = "null_Recipe")
                                ),
                                tabPanel("Model Output",
                                         h4("Training summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         verbatimTextOutput(outputId = "null_TrainSummary")
                                )
                              )
                     ),
                     
                     tabPanel("GLMnet", value = "glmnet",
                              br(),
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Summary",
                                         verbatimTextOutput(outputId = "glmnet_MethodSummary"),
                                         h4("Resampled performance", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         tableOutput(outputId = "glmnet_Metrics")
                                ),
                                tabPanel("Tuning",
                                         plotOutput(outputId = "glmnet_ModelTune", height = "650px")
                                ),
                                tabPanel("Recipe",
                                         htmlOutput(outputId = "glmnet_RecipePrint"),
                                         tableOutput(outputId = "glmnet_RecipeOutput")
                                ),
                                tabPanel("Model Output",
                                         fluidRow(
                                           column(width = 6,
                                                  h4("Training summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                                  verbatimTextOutput(outputId = "glmnet_TrainSummary")
                                           ),
                                           column(width = 6,
                                                  h4("Coefficients", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                                  wellPanel(tableOutput(outputId = "glmnet_Coef"))
                                           )
                                         )
                                )
                              )
                     ),
                     
                     tabPanel("PLS", value = "pls",
                              br(),
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Summary",
                                         verbatimTextOutput(outputId = "pls_MethodSummary"),
                                         h4("Resampled performance", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         tableOutput(outputId = "pls_Metrics")
                                ),
                                tabPanel("Tuning",
                                         plotOutput(outputId = "pls_ModelTune", height = "650px")
                                ),
                                tabPanel("Recipe",
                                         htmlOutput(outputId = "pls_RecipePrint"),
                                         tableOutput(outputId = "pls_RecipeOutput")
                                ),
                                tabPanel("Model Output",
                                         fluidRow(
                                           column(width = 6,
                                                  h4("Training summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                                  verbatimTextOutput(outputId = "pls_TrainSummary")
                                           ),
                                           column(width = 6,
                                                  h4("Coefficients", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                                  wellPanel(tableOutput(outputId = "pls_Coef"))
                                           )
                                         )
                                )
                              )
                     ),
                     model_tab_panel("spls", "spls"),
                     
                     tabPanel("Rpart", value = "rpart",
                              br(),
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Summary",
                                         verbatimTextOutput(outputId = "rpart_MethodSummary"),
                                         h4("Resampled performance", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         tableOutput(outputId = "rpart_Metrics")
                                ),
                                tabPanel("Tuning",
                                         plotOutput(outputId = "rpart_ModelTune", height = "650px")
                                ),
                                tabPanel("Recipe",
                                         htmlOutput(outputId = "rpart_RecipePrint"),
                                         tableOutput(outputId = "rpart_RecipeOutput")
                                ),
                                tabPanel("Model Output",
                                         fluidRow(
                                           column(width = 6,
                                                  h4("Training summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                                  verbatimTextOutput(outputId = "rpart_TrainSummary")
                                           ),
                                           column(width = 6,
                                                  h4("Model tree", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                                  plotOutput(outputId = "rpart_ModelTree")
                                           )
                                         )
                                )
                              )
                     ), 
                     

                     model_tab_panel("lmStepAIC", "lmStepAIC"),
                     model_tab_panel("rlm", "rlm"),
                     model_tab_panel("xgbLinear", "xgbLinear"),
                     
                     tabPanel("BRNN", value = "brnn",
                              br(),
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Summary",
                                         verbatimTextOutput(outputId = "brnn_MethodSummary"),
                                         h4("Resampled performance", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         tableOutput(outputId = "brnn_Metrics")
                                ),
                                tabPanel("Tuning",
                                         plotOutput(outputId = "brnn_ModelTune", height = "650px")
                                ),
                                tabPanel("Recipe",
                                         htmlOutput(outputId = "brnn_RecipePrint"),
                                         tableOutput(outputId = "brnn_RecipeOutput")
                                ),
                                tabPanel("Model Output",
                                         h4("Training summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         verbatimTextOutput(outputId = "brnn_TrainSummary")
                                )
                              )
                     ),
                     
                     tabPanel("avNNet", value = "avNNet",
                              br(),
                              tabsetPanel(
                                type = "tabs",
                                tabPanel("Summary",
                                         verbatimTextOutput(outputId = "avNNet_MethodSummary"),
                                         h4("Resampled performance", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         tableOutput(outputId = "avNNet_Metrics")
                                ),
                                tabPanel("Tuning",
                                         plotOutput(outputId = "avNNet_ModelTune", height = "650px")
                                ),
                                tabPanel("Recipe",
                                         htmlOutput(outputId = "avNNet_RecipePrint"),
                                         tableOutput(outputId = "avNNet_RecipeOutput")
                                ),
                                tabPanel("Model Output",
                                         h4("Training summary", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                                         verbatimTextOutput(outputId = "avNNet_TrainSummary")
                                )
                              )
                     ),
                     model_tab_panel("nnet", "nnet"),
                     model_tab_panel("mlpWeightDecay", "mlpWeightDecay"),
                     model_tab_panel("monmlp", "monmlp"),
                     model_tab_panel("qrnn", "qrnn"),
                     model_tab_panel("cubist", "cubist"),
                     model_tab_panel("M5", "M5"),
                     model_tab_panel("M5Rules", "M5Rules"),
                     model_tab_panel("evtree", "evtree"),
                     model_tab_panel("svmRadial", "svmRadial"),
                     model_tab_panel("svmRadialSigma", "svmRadialSigma"),
                     model_tab_panel("krlsRadial", "krlsRadial"),
                     model_tab_panel("krlsPoly", "krlsPoly"),
                     model_tab_panel("gaussprRadial", "gaussprRadial"),
                     model_tab_panel("kernelpls", "kernelpls"),
                     model_tab_panel("rvmLinear", "rvmLinear"),
                     model_tab_panel("rvmRadial", "rvmRadial"),
                     model_tab_panel("rvmPoly", "rvmPoly"),
                     model_tab_panel("svmLinear", "svmLinear"),
                     model_tab_panel("svmLinear2", "svmLinear2"),
                     model_tab_panel("svmLinear3", "svmLinear3"),
                     model_tab_panel("ranger", "ranger"),
                     model_tab_panel("rf", "rf"),
                     model_tab_panel("RRF", "RRF"),
                     model_tab_panel("RRFglobal", "RRFglobal"),
                     model_tab_panel("extraTrees", "extraTrees"),
                     model_tab_panel("Rborist", "Rborist"),
                     model_tab_panel("gbm", "gbm"),
                     model_tab_panel("xgbTree", "xgbTree"),
                     model_tab_panel("bagEarth", "bagEarth"),
                     model_tab_panel("earth", "earth"),
                     model_tab_panel("ppr", "ppr")
                   )
                 )
               )
             )
    ),
    
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot", height = "760px"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot", width = "600", height="600")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals", height="600")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals", height="600"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
)
# )





# =================================================================================
# server.R
# =================================================================================

# shinyServer(function(input, output, session) {
server <- function(input, output, session) {
    
    # initialisation ----
    models <- reactiveValues()  # this is a collection of the models
    training_times <- reactiveValues()
    
    getSplitSeed <- reactive({
      seed <- input$SplitSeed %||% 199
      as.integer(seed)
    })
    
    getTrainSeed <- reactive({
      seed <- input$TrainSeed %||% 673
      as.integer(seed)
    })
    
    getBestMetricRow <- function(method) {
      mod <- models[[method]]
      req(mod)
      row <- mod$results[which.min(mod$results[, "RMSE"]), , drop = FALSE]
      row$TrainingTimeSeconds <- if (!is.null(training_times[[method]])) {
        round(training_times[[method]], 2)
      } else if (!is.null(mod$trainingTimeSeconds)) {
        round(mod$trainingTimeSeconds, 2)
      } else {
        NA_real_
      }
      row
    }
    
    .preprocess_defaults <- function(method) {
      if (method == "glmnet") {
        list(knn = 2, bag = 4, pca = 25, pls = 25, ica = 25,
             nzv_freq = 95/5, nzv_unique = 10, other = 0.05,
             poly = 2, corr = 0.95, tune = 10)
      } else if (method == "pls") {
        list(knn = 2, bag = 4, pca = 25, pls = 25, ica = 25,
             nzv_freq = 95/5, nzv_unique = 10, other = 0.05,
             poly = 2, corr = 0.90, tune = 25)
      } else if (method == "rpart") {
        list(knn = 2, bag = 4, pca = 10, pls = 10, ica = 10,
             nzv_freq = 95/5, nzv_unique = 10, other = 0.05,
             poly = 2, corr = 0.90, tune = 5)
      } else {
        list(knn = 2, bag = 4, pca = 25, pls = 25, ica = 25,
             nzv_freq = 95/5, nzv_unique = 10, other = 0.05,
             poly = 2, corr = 0.90, tune = 5)
      }
    }
    
    .method_specific_preprocess <- function(method) {
      switch(method,
             "glmnet" = list(input_id = "glmnet_Preprocess", initial = glmnet_initial),
             "pls"    = list(input_id = "pls_Preprocess",    initial = pls_initial),
             "rpart"  = list(input_id = "rpart_Preprocess",  initial = rpart_initial),
             NULL)
    }
    
    .uses_general_config <- reactive({
      method <- input$active_method %||% "null"
      mode   <- input$method_config_mode %||% "general"
      mode == "general" || is.null(.method_specific_preprocess(method))
    })
    
    .config_scope <- reactive({
      if (.uses_general_config()) {
        "general"
      } else {
        input$active_method %||% "glmnet"
      }
    })
    
    .config_id <- function(scope, name) {
      paste0("cfg_", scope, "_", name)
    }
    
    getSelectedPreprocess <- reactive({
      method <- input$active_method %||% "null"
      mode   <- input$method_config_mode %||% "general"
      
      if (method == "null") {
        NULL
      } else if (mode == "general") {
        input$general_Preprocess %||% general_initial
      } else {
        spec <- .method_specific_preprocess(method)
        if (is.null(spec)) {
          input$general_Preprocess %||% general_initial
        } else {
          input[[spec$input_id]] %||% spec$initial
        }
      }
    })    
    
    output$method_preprocess_ui <- renderUI({
      method <- input$active_method %||% "null"
      mode   <- input$method_config_mode %||% "general"
      
      if (method == "null") {
        return(div(style="font-size:12px;color:#6c757d;", "The null model has no preprocessing choices."))
      }
      
      shared_preprocess <- function(prefix_warning = NULL) {
        tagList(
          prefix_warning,
          selectizeInput(inputId = "general_Preprocess",
                         label = "Pre-processing",
                         choices = unique(c(general_initial, ppchoices)),
                         multiple = TRUE,
                         selected = general_initial)
        )
      }
      
      if (mode == "general") {
        return(shared_preprocess())
      }
      
      spec <- .method_specific_preprocess(method)
      if (is.null(spec)) {
        return(shared_preprocess(
          div(style="font-size:12px;color:#856404;background:#fff3cd;border-left:3px solid #ffc107;padding:8px 10px;border-radius:4px;margin-bottom:8px;",
              icon("circle-info"), HTML(" This model does not have model-specific preprocessing yet. It will use the shared general preprocessing."))
        ))
      }
      
      selectizeInput(inputId = spec$input_id,
                     label = "Pre-processing",
                     choices = unique(c(spec$initial, ppchoices)),
                     multiple = TRUE,
                     selected = spec$initial)
    })
        output$preprocess_config_ui <- renderUI({
      method <- input$active_method %||% "null"
      if (method == "null") return(NULL)
      
      selected_steps <- getSelectedPreprocess()
      
      scope <- .config_scope()
      defs <- if (.uses_general_config()) {
        .preprocess_defaults("general")
      } else {
        .preprocess_defaults(method)
      }
      
      
      controls <- list()
      
      if ("impute_knn" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "impute_knn_neighbors"), "KNN neighbours",
                      min = 1, max = 10, value = defs$knn, step = 1, width = "100%")
        ))
      }
      
      if ("impute_bag" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "impute_bag_trees"), "Bagged imputation trees",
                      min = 2, max = 50, value = defs$bag, step = 1, width = "100%")
        ))
      }
      
      if ("pca" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "pca_num_comp"), "PCA components",
                      min = 1, max = 50, value = defs$pca, step = 1, width = "100%")
        ))
      }
      
      if ("pls" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "pls_num_comp"), "PLS components",
                      min = 1, max = 50, value = defs$pls, step = 1, width = "100%")
        ))
      }
      
      if ("ica" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "ica_num_comp"), "ICA components",
                      min = 1, max = 50, value = defs$ica, step = 1, width = "100%")
        ))
      }
      
      if ("nzv" %in% selected_steps) {
        controls <- c(controls, list(
          numericInput(.config_id(scope, "nzv_freq_cut"), "NZV frequency cut", value = defs$nzv_freq, min = 1),
          numericInput(.config_id(scope, "nzv_unique_cut"), "NZV unique cut", value = defs$nzv_unique, min = 1)
        ))
      }
      
      if ("other" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "other_threshold"), "Rare level threshold",
                      min = 0.01, max = 0.20, value = defs$other, step = 0.01, width = "100%")
        ))
      }
      
      if ("poly" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "poly_degree"), "Polynomial degree",
                      min = 2, max = 5, value = defs$poly, step = 1, width = "100%")
        ))
      }
      
      if ("corr" %in% selected_steps) {
        controls <- c(controls, list(
          sliderInput(.config_id(scope, "corr_threshold"), "Correlation threshold",
                      min = 0.50, max = 0.99, value = defs$corr, step = 0.01, width = "100%")
        ))
      }
      
      if (length(controls) == 0) {
        div(style="font-size:12px;color:#6c757d;", "No configurable preprocessing steps selected.")
      } else {
        tagList(controls)
      }
    })
    
    output$model_specific_config_ui <- renderUI({
      method <- input$active_method %||% "null"
      mode   <- input$method_config_mode %||% "general"
      if (method == "null") {
        return(div(style="font-size:12px;color:#6c757d;", "The null model has no tuning parameters."))
      }
      
      scope <- .config_scope()
      defs <- if (.uses_general_config()) {
        .preprocess_defaults("general")
      } else {
        .preprocess_defaults(method)
      }
      
      controls <- list()
      if (method == "glmnet" && mode == "specific") {
        controls <- c(controls, list(
          selectInput("glmnet_penalty_mode", "Penalty type",
                      choices = c("Elastic net (tune alpha + lambda)" = "elasticnet",
                                  "Ridge (alpha = 0)" = "ridge",
                                  "Lasso (alpha = 1)" = "lasso"),
                      selected = "elasticnet", width = "100%"),
          selectInput("glmnet_grid_mode", "Tuning grid",
                      choices = c("Tune length default" = "default",
                                  "Custom alpha/lambda grid" = "custom"),
                      selected = "default", width = "100%"),
          conditionalPanel(
            condition = "input.glmnet_grid_mode == 'custom'",
            sliderInput("glmnet_alpha_min", "Alpha minimum", min = 0, max = 1, value = 0.1, step = 0.05, width = "100%"),
            sliderInput("glmnet_alpha_max", "Alpha maximum", min = 0, max = 1, value = 1.0, step = 0.05, width = "100%"),
            sliderInput("glmnet_alpha_step", "Alpha step", min = 0.05, max = 0.50, value = 0.10, step = 0.05, width = "100%"),
            sliderInput("glmnet_lambda_min", "Log10 lambda minimum", min = -6, max = 2, value = -3, step = 1, width = "100%"),
            sliderInput("glmnet_lambda_max", "Log10 lambda maximum", min = -2, max = 6, value = 3, step = 1, width = "100%"),
            sliderInput("glmnet_lambda_n", "Lambda values", min = 10, max = 100, value = 50, step = 5, width = "100%")
          )
        ))
      } else if (mode == "specific") {
        controls <- c(controls, list(
          div(style="font-size:12px;color:#856404;background:#fff3cd;border-left:3px solid #ffc107;padding:8px 10px;border-radius:4px;margin-bottom:8px;",
              icon("circle-info"), HTML(" No model-specific controls are defined for this model yet. It will use the shared general configuration."))
        ))
      }
      controls <- c(controls, list(
        sliderInput(.config_id(scope, "tuneLength"), "Tune length",
                    min = 1, max = 50, value = defs$tune, step = 1, width = "100%")
      ))
      tagList(controls)
    })
        output$method_action_buttons <- renderUI({
      method <- input$active_method %||% "null"
      
      tagList(
        actionButton(inputId = paste0(method, "_Go"), label = "Train", icon = icon("play"),
                     width = "100%", style = "background:#534AB7;color:white;border:none;font-size:13px;margin-bottom:6px;"),
        actionButton(inputId = paste0(method, "_Load"), label = "Load", icon = icon("file-arrow-up"),
                     width = "100%", style = "font-size:13px;margin-bottom:6px;"),
        actionButton(inputId = paste0(method, "_Delete"), label = "Forget", icon = icon("trash-can"),
                     width = "100%", style = "font-size:13px;")
      )
    })
    
    getPreprocessConfig <- reactive({
      method <- input$active_method %||% "glmnet"
      scope  <- .config_scope()
      defs   <- if (.uses_general_config()) .preprocess_defaults("general") else .preprocess_defaults(method)
      
      val <- function(name, default) {
        input[[.config_id(scope, name)]] %||% default
      }
      
      list(
        impute_knn_neighbors = val("impute_knn_neighbors", defs$knn),
        impute_bag_trees     = val("impute_bag_trees", defs$bag),
        pca_num_comp         = val("pca_num_comp", defs$pca),
        pls_num_comp         = val("pls_num_comp", defs$pls),
        ica_num_comp         = val("ica_num_comp", defs$ica),
        nzv_freq_cut         = val("nzv_freq_cut", defs$nzv_freq),
        nzv_unique_cut       = val("nzv_unique_cut", defs$nzv_unique),
        other_threshold      = val("other_threshold", defs$other),
        other_label          = "other",
        poly_degree          = val("poly_degree", defs$poly),
        corr_threshold       = val("corr_threshold", defs$corr)
      )
    })
    
    getTuneLength <- reactive({
      method <- input$active_method %||% "glmnet"
      scope  <- .config_scope()
      
      if (.uses_general_config()) {
        input$cfg_general_tuneLength %||% 10
      } else {
        defs <- .preprocess_defaults(method)
        input[[.config_id(scope, "tuneLength")]] %||% defs$tune
      }
    })
    
    
    
    .model_na_action <- function(method) {
      if (method %in% c("earth", "bagEarth", "ranger")) {
        na.fail
      } else {
        na.pass
      }
    }
    
    # Ensure the "SavedModels folder exists
    if (!"./SavedModels" %in% list.dirs()) {
      dir.create("./SavedModels")
    }
    
    shiny::onSessionEnded(stopApp)
    
    
    # reactive getData ----
    getData <- reactive({
      d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
      d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
      d
    })
    
    # output BoxPlots ----
    output$BoxPlots <- renderPlot({
      d <- getData()
      numeric <- sapply(d, FUN = is.numeric)
      req(d, input$Multiplier, length(numeric) > 0)
      d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
      boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
    })
    
    # output Missing ----
    output$Missing <- renderPlot({
      d <- getData()
      vis_dat(d)
    })
    
    # output Corr ----
    output$Corr <- renderPlot({
      d <- getData()
      numeric <- sapply(d, FUN = is.numeric)
      req(d, length(numeric) > 0)
      corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
    })
    
    # output DataSummary ----
    output$DataSummary <- renderPrint({
      str(getData())
    })
    
    # output Table ----
    output$Table <- DT::renderDataTable({
      d <- getData()
      numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
      DT::datatable(d) %>%
        formatRound(columns = numeric, digits = 3)
    })
    
    # # reactive get Split
    # getSplit <- reactive({
    #   set.seed(199)
    #   createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
    # })
    
    
    # ── SPLIT: shared helpers ─────────────────────────────────────────────────
    
    split_numeric_cols <- reactive({
      df <- getData()
      names(df)[sapply(df, is.numeric) & names(df) != "Response"]
    })
    
    split_factor_cols <- reactive({
      df <- getData()
      names(df)[sapply(df, function(x) is.factor(x) | is.character(x))]
    })
    
    split_all_cols <- reactive({ names(getData()) })
    
    split_date_cols <- reactive({
      df <- getData()
      names(df)[sapply(df, function(x) inherits(x, c("Date","POSIXct","POSIXlt")))]
    })
    
    observe({
      cols <- split_date_cols()
      updateSelectInput(session, "p5_time",
                        choices = cols,
                        selected = if (length(cols) > 0) cols[1] else NULL)
    })
    
    observe({
      cols <- split_all_cols()
      for (id in c("p1_y","p2_y","p3_y")) {
        updateSelectInput(session, id, choices = cols,
                          selected = if ("Response" %in% cols) "Response" else cols[1])
      }
    })
    
    observe({
      updateSelectInput(session, "p4_group",
                        choices  = split_factor_cols(),
                        selected = split_factor_cols()[1])
    })
    
    observeEvent(input$p4_group, {
      df       <- getData()
      req(input$p4_group %in% names(df))
      n_groups <- length(unique(df[[input$p4_group]]))
      max_k    <- max(2, n_groups - 1)
      cur_k    <- min(isolate(input$p4_k), max_k)
      updateSliderInput(session, "p4_k", max = max_k, value = cur_k)
    })
    
    observe({
      cols <- split_numeric_cols()
      updateSelectInput(session, "p6_x", choices = cols, selected = cols[1])
      updateSelectInput(session, "p6_y", choices = cols,
                        selected = if (length(cols) > 1) cols[2] else cols[1])
      updateSelectizeInput(session, "p6_features", choices = cols, selected = cols)
    })
    
    # ── SPLIT TAB 1: 2-partition ──────────────────────────────────────────────
    
    p1_simple <- reactive({
      df <- getData()
      set.seed(getSplitSeed())
      base::sample(x = nrow(df), size = floor(input$p1_train * nrow(df)), replace = FALSE)
    })
    
    p1_strat <- reactive({
      df <- getData()
      req(input$p1_y %in% names(df))
      set.seed(getSplitSeed())
      caret::createDataPartition(y = df[[input$p1_y]], p = input$p1_train, list = FALSE)
    })
    
    .freq_table <- function(vec) {
      if (is.numeric(vec)) {
        table(cut(vec, breaks = quantile(vec, na.rm = TRUE), include.lowest = TRUE))
      } else {
        table(as.factor(vec))
      }
    }
    
    output$p1_full_table <- renderPrint({
      df <- getData()
      req(input$p1_y %in% names(df))
      cat(input$p1_y, "groups (full data):\n")
      print(.freq_table(df[[input$p1_y]]))
    })
    
    output$p1_simple_table <- renderPrint({
      df <- getData(); req(input$p1_y %in% names(df))
      idx <- p1_simple()
      cat(input$p1_y, "groups (simple random — train):\n")
      print(.freq_table(df[[input$p1_y]][idx]))
    })
    
    output$p1_strat_table <- renderPrint({
      df <- getData(); req(input$p1_y %in% names(df))
      idx <- p1_strat()
      cat(input$p1_y, "groups (stratified — train):\n")
      print(.freq_table(df[[input$p1_y]][idx]))
    })
    
    output$p1_sizes <- renderTable({
      df <- getData(); idx <- p1_strat(); n <- nrow(df)
      data.frame(Partition = c("Train","Test","Total"),
                 N = c(length(idx), n - length(idx), n),
                 Proportion = round(c(length(idx), n - length(idx), n) / n, 3),
                 check.names = FALSE)
    })
    
    # ── SPLIT TAB 2: 3-partition ──────────────────────────────────────────────
    
    p2_split <- reactive({
      df <- getData(); req(input$p2_y %in% names(df))
      set.seed(getSplitSeed())
      train_idx <- caret::createDataPartition(y = df[[input$p2_y]], p = input$p2_train, list = FALSE)
      remainder <- df[-train_idx, ]
      set.seed(getSplitSeed() + 1)
      val_idx_in_rem <- caret::createDataPartition(y = remainder[[input$p2_y]], p = input$p2_val, list = FALSE)
      list(train = train_idx,
           val   = as.integer(rownames(remainder)[val_idx_in_rem]),
           test  = as.integer(rownames(remainder)[-val_idx_in_rem]))
    })
    
    output$p2_counts <- renderPrint({
      idx <- p2_split()
      cat("There are", length(idx$train), "observations in the train data\n")
      cat("There are", length(idx$val),   "observations in the validation data\n")
      cat("There are", length(idx$test),  "observations in the test data\n")
    })
    
    output$p2_sizes <- renderTable({
      df <- getData(); idx <- p2_split(); n <- nrow(df)
      data.frame(Partition = c("Train","Validation","Test","Total"),
                 N = c(length(idx$train), length(idx$val), length(idx$test), n),
                 Proportion = round(c(length(idx$train), length(idx$val),
                                      length(idx$test), n) / n, 3),
                 check.names = FALSE)
    })
    
    # ── SPLIT TAB 3: Stratified bootstrap ────────────────────────────────────
    
    p3_resamples <- reactive({
      df <- getData(); req(input$p3_y %in% names(df))
      set.seed(getSplitSeed())
      caret::createResample(y = df[[input$p3_y]], times = input$p3_times, list = TRUE)
    })
    
    output$p3_str <- renderPrint({ str(p3_resamples()) })
    
    output$p3_perc <- renderPrint({
      rs <- p3_resamples()
      calcPerc <- function(x) {
        cat("Unique =", round(length(unique(x)) / length(x) * 100), "%\n")
      }
      invisible(mapply(rs, FUN = calcPerc))
    })
    
    # ── SPLIT TAB 4: Leave group out ─────────────────────────────────────────
    
    p4_folds <- reactive({
      df <- getData(); req(input$p4_group %in% names(df))
      grp      <- df[[input$p4_group]]
      n_groups <- length(unique(grp))
      safe_k   <- max(2, min(as.integer(input$p4_k), n_groups - 1))
      req(safe_k >= 2, n_groups >= 3)
      set.seed(getSplitSeed())
      caret::groupKFold(group = grp, k = safe_k)
    })
    
    output$p4_freq_table <- renderPrint({
      df <- getData(); req(input$p4_group %in% names(df))
      print(table(as.factor(df[[input$p4_group]])))
    })
    
    output$p4_folds_print <- renderPrint({ print(p4_folds()) })
    
    output$p4_in_groups <- renderPrint({
      df <- getData(); req(input$p4_group %in% names(df))
      folds <- p4_folds(); grp <- as.character(df[[input$p4_group]])
      print(sort(grp[folds[[1]]]))
    })
    
    output$p4_in_distinct <- renderPrint({
      df <- getData(); req(input$p4_group %in% names(df))
      folds <- p4_folds(); grp <- as.character(df[[input$p4_group]])
      cat("Distinct groups in train for Fold1:", length(unique(grp[folds[[1]]])), "\n")
    })
    
    output$p4_out_groups <- renderPrint({
      df <- getData(); req(input$p4_group %in% names(df))
      folds <- p4_folds(); grp <- as.character(df[[input$p4_group]])
      print(sort(grp[-folds[[1]]]))
    })
    
    output$p4_out_distinct <- renderPrint({
      df <- getData(); req(input$p4_group %in% names(df))
      folds <- p4_folds(); grp <- as.character(df[[input$p4_group]])
      cat("Distinct groups in test for Fold1:", length(unique(grp[-folds[[1]]])), "\n")
    })
    
    # ── SPLIT TAB 5: Time series ──────────────────────────────────────────────
    
    p5_slices <- reactive({
      df <- getData(); req(input$p5_time %in% names(df))
      df <- df[order(df[[input$p5_time]]), ]
      n  <- nrow(df)
      req(input$p5_init + input$p5_horizon <= n)
      caret::createTimeSlices(y = seq_len(n),
                              initialWindow = input$p5_init,
                              horizon       = input$p5_horizon,
                              fixedWindow   = as.logical(input$p5_fixed),
                              skip          = input$p5_skip)
    })
    
    output$p5_train_str <- renderPrint({ print(str(p5_slices()$train)) })
    output$p5_test_str  <- renderPrint({ print(str(p5_slices()$test)) })
    
    # ── SPLIT TAB 6: Diversity down-sampling ──────────────────────────────────
    
    p6_clusters <- reactive({
      df <- getData()
      feat_cols <- if (length(input$p6_features) > 0) {
        intersect(input$p6_features, names(df))
      } else {
        names(df)[sapply(df, is.numeric)]
      }
      req(length(feat_cols) > 0)
      nums <- df[, feat_cols, drop = FALSE]
      nums <- nums[complete.cases(nums), ]
      req(nrow(nums) >= input$p6_k)
      set.seed(getSplitSeed())
      cluster::pam(nums, k = input$p6_k,
                   metric = input$p6_metric,
                   stand  = as.logical(input$p6_stand))
    })
    
    output$p6_plot <- renderPlot({
      df <- getData(); cl <- p6_clusters()
      req(input$p6_x %in% names(df), input$p6_y %in% names(df))
      nums      <- df[, sapply(df, is.numeric), drop = FALSE]
      comp_rows <- which(complete.cases(nums))
      df_plot   <- df[comp_rows, ]
      df_plot$Type <- "not sampled"
      df_plot$Type[cl$id.med] <- "sampled"
      ggplot() +
        geom_point(data = df_plot,
                   mapping = aes(x = .data[[input$p6_x]],
                                 y = .data[[input$p6_y]],
                                 color = Type)) +
        labs(title = paste0("Data (sampled for diversity, k = ", input$p6_k, ")"),
             x = input$p6_x, y = input$p6_y) +
        theme_minimal(base_size = 13)
    })
    
    output$p6_table <- renderTable({
      df <- getData(); cl <- p6_clusters(); n <- nrow(df); k <- input$p6_k
      data.frame(Statistic = c("Total observations","Sampled (medoids)",
                               "Not sampled","Sample rate"),
                 Value = c(n, k, n - k, paste0(round(k / n * 100, 1), "%")))
    })
    
    # ── SPLIT: Apply-button logic + getSplit ───────────────────────────────────
    
    applied_indices <- reactiveVal({
      df <- isolate(getData())
      set.seed(isolate(getSplitSeed()))
      as.integer(caret::createDataPartition(y = df$Response, p = 0.8, list = FALSE))
    })
    
    applied_label <- reactiveVal("2-partition (train + test) — default")
    
    .notify_split <- function(label, n_train, n_test) {
      showNotification(paste0("Split applied: ", label,
                              "  |  Train: ", n_train, "  Test: ", n_test),
                       type = "message", duration = 4)
    }
    
    observeEvent(input$p1_apply, {
      idx <- isolate(p1_strat()); n <- isolate(nrow(getData()))
      applied_indices(as.integer(idx))
      applied_label("2-partition (train + test)")
      .notify_split("2-partition", length(idx), n - length(idx))
    })
    
    observeEvent(input$p2_apply, {
      idx <- isolate(p2_split()$train); n <- isolate(nrow(getData()))
      applied_indices(as.integer(idx))
      applied_label("3-partition (train + val + test)")
      .notify_split("3-partition — train set", length(idx), n - length(idx))
    })
    
    observeEvent(input$p3_apply, {
      idx <- isolate(unique(p3_resamples()[[1]])); n <- isolate(nrow(getData()))
      applied_indices(as.integer(idx))
      applied_label("Stratified bootstrap (resample 1)")
      .notify_split("Stratified bootstrap", length(idx), n - length(idx))
    })
    
    observeEvent(input$p4_apply, {
      idx <- isolate(p4_folds()[[1]]); n <- isolate(nrow(getData()))
      applied_indices(as.integer(idx))
      applied_label("Leave group out (Fold 1)")
      .notify_split("Leave group out", length(idx), n - length(idx))
    })
    
    observeEvent(input$p5_apply, {
      sl  <- isolate(p5_slices())
      idx <- sl$train[[length(sl$train)]]; n <- isolate(nrow(getData()))
      applied_indices(as.integer(idx))
      applied_label("Time series (last train slice)")
      .notify_split("Time series", length(idx), n - length(idx))
    })
    
    observeEvent(input$p6_apply, {
      idx <- isolate(p6_clusters()$id.med); n <- isolate(nrow(getData()))
      applied_indices(as.integer(idx))
      applied_label("Diversity down-sampling (medoids)")
      .notify_split("Diversity down-sampling", length(idx), n - length(idx))
    })
    
    # applied status banner — shared across all 6 tabs
    applied_banner <- reactive({
      div(style = "font-size:12px;color:white;background:#534AB7;
                 padding:6px 10px;border-radius:5px;margin-bottom:10px;
                 display:inline-block;",
          icon("circle-check"), HTML("&nbsp;"),
          paste("Currently applied:", applied_label()))
    })
    output$p1_applied <- output$p2_applied <- output$p3_applied <-
      output$p4_applied <- output$p5_applied <- output$p6_applied <-
      renderUI({ applied_banner() })
    
    # getSplit — feeds getTrainData and getTestData
    getSplit <- reactive({ applied_indices() })
    
    # ── STRATEGY: outlier response module ──────────────────────────────────
    out_response_roles <- reactive({
      df <- getData()
      setNames(rep("", length(names(df))), names(df))
    })
    
    out_response_result <- out_response_server(
      id = "out_response",
      get_data = getData,
      get_raw = getData,
      roles = out_response_roles
    )
    
    getStrategyData <- reactive({
      out_response_result$data()
    })
    
    # # reactive getMethods ----
    # getMethods <- reactive({
    #   mi <- caret::getModelInfo()
    #   Label <- vector(mode = "character", length = length(mi))
    #   Package <- vector(mode = "character", length = length(mi))
    #   Hyperparams <- vector(mode = "character", length = length(mi))
    #   Regression <- vector(mode = "logical", length = length(mi))
    #   Classification <- vector(mode = "logical", length = length(mi))
    #   Tags <- vector(mode = "character", length = length(mi))
    #   ClassProbs <- vector(mode = "character", length = length(mi))
    #   for (row in 1:length(mi)) {
    #     Label[row] <- mi[[row]]$label
    #     libs <- mi[[row]]$library
    #     libs <- na.omit(libs[libs != ""]) # remove blank libraries
    #     if (length(libs) > 0) {
    #       present <- vector(mode = "logical", length = length(libs))
    #       suppressWarnings({
    #         for (lib in 1:length(libs)) {
    #           present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
    #         }
    #       })
    #       check <- ifelse(present, "", as.character(icon(name = "ban")))
    #       Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
    #     }
    #     d <- mi[[row]]$parameters
    #     Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
    #     Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
    #     Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
    #     Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
    #     ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    #   }
    #   data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
    # })
    # 
    # # output Available ----
    # output$Available <- DT::renderDataTable({
    #   m <- getMethods()
    #   m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
    #   DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
    # })
    
    # ── AVAILABLE METHODS ─────────────────────────────────────────────────────
    
    av_mi <- caret::getModelInfo()
    
    # Group colours — order matches priority (first match wins on map)
    av_group_colours <- c(
      "1" = "#534AB7",   # purple  — Group 1
      "2" = "#0F6E56",   # teal    — Group 2
      "3" = "#BA7517",   # amber   — Group 3
      "4" = "#993C1D",   # coral   — Group 4
      "5" = "#1a6ebd",   # blue    — Group 5
      "6" = "#d63384",   # pink    — Group 6
      "none" = "#cccccc" # grey    — unmatched
    )
    
    av_methods_plain <- reactive({
      n <- length(av_mi)
      Label <- Tags_plain <- Packages_plain <- Hyperparams <- character(n)
      Regression <- Classification <- ClassProbs <- logical(n)
      for (i in seq_len(n)) {
        m                 <- av_mi[[i]]
        Label[i]          <- m$label
        libs              <- na.omit(m$library[m$library != ""])
        Packages_plain[i] <- paste(libs, collapse = "\n")
        Tags_plain[i]     <- paste(m$tags, collapse = "|")
        d                 <- m$parameters
        Hyperparams[i]    <- paste(paste0(d$parameter, " - ", d$label,
                                          " [", d$class, "]"), collapse = "\n")
        Regression[i]     <- "Regression"     %in% m$type
        Classification[i] <- "Classification" %in% m$type
        ClassProbs[i]     <- is.function(m$prob)
      }
      data.frame(Model = names(av_mi), Label, Packages_plain, Tags_plain,
                 Hyperparams, Regression, Classification, ClassProbs,
                 stringsAsFactors = FALSE)
    })
    
    av_packages_html <- reactive({
      df   <- av_methods_plain()
      html <- character(nrow(df))
      for (i in seq_len(nrow(df))) {
        libs <- strsplit(df$Packages_plain[i], "\n")[[1]]
        libs <- libs[nchar(libs) > 0]
        if (length(libs) > 0) {
          present <- suppressWarnings(
            sapply(libs, function(l)
              require(l, warn.conflicts = FALSE,
                      character.only = TRUE, quietly = TRUE))
          )
          check   <- ifelse(present, "", as.character(icon("ban")))
          html[i] <- paste(paste(libs, check), collapse = "<br/>")
        }
      }
      data.frame(Model = df$Model, Packages_html = html,
                 stringsAsFactors = FALSE)
    })
    
    av_wide_matrix <- reactive({
      tags <- lapply(av_mi, `[[`, "tags")
      Reg  <- sapply(av_mi, function(m) as.integer("Regression"     %in% m$type))
      Cls  <- sapply(av_mi, function(m) as.integer("Classification" %in% m$type))
      all_tags <- sort(unique(unlist(tags)))
      all_tags <- all_tags[nchar(all_tags) > 0]
      dat <- matrix(0L, nrow = length(av_mi), ncol = length(all_tags),
                    dimnames = list(names(av_mi), all_tags))
      for (i in seq_along(tags)) {
        matched <- intersect(tags[[i]], all_tags)
        if (length(matched) > 0) dat[i, matched] <- 1L
      }
      as.data.frame(cbind(Regression = Reg, Classification = Cls, dat))
    })
    
    # Observer 1 — populate exclude selector with ALL tags (runs once)
    observe({
      all_tags <- sort(unique(unlist(lapply(av_mi, `[[`, "tags"))))
      all_tags <- all_tags[nchar(all_tags) > 0]
      updateSelectizeInput(session, "av_flt_exclude", choices = all_tags,
                           server = TRUE,
                           selected = c("Two Class Only", "ROC Curves",
                                        "Text Mining", "String Kernel",
                                        "Self-Organising Maps",
                                        "Binary Predictors Only",
                                        "Categorical Predictors Only",
                                        "Cost Sensitive Learning",
                                        "Ordinal Outcomes"))
    })
    
    # Observer 2 — populate sections 2 & 3 with only non-excluded tags (re-runs when exclusions change)
    observe({
      all_tags <- sort(unique(unlist(lapply(av_mi, `[[`, "tags"))))
      all_tags <- all_tags[nchar(all_tags) > 0]
      available_tags <- all_tags[!all_tags %in% input$av_flt_exclude]
      
      default_lit_tags <- intersect(available_tags, c(
        "Regularization",
        "L1 Regularization",
        "L2 Regularization",
        "Implicit Feature Selection",
        "Feature Selection Wrapper",
        "Handle Missing Predictor Data",
        "Robust Methods",
        "Multivariate Adaptive Regression Splines"
      ))
      
      current_lit_tags <- isolate(input$av_flt_any)
      
      updateSelectizeInput(
        session,
        "av_flt_any",
        choices = available_tags,
        selected = if (length(current_lit_tags) > 0) current_lit_tags else default_lit_tags,
        server = TRUE
      )
      
      updateSelectizeInput(session, "av_g1", choices = available_tags,
                           selected = intersect("Neural Network", available_tags), server = TRUE)
      updateSelectizeInput(session, "av_g2", choices = available_tags,
                           selected = intersect("Linear Regression", available_tags), server = TRUE)
      updateSelectizeInput(session, "av_g3", choices = available_tags,
                           selected = intersect("Tree-Based Model", available_tags), server = TRUE)
      updateSelectizeInput(session, "av_g4", choices = available_tags,
                           selected = intersect("Kernel Method", available_tags), server = TRUE)
      updateSelectizeInput(session, "av_g5", choices = available_tags,
                           selected = intersect("Ensemble Model", available_tags), server = TRUE)
      updateSelectizeInput(session, "av_g6", choices = available_tags,
                           selected = intersect("Multivariate Adaptive Regression Splines", available_tags), server = TRUE)
    })
    
    # Helper: does a method's Tags_plain match ALL tags in a group?
    .matches_group <- function(tags_plain, group_tags) {
      if (length(group_tags) == 0) return(FALSE)
      all(sapply(group_tags, function(t)
        grepl(t, tags_plain, ignore.case = TRUE)))
    }
    
    # ── Shared filter reactive (exclude + any + type) ─────────────────────────
    # Returns the base-filtered df before group colouring
    av_base_df <- reactive({
      df <- av_methods_plain()
      
      # type filter
      df <- switch(input$av_type,
                   "reg"  = df[df$Regression,     ],
                   "cls"  = df[df$Classification, ],
                   "both" = df
      )
      
      # exclude tags — global grey-out
      for (tag in input$av_flt_exclude)
        df <- df[!grepl(tag, df$Tags_plain, ignore.case = TRUE), ]
      
      df
    })
    
    # Assign group membership to each method (first match wins)
    av_grouped_df <- reactive({
      df <- av_base_df()
      
      g_inputs <- list(
        "1" = input$av_g1,
        "2" = input$av_g2,
        "3" = input$av_g3,
        "4" = input$av_g4,
        "5" = input$av_g5,
        "6" = input$av_g6
      )
      
      df$Group <- sapply(df$Tags_plain, function(tp) {
        matched <- "none"
        for (g in names(g_inputs)) {
          if (.matches_group(tp, g_inputs[[g]])) {
            matched <- g
            break
          }
        }
        matched
      })
      
      df
    })
    
    # The filtered df for the table — only show methods matching at least one
    # active group (if any groups have tags set), or all base-filtered methods
    av_filtered_df <- reactive({
      df <- av_grouped_df()
      # if all groups are empty, show everything that passed base filter
      any_group_active <- any(sapply(
        list(input$av_g1, input$av_g2, input$av_g3, input$av_g4, input$av_g5, input$av_g6),
        function(x) length(x) > 0
      ))
      if (any_group_active) df[df$Group != "none", ] else df
    })
    
    # Sidebar summary
    output$av_filter_summary <- renderPrint({
      df <- av_filtered_df()
      cat(nrow(df), "method(s)\n")
      if (nrow(df) > 0 && nrow(df) <= 30)
        cat(paste(sort(df$Model), collapse = ", "))
    })
    
    # ── Method table ──────────────────────────────────────────────────────────
    output$av_method_table <- DT::renderDataTable({
      filt <- av_filtered_df()
      html <- av_packages_html()
      df   <- merge(filt, html, by = "Model")
      
      display <- data.frame(
        Model          = df$Model,
        Label          = df$Label,
        Packages       = df$Packages_html,
        Tags           = gsub("\\|", ", ", df$Tags_plain),
        Hyperparameters = df$Hyperparams,
        Regression     = df$Regression,
        Classification = df$Classification,
        ClassProbs     = df$ClassProbs,
        Group          = df$Group,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(display, escape = FALSE, rownames = FALSE,
                    selection = "none",
                    options = list(pageLength = 10,
                                   lengthMenu = c(5,10,25,50),
                                   scrollX = TRUE))
    })
    
    # ── Method map ────────────────────────────────────────────────────────────
    output$av_map_plot <- renderPlot({
      wide <- av_wide_matrix()
      
      # restrict to method type
      wide_sub <- switch(input$av_type,
                         "reg"  = wide[wide$Regression == 1,     ],
                         "cls"  = wide[wide$Classification == 1, ],
                         "both" = wide
      )
      req(nrow(wide_sub) >= 3)
      lit_size_boost <- 3   # font size boost for literature-informed highlights
      
      # cmdscale
      d  <- stats::dist(wide_sub, method = input$av_map_dist)
      dd <- stats::cmdscale(d, k = 2)
      df_map <- data.frame(
        Model = rownames(dd), X1 = dd[, 1], X2 = dd[, 2],
        stringsAsFactors = FALSE
      )
      
      # attach group from grouped_df (only methods that survived base filter)
      grp_df <- av_grouped_df()[, c("Model", "Group")]
      df_map <- merge(df_map, grp_df, by = "Model", all.x = TRUE)
      df_map$Group[is.na(df_map$Group)] <- "none"
      
      # methods excluded by base filter get grey too
      base_models <- av_base_df()$Model
      df_map$Group[!df_map$Model %in% base_models] <- "none"
      
      type_label <- switch(input$av_type,
                           "reg"  = "Regression",
                           "cls"  = "Classification",
                           "both" = "All"
      )
      
      p <- ggplot(mapping = aes(x = X1, y = X2, label = Model)) +
        ggtitle(paste(type_label, "Methods — coloured by group")) +
        xlab("Coordinate 1") + ylab("Coordinate 2") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
      
      # grey background — split into lit-highlighted vs plain grey
      df_grey <- df_map[df_map$Group == "none", ]
      if (nrow(df_grey) > 0) {
        # determine which grey methods match the literature highlight
        if (length(input$av_flt_any) > 0) {
          pattern <- paste(input$av_flt_any, collapse = "|")
          # need Tags_plain to check — join from base_df
          tags_lookup <- av_base_df()[, c("Model", "Tags_plain")]
          df_grey <- merge(df_grey, tags_lookup, by = "Model", all.x = TRUE)
          df_grey$LitHit <- grepl(pattern, df_grey$Tags_plain, ignore.case = TRUE)
        } else {
          df_grey$LitHit <- FALSE
        }
        
        df_grey_plain <- df_grey[!df_grey$LitHit, ]
        df_grey_lit   <- df_grey[df_grey$LitHit,  ]
        
        # plain grey — normal weight
        if (nrow(df_grey_plain) > 0) {
          p <- p +
            geom_point(data = df_grey_plain, color = "#cccccc", size = 1.5) +
            ggrepel::geom_text_repel(data = df_grey_plain,
                                     size = input$av_map_label_size,
                                     color = "#aaaaaa",
                                     max.overlaps = input$av_map_overlaps,
                                     na.rm = TRUE)
        }
        
        # lit-highlighted grey — bold, same size as plain grey
        if (nrow(df_grey_lit) > 0) {
          p <- p +
            geom_point(data = df_grey_lit, color = "#888888", size = 2) +
            ggrepel::geom_text_repel(data = df_grey_lit,
                                     size = input$av_map_label_size + lit_size_boost,
                                     color = "#555555",
                                     fontface = "bold",
                                     max.overlaps = input$av_map_overlaps,
                                     box.padding = 0.4,
                                     na.rm = TRUE)
        }
      }
      
      # coloured groups on top (groups 6 → 1 so group 1 renders last = on top)
      for (g in c("6", "5", "4", "3", "2", "1")) {
        df_g <- df_map[df_map$Group == g, ]
        if (nrow(df_g) > 0) {
          col <- av_group_colours[g]
          
          # check which methods in this group also match the lit highlight
          if (length(input$av_flt_any) > 0) {
            pattern <- paste(input$av_flt_any, collapse = "|")
            tags_lookup <- av_base_df()[, c("Model", "Tags_plain")]
            df_g <- merge(df_g, tags_lookup, by = "Model", all.x = TRUE)
            df_g$LitHit <- grepl(pattern, df_g$Tags_plain, ignore.case = TRUE)
          } else {
            df_g$LitHit <- FALSE
          }
          
          df_g_plain <- df_g[!df_g$LitHit, ]
          df_g_lit   <- df_g[df_g$LitHit,  ]
          
          # colour only — plain weight
          if (nrow(df_g_plain) > 0) {
            p <- p +
              geom_point(data = df_g_plain, color = col, size = 2) +
              ggrepel::geom_text_repel(data = df_g_plain,
                                       size = input$av_map_label_size,
                                       color = col,
                                       fontface = "plain",
                                       max.overlaps = input$av_map_overlaps,
                                       box.padding = 0.4, na.rm = TRUE)
          }
          
          # colour + bold (lit-highlighted within group)
          if (nrow(df_g_lit) > 0) {
            p <- p +
              geom_point(data = df_g_lit, color = col, size = 2.5) +
              ggrepel::geom_text_repel(data = df_g_lit,
                                       size = input$av_map_label_size + lit_size_boost,
                                       color = col,
                                       fontface = "bold",
                                       max.overlaps = input$av_map_overlaps,
                                       box.padding = 0.4, na.rm = TRUE)
          }
        }
      }
      
      # caption legend — avoids aes() conflict with manual colour assignments
      active_groups <- sort(unique(df_map$Group[df_map$Group != "none"]))
      if (length(active_groups) > 0) {
        group_names <- c("1"="Neural Network","2"="OLS","3"="Tree-Based",
                         "4"="Kernel","5"="Ensemble","6"="Wildcard")
        caption_lines <- sapply(active_groups, function(g)
          paste0("Group ", g, " (", group_names[g], ")"))
        p <- p + labs(caption = paste(caption_lines, collapse = "   "))
      }
      
      p
    })
    
    
    # reactive getTrainData ----
    getTrainData <- reactive({
      d <- getStrategyData()
      train_rows <- rownames(getData())[getSplit()]
      keep <- intersect(train_rows, rownames(d))
      d[keep, , drop = FALSE]
    })
    
    # reactive getTestData ----
    getTestData <- reactive({
      d <- getStrategyData()
      test_rows <- rownames(getData())[-getSplit()]
      keep <- intersect(test_rows, rownames(d))
      d[keep, , drop = FALSE]
    })
    
    # reactive getTrControl ----
    getTrControl <- reactive({
      # Shared resampling specification for fair model comparison.
      y <- getTrainData()[,"Response"]
      resampling <- input$cfg_resampling %||% "boot"
      search <- input$cfg_search %||% "grid"
      set.seed(getTrainSeed())
      
      if (resampling == "cv") {
        n <- input$cfg_cv_folds %||% 10
        idx <- caret::createFolds(y = y, k = n, returnTrain = TRUE)
        reps <- NA
      } else if (resampling == "repeatedcv") {
        n <- input$cfg_repeatedcv_folds %||% 4
        reps <- input$cfg_repeatedcv_repeats %||% 6
        idx <- caret::createMultiFolds(y = y, k = n, times = reps)
      } else {
        resampling <- "boot"
        n <- input$cfg_boot_n %||% 25
        idx <- caret::createResample(y = y, times = n)
        reps <- NA
      }
      
      # Let caret manage per-model seed vectors. Different methods expand
      # tuneLength into different grid sizes, e.g. xgbTree can need 100 seeds.
      
      trainControl(method = resampling, number = n, repeats = reps, allowParallel = TRUE, search = search,
                   index = idx, savePredictions = "final", trim = TRUE)
    })
    
    # # output SplitSummary ----
    # output$SplitSummary <- renderPrint({
    #   cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
    # })
    
    # reactive getResamples ----
    getResamples <- reactive({
      models2 <- reactiveValuesToList(models) %>% 
        rlist::list.clean( fun = is.null, recursive = FALSE)
      req(length(models2) > 1)
      results <- caret::resamples(models2)
      
      #scale metrics using null model. Tough code to follow -sorry
      NullModel <- "null"
      if (input$NullNormalise & NullModel %in% results$models) {
        actualNames <- colnames(results$values)
        # Normalise the various hyper-metrics except R2 (as this is already normalised)
        for (metric in c("RMSE", "MAE")) {
          col <- paste(sep = "~", NullModel, metric)
          if (col %in% actualNames) {
            nullMetric <- mean(results$values[, col], na.rm = TRUE)
            if (!is.na(nullMetric) & nullMetric != 0) {
              for (model in results$models) {
                mcol <- paste(sep = "~", model, metric)
                if (mcol %in% actualNames) {
                  results$values[, mcol] <- results$values[, mcol] / nullMetric
                }
              }
            }
          }
        }
      }
      
      # hide results worse than null model
      subset <- rep(TRUE, length(models2))
      if (input$HideWorse & NullModel %in% names(models2)) {
        actualNames <- colnames(results$values)
        col <- paste(sep = "~", "null","RMSE" )
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric)) {
            m <- 0
            for (model3 in results$models) {
              m <- m + 1
              mcol <- paste(sep = "~", model3, "RMSE")
              if (mcol %in% actualNames) {
                subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
              }
            }
          }
        }
        results$models <- results$models[subset]
      }
      
      updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "")  ## change the value parameter to your best method
      results
    })
    
    # output SelectionBoxPlot (plot) ----
    output$SelectionBoxPlot <- renderPlot({
      mod <- getResamples()
      bwplot(mod, notch = input$Notch, ylab = "Model", scales = list(y = list(cex = 1.25, font = 2)), par.settings = list(axis.text = list(cex = 1.1), par.ylab.text = list(cex = 1.15, font = 2), strip.background = list(col = "#f2f2f2"), strip.border = list(col = "#555555"), par.strip.text = list(cex = 1.05, font = 2)))
    })
    
    # output Title (UI) ----
    output$Title <- renderUI({
      tags$h3(paste("Unseen data results for chosen model:", input$Choice))
    })
    
    .restore_prediction_scale <- function(mod, predictions) {
      predictions <- as.numeric(predictions)
      if (!is.null(mod$outcomeCenter) && !is.null(mod$outcomeScale)) {
        predictions <- predictions * mod$outcomeScale + mod$outcomeCenter
      }
      predictions
    }
    
    .predict_model <- function(mod, dat) {
      if (!is.null(mod$preppedRecipe)) {
        baked <- recipes::bake(mod$preppedRecipe, new_data = dat)
        x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
        x <- x[, sapply(x, is.numeric), drop = FALSE]
        x <- as.matrix(x)
        if (!is.null(mod$bakedFeatureNames)) {
          colnames(x) <- make.names(colnames(x), unique = TRUE)
          missing_cols <- setdiff(mod$bakedFeatureNames, colnames(x))
          for (m in missing_cols) {
            x <- cbind(x, 0)
            colnames(x)[ncol(x)] <- m
          }
          x <- x[, mod$bakedFeatureNames, drop = FALSE]
        }
        return(.restore_prediction_scale(mod, predict(mod, newdata = x)))
      }
      .restore_prediction_scale(mod, predict(mod, newdata = dat))
    }
        # reactive getTestResults ----
    getTestResults <- reactive({
      dat <- getTestData()
      req(input$Choice)
      mod <- models[[input$Choice]]
      predictions <- .predict_model(mod, dat)
      d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
      colnames(d) <- c("obs", "pred")
      d
    })
    
    # reactive getTrainResults ----
    getTrainResults <- reactive({
      dat <- getTrainData()
      req(input$Choice)
      mod <- models[[input$Choice]]
      predictions <- .predict_model(mod, dat)
      d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
      colnames(d) <- c("obs", "pred")
      d
    })
    
    # Range for charts
    getResidualRange <- reactive({
      d1 <- getTrainResults()
      d1$residuals <- d1$obs - d1$pred
      d2 <- getTestResults()
      d2$residuals <- d2$obs - d2$pred
      d <- c(d1$residuals, d2$residuals)
      range(d, na.rm = TRUE)
    })
    
    # output TestSummary (print)
    output$TestSummary <- renderPrint({
      if (is.null(input$Choice) || input$Choice == "") {
        cat("No model chosen")
      } else {
        caret::defaultSummary(getTestResults())
      }
    })
    
    # output TestPlot (plot) ----
    output$TestPlot <- renderPlot({
      d <- getTestResults()
      req(nrow(d) > 0)
      par(pty = "s")
      range <- range(c(d$obs, d$pred), na.rm = TRUE)
      plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
      abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
    })
    
    # output TestResiduals (plot) ----
    output$TestResiduals <- renderPlot({
      d <- getTestResults()
      req(nrow(d) > 0)
      d$residuals <- d$obs - d$pred
      coef <- input$IqrM
      limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
      label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
      ggplot(d, mapping = aes(y = residuals, x = 0)) +
        ylim(getResidualRange()[1], getResidualRange()[2]) +
        geom_boxplot(coef = coef, orientation = "vertical", ) +
        ggrepel::geom_text_repel(aes(label = label)) +
        labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    })
    
    # output TrainResiduals (plot) ----
    output$TrainResiduals <- renderPlot({
      d <- getTrainResults()
      req(nrow(d) > 0)
      d$residuals <- d$obs - d$pred
      coef <- input$IqrM
      limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
      label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
      ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
        ylim(getResidualRange()[1], getResidualRange()[2]) +
        geom_boxplot(coef = coef, orientation = "vertical") +
        ggrepel::geom_text_repel() +
        labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    })
    
    
    # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
    
    # reactive getNullRecipe ----
    getNullRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData())
    })
    
    # observeEvent null_Go ----
    observeEvent(
      input$null_Go,
      {
        method <- "null"
        models[[method]] <- NULL
        showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
        obj <- startMode(input$Parallel)
        tryCatch({
          timing <- system.time({
          model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
          })
          training_times[[method]] <- timing[["elapsed"]]
          model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
          deleteRds(method)
          saveToRds(model, method)
          models[[method]] <- model
        }, 
        finally = {
          removeNotification(id = method)
          stopMode(obj)
        })
      }
    )
    
    observeEvent(
      input$null_Load,
      {
        method  <- "null"
        model <- loadRds(method, session)
        if (!is.null(model)) {
          models[[method]] <- model
        }
      }
    )
    
    observeEvent(
      input$null_Delete,
      {
        method <- "null"
        models[[method]] <- NULL
        gc()
      }
    )
    
    # observeEvent null_Metrics ----
    output$null_Metrics <- renderTable({
      getBestMetricRow("null")
    })
    
    output$null_TuningNote <- renderPrint({
      cat("The null model has no hyperparameters. It is used as a baseline for model selection.")
    })
    
    output$null_TrainSummary <- renderPrint({
      method <- "null"
      mod <- models[[method]]
      req(mod)
      elapsed <- if (!is.null(training_times[[method]])) {
        training_times[[method]]
      } else {
        mod$trainingTimeSeconds
      }
      if (!is.null(elapsed)) {
        cat("Training time:", round(elapsed, 2), "seconds\n\n")
      }
      print(mod)
    })
    
    
    # output null_Recipe (table) ----
    output$null_Recipe <- renderTable({
      method <- "null"
      mod <- models[[method]]
      req(mod)
      terms <- mod$recipe$term_info
      n <- dim(terms)[1]
      types <- vector(mode="character", length=n)
      for (row in 1:n) {
        types[row] <- paste(collapse = " ", unlist(terms$type[row]))
      }
      terms$type <- types
      terms
    })  
    
    
    
    
    # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
    library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
    # reactive getGlmnetRecipe ----
    getGlmnetRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    
    
    # observe GO event ----
    observeEvent(
      input$glmnet_Go,
      {
        method <- "glmnet"
        models[[method]] <- NULL
        showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
        obj <- startMode(input$Parallel)
        tryCatch({
          timing <- system.time({
          specific_mode <- (input$method_config_mode %||% "general") == "specific"
          penalty_mode <- if (specific_mode) input$glmnet_penalty_mode %||% "elasticnet" else "elasticnet"
          grid_mode <- if (specific_mode) input$glmnet_grid_mode %||% "default" else "default"
          
          if (grid_mode == "custom") {
            alpha_values <- if (penalty_mode == "ridge") {
              0
            } else if (penalty_mode == "lasso") {
              1
            } else {
              amin <- min(input$glmnet_alpha_min %||% 0.1, input$glmnet_alpha_max %||% 1.0)
              amax <- max(input$glmnet_alpha_min %||% 0.1, input$glmnet_alpha_max %||% 1.0)
              astep <- input$glmnet_alpha_step %||% 0.1
              round(seq(amin, amax, by = astep), 3)
            }
            lmin <- min(input$glmnet_lambda_min %||% -3, input$glmnet_lambda_max %||% 3)
            lmax <- max(input$glmnet_lambda_min %||% -3, input$glmnet_lambda_max %||% 3)
            lambda_values <- 10^seq(lmin, lmax, length.out = input$glmnet_lambda_n %||% 50)
            tune_grid <- expand.grid(alpha = alpha_values, lambda = lambda_values)
            model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method,
                                  metric = "RMSE", trControl = getTrControl(),
                                  tuneGrid = tune_grid, na.action = .model_na_action(method))
          } else if (penalty_mode == "ridge") {
            tune_grid <- expand.grid(alpha = 0,
                                     lambda = 10^seq(-4, 4, length.out = getTuneLength()))
            model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method,
                                  metric = "RMSE", trControl = getTrControl(),
                                  tuneGrid = tune_grid, na.action = .model_na_action(method))
          } else if (penalty_mode == "lasso") {
            tune_grid <- expand.grid(alpha = 1,
                                     lambda = 10^seq(-4, 4, length.out = getTuneLength()))
            model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method,
                                  metric = "RMSE", trControl = getTrControl(),
                                  tuneGrid = tune_grid, na.action = .model_na_action(method))
          } else {
            model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method,
                                  metric = "RMSE", trControl = getTrControl(),
                                  tuneLength = getTuneLength(), na.action = na.pass)
          }
          })
          training_times[[method]] <- timing[["elapsed"]]
          model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
          deleteRds(method)
          saveToRds(model, method)
          models[[method]] <- model
        },
        finally = {
          removeNotification(id = method)
          stopMode(obj)
        })
      }
    )
    
    observeEvent(
      input$glmnet_Load,
      {
        method  <- "glmnet"
        model <- loadRds(method, session)
        if (!is.null(model)) {
          models[[method]] <- model
        }
      }
    )
    
    observeEvent(
      input$glmnet_Delete,
      {
        method <- "glmnet"
        models[[method]] <- NULL
        gc()
      }
    )
    
    # output method summary text ----
    output$glmnet_MethodSummary <- renderText({
      method <- "glmnet"
      description(method)
    })
    
    # output resampling metrics table ----
    output$glmnet_Metrics <- renderTable({
      getBestMetricRow("glmnet")
    })
    
    # output hyperparameter tuning chart ----
    output$glmnet_ModelTune <- renderPlot({
      method <- "glmnet"
      mod <- models[[method]]
      req(mod)
      plot(mod)
    })
    
    # output an html formatted recipe "print" ----
    output$glmnet_RecipePrint <- renderUI({
      method <- "glmnet"
      mod <- models[[method]]
      req(mod)
      html <- mod$recipe %>%
        print() %>%
        cli::cli_fmt() %>%
        cli::ansi_collapse(sep="<br>", last = "<br>") %>%
        cli::ansi_html(escape_reserved = FALSE) %>%
        gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
      css <- paste(format(ansi_html_style()), collapse= "\n")
      tagList(
        tags$head(tags$style(css)),
        tags$pre(HTML(html))
      )
    })
    
    
    # output Recipe-output table ----
    output$glmnet_RecipeOutput <- renderTable({
      method <- "glmnet"
      mod <- models[[method]]
      req(mod)
      terms <- as.data.frame(mod$recipe$term_info)
      n <- dim(terms)[1]
      types <- vector(mode="character", length=n)
      for (row in 1:n) {
        types[row] <- paste(collapse = " ", unlist(terms$type[row]))
      }
      terms$type <- types
      terms |>
        dplyr::filter(role == "predictor") |>
        dplyr::select(type, source) |>
        dplyr::group_by(type, source) |>
        dplyr::summarise(count = n())
    })  
    
    # output training summary print ----
    output$glmnet_TrainSummary <- renderPrint({
      method <- "glmnet"
      mod <- models[[method]]
      req(mod)
      elapsed <- if (!is.null(training_times[[method]])) {
        training_times[[method]]
      } else {
        mod$trainingTimeSeconds
      }
      if (!is.null(elapsed)) {
        cat("Training time:", round(elapsed, 2), "seconds\n\n")
      }
      print(mod)
    })
    
    # output coefficient print ----
    output$glmnet_Coef <- renderTable({
      req(models$glmnet)
      co <- as.matrix(coef(models$glmnet$finalModel,
                           s = models$glmnet$bestTune$lambda))  # special for glmnet
      df <- data.frame(
        Predictor = rownames(co),
        Coefficient = as.numeric(co[, 1]),
        row.names = NULL,
        check.names = FALSE
      )
      df <- df[df$Coefficient != 0, , drop = FALSE]
      df[order(abs(df$Coefficient), decreasing = TRUE), ]
    }, rownames = FALSE)
    
    
    
    # METHOD * neural networks ----------------------------------------------------------------------------------------------------------------
    # These candidates use the shared general controls by default.
    .train_and_store <- function(method, train_expr) {
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- force(train_expr)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    }
    
    .recipe_summary_table <- function(method) {
      mod <- models[[method]]
      req(mod)
      terms <- as.data.frame(mod$recipe$term_info)
      n <- dim(terms)[1]
      types <- vector(mode="character", length=n)
      for (row in 1:n) {
        types[row] <- paste(collapse = " ", unlist(terms$type[row]))
      }
      terms$type <- types
      terms |>
        dplyr::filter(role == "predictor") |>
        dplyr::select(type, source) |>
        dplyr::group_by(type, source) |>
        dplyr::summarise(count = n())
    }
    
    .recipe_print_ui <- function(method) {
      mod <- models[[method]]
      req(mod)
      html <- mod$recipe %>%
        print() %>%
        cli::cli_fmt() %>%
        cli::ansi_collapse(sep="<br>", last = "<br>") %>%
        cli::ansi_html(escape_reserved = FALSE) %>%
        gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
      css <- paste(format(ansi_html_style()), collapse= "\n")
      tagList(
        tags$head(tags$style(css)),
        tags$pre(HTML(html))
      )
    }
    
    .train_summary_print <- function(method) {
      mod <- models[[method]]
      req(mod)
      elapsed <- if (!is.null(training_times[[method]])) {
        training_times[[method]]
      } else {
        mod$trainingTimeSeconds
      }
      if (!is.null(elapsed)) {
        cat("Training time:", round(elapsed, 2), "seconds\n\n")
      }
      print(mod)
    }
    
    .forget_model <- function(method) {
      models[[method]] <- NULL
      gc()
    }
    
    # reactive getBrnnRecipe ----
    getBrnnRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$brnn_Go, {
      method <- "brnn"
      if (!requireNamespace("brnn", quietly = TRUE)) {
        showNotification("Package brnn is required before training brnn.", type = "error", duration = 6)
        return(NULL)
      }
      .train_and_store(method, {
        caret::train(getBrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                     trControl = getTrControl(), tuneLength = getTuneLength(), na.action = na.pass)
      })
    })
    
    observeEvent(input$brnn_Load, {
      method <- "brnn"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$brnn_Delete, { .forget_model("brnn") })
    
    output$brnn_MethodSummary <- renderText({ description("brnn") })
    output$brnn_Metrics <- renderTable({ getBestMetricRow("brnn") })
    output$brnn_ModelTune <- renderPlot({ mod <- models[["brnn"]]; req(mod); plot(mod) })
    output$brnn_RecipePrint <- renderUI({ .recipe_print_ui("brnn") })
    output$brnn_RecipeOutput <- renderTable({ .recipe_summary_table("brnn") })
    output$brnn_TrainSummary <- renderPrint({ .train_summary_print("brnn") })
    
    # reactive getAvNNetRecipe ----
    getAvNNetRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$avNNet_Go, {
      method <- "avNNet"
      if (!requireNamespace("nnet", quietly = TRUE)) {
        showNotification("Package nnet is required before training avNNet.", type = "error", duration = 6)
        return(NULL)
      }
      .train_and_store(method, {
        caret::train(getAvNNetRecipe(), data = getTrainData(), method = method, metric = "RMSE",
                     trControl = getTrControl(), tuneLength = getTuneLength(), na.action = .model_na_action(method),
                     linout = TRUE, trace = FALSE, maxit = 1000, MaxNWts = 10000)
      })
    })
    
    observeEvent(input$avNNet_Load, {
      method <- "avNNet"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$avNNet_Delete, { .forget_model("avNNet") })
    
    output$avNNet_MethodSummary <- renderText({ description("avNNet") })
    output$avNNet_Metrics <- renderTable({ getBestMetricRow("avNNet") })
    output$avNNet_ModelTune <- renderPlot({ mod <- models[["avNNet"]]; req(mod); plot(mod) })
    output$avNNet_RecipePrint <- renderUI({ .recipe_print_ui("avNNet") })
    output$avNNet_RecipeOutput <- renderTable({ .recipe_summary_table("avNNet") })
    output$avNNet_TrainSummary <- renderPrint({ .train_summary_print("avNNet") })
    
    # METHOD * additional candidates -----------------------------------------------------------------------------------------------------------
    # These model blocks are intentionally explicit so model-specific failures point to the model being trained.

    # METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------
    getCubistRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$cubist_Go, {
      method <- "cubist"
      if (!requireNamespace("Cubist", quietly = TRUE)) {
        showNotification(paste("Package", "Cubist", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- caret::train(getCubistRecipe(), data = getTrainData(), method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.pass)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$cubist_Load, {
      method <- "cubist"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$cubist_Delete, { .forget_model("cubist") })
    
    output$cubist_MethodSummary <- renderText({ description("cubist") })
    output$cubist_Metrics <- renderTable({ getBestMetricRow("cubist") })
    output$cubist_ModelTune <- renderPlot({ mod <- models[["cubist"]]; req(mod); plot(mod) })
    output$cubist_RecipePrint <- renderUI({ .recipe_print_ui("cubist") })
    output$cubist_RecipeOutput <- renderTable({ .recipe_summary_table("cubist") })
    output$cubist_TrainSummary <- renderPrint({ .train_summary_print("cubist") })
    # METHOD * M5 ---------------------------------------------------------------------------------------------------------------------------
    getM5Recipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$M5_Go, {
      method <- "M5"
      if (!requireNamespace("RWeka", quietly = TRUE)) {
        showNotification(paste("Package", "RWeka", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- caret::train(getM5Recipe(), data = getTrainData(), method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.pass)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$M5_Load, {
      method <- "M5"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$M5_Delete, { .forget_model("M5") })
    
    output$M5_MethodSummary <- renderText({ description("M5") })
    output$M5_Metrics <- renderTable({ getBestMetricRow("M5") })
    output$M5_ModelTune <- renderPlot({ mod <- models[["M5"]]; req(mod); plot(mod) })
    output$M5_RecipePrint <- renderUI({ .recipe_print_ui("M5") })
    output$M5_RecipeOutput <- renderTable({ .recipe_summary_table("M5") })
    output$M5_TrainSummary <- renderPrint({ .train_summary_print("M5") })
    # METHOD * M5Rules ---------------------------------------------------------------------------------------------------------------------------
    getM5RulesRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$M5Rules_Go, {
      method <- "M5Rules"
      if (!requireNamespace("RWeka", quietly = TRUE)) {
        showNotification(paste("Package", "RWeka", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- caret::train(getM5RulesRecipe(), data = getTrainData(), method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.pass)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$M5Rules_Load, {
      method <- "M5Rules"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$M5Rules_Delete, { .forget_model("M5Rules") })
    
    output$M5Rules_MethodSummary <- renderText({ description("M5Rules") })
    output$M5Rules_Metrics <- renderTable({ getBestMetricRow("M5Rules") })
    output$M5Rules_ModelTune <- renderPlot({ mod <- models[["M5Rules"]]; req(mod); plot(mod) })
    output$M5Rules_RecipePrint <- renderUI({ .recipe_print_ui("M5Rules") })
    output$M5Rules_RecipeOutput <- renderTable({ .recipe_summary_table("M5Rules") })
    output$M5Rules_TrainSummary <- renderPrint({ .train_summary_print("M5Rules") })
    # METHOD * svmRadial ---------------------------------------------------------------------------------------------------------------------------
    getSvmRadialRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$svmRadial_Go, {
      method <- "svmRadial"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- caret::train(getSvmRadialRecipe(), data = getTrainData(), method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.pass)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$svmRadial_Load, {
      method <- "svmRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$svmRadial_Delete, { .forget_model("svmRadial") })
    
    output$svmRadial_MethodSummary <- renderText({ description("svmRadial") })
    output$svmRadial_Metrics <- renderTable({ getBestMetricRow("svmRadial") })
    output$svmRadial_ModelTune <- renderPlot({ mod <- models[["svmRadial"]]; req(mod); plot(mod) })
    output$svmRadial_RecipePrint <- renderUI({ .recipe_print_ui("svmRadial") })
    output$svmRadial_RecipeOutput <- renderTable({ .recipe_summary_table("svmRadial") })
    output$svmRadial_TrainSummary <- renderPrint({ .train_summary_print("svmRadial") })
    # METHOD * krlsRadial ---------------------------------------------------------------------------------------------------------------------------
    getKrlsRadialRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$krlsRadial_Go, {
      method <- "krlsRadial"
      if (!requireNamespace("KRLS", quietly = TRUE)) {
        showNotification(paste("Package", "KRLS", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getKrlsRadialRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$krlsRadial_Load, {
      method <- "krlsRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$krlsRadial_Delete, { .forget_model("krlsRadial") })
    
    output$krlsRadial_MethodSummary <- renderText({ description("krlsRadial") })
    output$krlsRadial_Metrics <- renderTable({ getBestMetricRow("krlsRadial") })
    output$krlsRadial_ModelTune <- renderPlot({ mod <- models[["krlsRadial"]]; req(mod); plot(mod) })
    output$krlsRadial_RecipePrint <- renderUI({ .recipe_print_ui("krlsRadial") })
    output$krlsRadial_RecipeOutput <- renderTable({ .recipe_summary_table("krlsRadial") })
    output$krlsRadial_TrainSummary <- renderPrint({ .train_summary_print("krlsRadial") })
    # METHOD * ranger ---------------------------------------------------------------------------------------------------------------------------
    getRangerRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$ranger_Go, {
      method <- "ranger"
      if (!requireNamespace("ranger", quietly = TRUE)) {
        showNotification(paste("Package", "ranger", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRangerRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$ranger_Load, {
      method <- "ranger"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$ranger_Delete, { .forget_model("ranger") })
    
    output$ranger_MethodSummary <- renderText({ description("ranger") })
    output$ranger_Metrics <- renderTable({ getBestMetricRow("ranger") })
    output$ranger_ModelTune <- renderPlot({ mod <- models[["ranger"]]; req(mod); plot(mod) })
    output$ranger_RecipePrint <- renderUI({ .recipe_print_ui("ranger") })
    output$ranger_RecipeOutput <- renderTable({ .recipe_summary_table("ranger") })
    output$ranger_TrainSummary <- renderPrint({ .train_summary_print("ranger") })
    # METHOD * rf ---------------------------------------------------------------------------------------------------------------------------
    getRfRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$rf_Go, {
      method <- "rf"
      if (!requireNamespace("randomForest", quietly = TRUE)) {
        showNotification(paste("Package", "randomForest", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- caret::train(getRfRecipe(), data = getTrainData(), method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.pass)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$rf_Load, {
      method <- "rf"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$rf_Delete, { .forget_model("rf") })
    
    output$rf_MethodSummary <- renderText({ description("rf") })
    output$rf_Metrics <- renderTable({ getBestMetricRow("rf") })
    output$rf_ModelTune <- renderPlot({ mod <- models[["rf"]]; req(mod); plot(mod) })
    output$rf_RecipePrint <- renderUI({ .recipe_print_ui("rf") })
    output$rf_RecipeOutput <- renderTable({ .recipe_summary_table("rf") })
    output$rf_TrainSummary <- renderPrint({ .train_summary_print("rf") })
    # METHOD * gbm ---------------------------------------------------------------------------------------------------------------------------
    getGbmRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$gbm_Go, {
      method <- "gbm"
      if (!requireNamespace("gbm", quietly = TRUE)) {
        showNotification(paste("Package", "gbm", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getGbmRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          x <- as.matrix(x)
          colnames(x) <- make.names(colnames(x), unique = TRUE)
          req(nrow(x) > 5, ncol(x) > 0, length(y) == nrow(x))
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), verbose = FALSE)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$gbm_Load, {
      method <- "gbm"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$gbm_Delete, { .forget_model("gbm") })
    
    output$gbm_MethodSummary <- renderText({ description("gbm") })
    output$gbm_Metrics <- renderTable({ getBestMetricRow("gbm") })
    output$gbm_ModelTune <- renderPlot({ mod <- models[["gbm"]]; req(mod); plot(mod) })
    output$gbm_RecipePrint <- renderUI({ .recipe_print_ui("gbm") })
    output$gbm_RecipeOutput <- renderTable({ .recipe_summary_table("gbm") })
    output$gbm_TrainSummary <- renderPrint({ .train_summary_print("gbm") })
    # METHOD * xgbTree ---------------------------------------------------------------------------------------------------------------------------
    getXgbTreeRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$xgbTree_Go, {
      method <- "xgbTree"
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        showNotification(paste("Package", "xgboost", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getXgbTreeRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          x <- as.matrix(x)
          colnames(x) <- make.names(colnames(x), unique = TRUE)
          x[!is.finite(x)] <- NA_real_
          x[is.na(x)] <- 0
          req(nrow(x) > 5, ncol(x) > 0, length(y) == nrow(x))
          ctrl <- getTrControl()
          ctrl$allowParallel <- FALSE
          tune_grid <- expand.grid(
            nrounds = c(400, 800, 1200),
            max_depth = c(2, 3, 4),
            eta = c(0.03, 0.05, 0.10),
            gamma = 0,
            colsample_bytree = 0.8,
            min_child_weight = 1,
            subsample = 0.8
          )
          xgb_tree_model <- list(
            label = "eXtreme Gradient Boosting",
            library = "xgboost",
            type = "Regression",
            parameters = data.frame(
              parameter = c("nrounds", "max_depth", "eta", "gamma", "colsample_bytree", "min_child_weight", "subsample"),
              class = rep("numeric", 7),
              label = c("# Boosting Iterations", "Max Tree Depth", "Shrinkage", "Minimum Loss Reduction",
                        "Subsample Ratio of Columns", "Minimum Sum of Instance Weight", "Subsample Percentage"),
              stringsAsFactors = FALSE
            ),
            grid = function(x, y, len = NULL, search = "grid") tune_grid,
            fit = function(x, y, wts, param, lev, last, classProbs, ...) {
              dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x), label = y)
              params <- list(
                objective = "reg:squarederror",
                eta = param$eta,
                max_depth = as.integer(param$max_depth),
                gamma = param$gamma,
                colsample_bytree = param$colsample_bytree,
                min_child_weight = param$min_child_weight,
                subsample = param$subsample,
                nthread = 1
              )
              booster <- xgboost::xgb.train(
                params = params,
                data = dtrain,
                nrounds = as.integer(param$nrounds),
                verbose = 0
              )
              list(booster = booster, xNames = colnames(x))
            },
            predict = function(modelFit, newdata, submodels = NULL) {
              dtest <- xgboost::xgb.DMatrix(data = as.matrix(newdata))
              predict(modelFit$booster, dtest)
            },
            prob = NULL,
            sort = function(x) x[order(x$nrounds, x$max_depth, x$eta), ],
            levels = function(x) NULL
          )
          model <- caret::train(x = x, y = y, method = xgb_tree_model,
                                metric = "RMSE", trControl = ctrl,
                                tuneGrid = tune_grid)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$xgbTree_Load, {
      method <- "xgbTree"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$xgbTree_Delete, { .forget_model("xgbTree") })
    
    output$xgbTree_MethodSummary <- renderText({ description("xgbTree") })
    output$xgbTree_Metrics <- renderTable({ getBestMetricRow("xgbTree") })
    output$xgbTree_ModelTune <- renderPlot({
      mod <- models[["xgbTree"]]
      req(mod)
      d <- mod$results
      req(nrow(d) > 0)
      d$Depth <- factor(d$max_depth)
      d$Eta <- factor(d$eta)
      ggplot(d, aes(x = nrounds, y = RMSE, color = Depth, group = interaction(Depth, Eta))) +
        geom_line() +
        geom_point(size = 2) +
        facet_wrap(~ Eta, labeller = label_both) +
        labs(title = "xgbTree tuning", x = "Boosting rounds", y = "RMSE", color = "Max depth") +
        theme_minimal(base_size = 13)
    })
    output$xgbTree_RecipePrint <- renderUI({ .recipe_print_ui("xgbTree") })
    output$xgbTree_RecipeOutput <- renderTable({ .recipe_summary_table("xgbTree") })
    output$xgbTree_TrainSummary <- renderPrint({ .train_summary_print("xgbTree") })
    # METHOD * bagEarth ---------------------------------------------------------------------------------------------------------------------------
    getBagEarthRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$bagEarth_Go, {
      method <- "bagEarth"
      if (!requireNamespace("earth", quietly = TRUE)) {
        showNotification(paste("Package", "earth", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- caret::train(getBagEarthRecipe(), data = getTrainData(), method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.fail)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$bagEarth_Load, {
      method <- "bagEarth"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$bagEarth_Delete, { .forget_model("bagEarth") })
    
    output$bagEarth_MethodSummary <- renderText({ description("bagEarth") })
    output$bagEarth_Metrics <- renderTable({ getBestMetricRow("bagEarth") })
    output$bagEarth_ModelTune <- renderPlot({ mod <- models[["bagEarth"]]; req(mod); plot(mod) })
    output$bagEarth_RecipePrint <- renderUI({ .recipe_print_ui("bagEarth") })
    output$bagEarth_RecipeOutput <- renderTable({ .recipe_summary_table("bagEarth") })
    output$bagEarth_TrainSummary <- renderPrint({ .train_summary_print("bagEarth") })
    # METHOD * earth ---------------------------------------------------------------------------------------------------------------------------
    getEarthRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    observeEvent(input$earth_Go, {
      method <- "earth"
      if (!requireNamespace("earth", quietly = TRUE)) {
        showNotification(paste("Package", "earth", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          model <- caret::train(getEarthRecipe(), data = getTrainData(), method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.fail)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    
    observeEvent(input$earth_Load, {
      method <- "earth"
      model <- loadRds(method, session)
      if (!is.null(model)) models[[method]] <- model
    })
    
    observeEvent(input$earth_Delete, { .forget_model("earth") })
    
    output$earth_MethodSummary <- renderText({ description("earth") })
    output$earth_Metrics <- renderTable({ getBestMetricRow("earth") })
    output$earth_ModelTune <- renderPlot({ mod <- models[["earth"]]; req(mod); plot(mod) })
    output$earth_RecipePrint <- renderUI({ .recipe_print_ui("earth") })
    output$earth_RecipeOutput <- renderTable({ .recipe_summary_table("earth") })
    output$earth_TrainSummary <- renderPrint({ .train_summary_print("earth") })
    # METHOD * lmStepAIC ---------------------------------------------------------------------------------------------------------------------------
    getLmStepAICRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$lmStepAIC_Go, {
      method <- "lmStepAIC"
      if (!requireNamespace("MASS", quietly = TRUE)) {
        showNotification(paste("Package", "MASS", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getLmStepAICRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.data.frame(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$lmStepAIC_Load, { method <- "lmStepAIC"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$lmStepAIC_Delete, { .forget_model("lmStepAIC") })
    output$lmStepAIC_MethodSummary <- renderText({ description("lmStepAIC") })
    output$lmStepAIC_Metrics <- renderTable({ getBestMetricRow("lmStepAIC") })
    output$lmStepAIC_ModelTune <- renderPlot({ mod <- models[["lmStepAIC"]]; req(mod); plot(mod) })
    output$lmStepAIC_RecipePrint <- renderUI({ .recipe_print_ui("lmStepAIC") })
    output$lmStepAIC_RecipeOutput <- renderTable({ .recipe_summary_table("lmStepAIC") })
    output$lmStepAIC_TrainSummary <- renderPrint({ .train_summary_print("lmStepAIC") })
        # METHOD * rlm ---------------------------------------------------------------------------------------------------------------------------
    getRlmRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$rlm_Go, {
      method <- "rlm"
      if (!requireNamespace("MASS", quietly = TRUE)) {
        showNotification(paste("Package", "MASS", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRlmRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.data.frame(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$rlm_Load, { method <- "rlm"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$rlm_Delete, { .forget_model("rlm") })
    output$rlm_MethodSummary <- renderText({ description("rlm") })
    output$rlm_Metrics <- renderTable({ getBestMetricRow("rlm") })
    output$rlm_ModelTune <- renderPlot({ mod <- models[["rlm"]]; req(mod); plot(mod) })
    output$rlm_RecipePrint <- renderUI({ .recipe_print_ui("rlm") })
    output$rlm_RecipeOutput <- renderTable({ .recipe_summary_table("rlm") })
    output$rlm_TrainSummary <- renderPrint({ .train_summary_print("rlm") })
        # METHOD * xgbLinear ---------------------------------------------------------------------------------------------------------------------------
    getXgbLinearRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$xgbLinear_Go, {
      method <- "xgbLinear"
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        showNotification(paste("Package", "xgboost", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getXgbLinearRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          x <- as.matrix(x)
          colnames(x) <- make.names(colnames(x), unique = TRUE)
          x[!is.finite(x)] <- NA_real_
          x[is.na(x)] <- 0
          req(nrow(x) > 5, ncol(x) > 0, length(y) == nrow(x))
          ctrl <- getTrControl()
          ctrl$allowParallel <- FALSE
          tune_grid <- expand.grid(
            nrounds = c(100, 400),
            lambda = c(0, 0.1, 1),
            alpha = c(0, 0.5, 1),
            eta = 0.05
          )
          xgb_linear_model <- list(
            label = "eXtreme Gradient Boosted Linear Model",
            library = "xgboost",
            type = "Regression",
            parameters = data.frame(
              parameter = c("nrounds", "lambda", "alpha", "eta"),
              class = rep("numeric", 4),
              label = c("# Boosting Iterations", "L2 Regularization", "L1 Regularization", "Shrinkage"),
              stringsAsFactors = FALSE
            ),
            grid = function(x, y, len = NULL, search = "grid") tune_grid,
            fit = function(x, y, wts, param, lev, last, classProbs, ...) {
              dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x), label = y)
              params <- list(
                booster = "gblinear",
                objective = "reg:squarederror",
                eta = param$eta,
                lambda = param$lambda,
                alpha = param$alpha,
                nthread = 1
              )
              booster <- xgboost::xgb.train(params = params, data = dtrain,
                                            nrounds = as.integer(param$nrounds), verbose = 0)
              list(booster = booster, xNames = colnames(x))
            },
            predict = function(modelFit, newdata, submodels = NULL) {
              dtest <- xgboost::xgb.DMatrix(data = as.matrix(newdata))
              predict(modelFit$booster, dtest)
            },
            prob = NULL,
            sort = function(x) x[order(x$nrounds, x$lambda, x$alpha, x$eta), ],
            levels = function(x) NULL
          )
          model <- caret::train(x = x, y = y, method = xgb_linear_model,
                                metric = "RMSE", trControl = ctrl,
                                tuneGrid = tune_grid)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$xgbLinear_Load, { method <- "xgbLinear"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$xgbLinear_Delete, { .forget_model("xgbLinear") })
    output$xgbLinear_MethodSummary <- renderText({ description("xgbLinear") })
    output$xgbLinear_Metrics <- renderTable({ getBestMetricRow("xgbLinear") })
    output$xgbLinear_ModelTune <- renderPlot({
      mod <- models[["xgbLinear"]]
      req(mod)
      d <- mod$results
      req(nrow(d) > 0)
      d$Alpha <- factor(d$alpha)
      d$Lambda <- factor(d$lambda)
      ggplot(d, aes(x = nrounds, y = RMSE, color = Alpha, group = interaction(Alpha, Lambda))) +
        geom_line() +
        geom_point(size = 2) +
        facet_wrap(~ Lambda, labeller = label_both) +
        labs(title = "xgbLinear tuning", x = "Boosting rounds", y = "RMSE", color = "Alpha") +
        theme_minimal(base_size = 13)
    })
    output$xgbLinear_RecipePrint <- renderUI({ .recipe_print_ui("xgbLinear") })
    output$xgbLinear_RecipeOutput <- renderTable({ .recipe_summary_table("xgbLinear") })
    output$xgbLinear_TrainSummary <- renderPrint({ .train_summary_print("xgbLinear") })
        # METHOD * gaussprRadial ---------------------------------------------------------------------------------------------------------------------------
    getGaussprRadialRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$gaussprRadial_Go, {
      method <- "gaussprRadial"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getGaussprRadialRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$gaussprRadial_Load, { method <- "gaussprRadial"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$gaussprRadial_Delete, { .forget_model("gaussprRadial") })
    output$gaussprRadial_MethodSummary <- renderText({ description("gaussprRadial") })
    output$gaussprRadial_Metrics <- renderTable({ getBestMetricRow("gaussprRadial") })
    output$gaussprRadial_ModelTune <- renderPlot({ mod <- models[["gaussprRadial"]]; req(mod); plot(mod) })
    output$gaussprRadial_RecipePrint <- renderUI({ .recipe_print_ui("gaussprRadial") })
    output$gaussprRadial_RecipeOutput <- renderTable({ .recipe_summary_table("gaussprRadial") })
    output$gaussprRadial_TrainSummary <- renderPrint({ .train_summary_print("gaussprRadial") })
        # METHOD * svmLinear2 ---------------------------------------------------------------------------------------------------------------------------
    getSvmLinear2Recipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$svmLinear2_Go, {
      method <- "svmLinear2"
      if (!requireNamespace("e1071", quietly = TRUE)) {
        showNotification(paste("Package", "e1071", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getSvmLinear2Recipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$svmLinear2_Load, { method <- "svmLinear2"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$svmLinear2_Delete, { .forget_model("svmLinear2") })
    output$svmLinear2_MethodSummary <- renderText({ description("svmLinear2") })
    output$svmLinear2_Metrics <- renderTable({ getBestMetricRow("svmLinear2") })
    output$svmLinear2_ModelTune <- renderPlot({ mod <- models[["svmLinear2"]]; req(mod); plot(mod) })
    output$svmLinear2_RecipePrint <- renderUI({ .recipe_print_ui("svmLinear2") })
    output$svmLinear2_RecipeOutput <- renderTable({ .recipe_summary_table("svmLinear2") })
    output$svmLinear2_TrainSummary <- renderPrint({ .train_summary_print("svmLinear2") })
        # METHOD * RRF ---------------------------------------------------------------------------------------------------------------------------
    getRRFRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$RRF_Go, {
      method <- "RRF"
      if (!requireNamespace("RRF", quietly = TRUE)) {
        showNotification(paste("Package", "RRF", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRRFRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$RRF_Load, { method <- "RRF"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$RRF_Delete, { .forget_model("RRF") })
    output$RRF_MethodSummary <- renderText({ description("RRF") })
    output$RRF_Metrics <- renderTable({ getBestMetricRow("RRF") })
    output$RRF_ModelTune <- renderPlot({ mod <- models[["RRF"]]; req(mod); plot(mod) })
    output$RRF_RecipePrint <- renderUI({ .recipe_print_ui("RRF") })
    output$RRF_RecipeOutput <- renderTable({ .recipe_summary_table("RRF") })
    output$RRF_TrainSummary <- renderPrint({ .train_summary_print("RRF") })
        # METHOD * RRFglobal ---------------------------------------------------------------------------------------------------------------------------
    getRRFglobalRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$RRFglobal_Go, {
      method <- "RRFglobal"
      if (!requireNamespace("RRF", quietly = TRUE)) {
        showNotification(paste("Package", "RRF", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRRFglobalRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$RRFglobal_Load, { method <- "RRFglobal"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$RRFglobal_Delete, { .forget_model("RRFglobal") })
    output$RRFglobal_MethodSummary <- renderText({ description("RRFglobal") })
    output$RRFglobal_Metrics <- renderTable({ getBestMetricRow("RRFglobal") })
    output$RRFglobal_ModelTune <- renderPlot({ mod <- models[["RRFglobal"]]; req(mod); plot(mod) })
    output$RRFglobal_RecipePrint <- renderUI({ .recipe_print_ui("RRFglobal") })
    output$RRFglobal_RecipeOutput <- renderTable({ .recipe_summary_table("RRFglobal") })
    output$RRFglobal_TrainSummary <- renderPrint({ .train_summary_print("RRFglobal") })
        # METHOD * evtree ---------------------------------------------------------------------------------------------------------------------------
    getEvtreeRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$evtree_Go, {
      method <- "evtree"
      if (!requireNamespace("evtree", quietly = TRUE)) {
        showNotification(paste("Package", "evtree", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getEvtreeRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.data.frame(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$evtree_Load, { method <- "evtree"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$evtree_Delete, { .forget_model("evtree") })
    output$evtree_MethodSummary <- renderText({ description("evtree") })
    output$evtree_Metrics <- renderTable({ getBestMetricRow("evtree") })
    output$evtree_ModelTune <- renderPlot({ mod <- models[["evtree"]]; req(mod); plot(mod) })
    output$evtree_RecipePrint <- renderUI({ .recipe_print_ui("evtree") })
    output$evtree_RecipeOutput <- renderTable({ .recipe_summary_table("evtree") })
    output$evtree_TrainSummary <- renderPrint({ .train_summary_print("evtree") })
        # METHOD * nnet ---------------------------------------------------------------------------------------------------------------------------
    getNnetRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$nnet_Go, {
      method <- "nnet"
      if (!requireNamespace("nnet", quietly = TRUE)) {
        showNotification(paste("Package", "nnet", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getNnetRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          y_center <- mean(y, na.rm = TRUE)
          y_scale <- stats::sd(y, na.rm = TRUE)
          req(is.finite(y_center), is.finite(y_scale), y_scale > 0)
          y_train <- as.numeric((y - y_center) / y_scale)
          model <- caret::train(x = x, y = y_train, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), trace = FALSE,
                                maxit = 500, MaxNWts = 10000)
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$outcomeCenter <- y_center
        model$outcomeScale <- y_scale
        for (metric in c("RMSE", "MAE", "RMSESD", "MAESD")) {
          if (metric %in% names(model$results)) model$results[[metric]] <- model$results[[metric]] * y_scale
          if (!is.null(model$resample) && metric %in% names(model$resample)) model$resample[[metric]] <- model$resample[[metric]] * y_scale
        }
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$nnet_Load, { method <- "nnet"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$nnet_Delete, { .forget_model("nnet") })
    output$nnet_MethodSummary <- renderText({ description("nnet") })
    output$nnet_Metrics <- renderTable({ getBestMetricRow("nnet") })
    output$nnet_ModelTune <- renderPlot({ mod <- models[["nnet"]]; req(mod); plot(mod) })
    output$nnet_RecipePrint <- renderUI({ .recipe_print_ui("nnet") })
    output$nnet_RecipeOutput <- renderTable({ .recipe_summary_table("nnet") })
    output$nnet_TrainSummary <- renderPrint({ .train_summary_print("nnet") })
    # METHOD * svmRadialSigma ---------------------------------------------------------------------------------------------------------------------------
    getSvmRadialSigmaRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$svmRadialSigma_Go, {
      method <- "svmRadialSigma"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getSvmRadialSigmaRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          y_train <- y
          model <- caret::train(x = x, y = y_train, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$svmRadialSigma_Load, { method <- "svmRadialSigma"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$svmRadialSigma_Delete, { .forget_model("svmRadialSigma") })
    output$svmRadialSigma_MethodSummary <- renderText({ description("svmRadialSigma") })
    output$svmRadialSigma_Metrics <- renderTable({ getBestMetricRow("svmRadialSigma") })
    output$svmRadialSigma_ModelTune <- renderPlot({ mod <- models[["svmRadialSigma"]]; req(mod); plot(mod) })
    output$svmRadialSigma_RecipePrint <- renderUI({ .recipe_print_ui("svmRadialSigma") })
    output$svmRadialSigma_RecipeOutput <- renderTable({ .recipe_summary_table("svmRadialSigma") })
    output$svmRadialSigma_TrainSummary <- renderPrint({ .train_summary_print("svmRadialSigma") })
    # METHOD * rvmRadial ---------------------------------------------------------------------------------------------------------------------------
    getRvmRadialRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$rvmRadial_Go, {
      method <- "rvmRadial"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRvmRadialRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          y_train <- y
          model <- caret::train(x = x, y = y_train, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$rvmRadial_Load, { method <- "rvmRadial"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$rvmRadial_Delete, { .forget_model("rvmRadial") })
    output$rvmRadial_MethodSummary <- renderText({ description("rvmRadial") })
    output$rvmRadial_Metrics <- renderTable({ getBestMetricRow("rvmRadial") })
    output$rvmRadial_ModelTune <- renderPlot({ mod <- models[["rvmRadial"]]; req(mod); plot(mod) })
    output$rvmRadial_RecipePrint <- renderUI({ .recipe_print_ui("rvmRadial") })
    output$rvmRadial_RecipeOutput <- renderTable({ .recipe_summary_table("rvmRadial") })
    output$rvmRadial_TrainSummary <- renderPrint({ .train_summary_print("rvmRadial") })
    # METHOD * mlpWeightDecay ---------------------------------------------------------------------------------------------------------------------------
    getMlpWeightDecayRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$mlpWeightDecay_Go, {
      method <- "mlpWeightDecay"
      if (!requireNamespace("RSNNS", quietly = TRUE)) {
        showNotification(paste("Package", "RSNNS", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getMlpWeightDecayRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          y_center <- mean(y, na.rm = TRUE)
          y_scale <- stats::sd(y, na.rm = TRUE)
          req(is.finite(y_center), is.finite(y_scale), y_scale > 0)
          y_train <- as.numeric((y - y_center) / y_scale)
          model <- caret::train(x = x, y = y_train, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$outcomeCenter <- y_center
        model$outcomeScale <- y_scale
        for (metric in c("RMSE", "MAE", "RMSESD", "MAESD")) {
          if (metric %in% names(model$results)) model$results[[metric]] <- model$results[[metric]] * y_scale
          if (!is.null(model$resample) && metric %in% names(model$resample)) model$resample[[metric]] <- model$resample[[metric]] * y_scale
        }
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$mlpWeightDecay_Load, { method <- "mlpWeightDecay"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$mlpWeightDecay_Delete, { .forget_model("mlpWeightDecay") })
    output$mlpWeightDecay_MethodSummary <- renderText({ description("mlpWeightDecay") })
    output$mlpWeightDecay_Metrics <- renderTable({ getBestMetricRow("mlpWeightDecay") })
    output$mlpWeightDecay_ModelTune <- renderPlot({ mod <- models[["mlpWeightDecay"]]; req(mod); plot(mod) })
    output$mlpWeightDecay_RecipePrint <- renderUI({ .recipe_print_ui("mlpWeightDecay") })
    output$mlpWeightDecay_RecipeOutput <- renderTable({ .recipe_summary_table("mlpWeightDecay") })
    output$mlpWeightDecay_TrainSummary <- renderPrint({ .train_summary_print("mlpWeightDecay") })
    # METHOD * monmlp ---------------------------------------------------------------------------------------------------------------------------
    getMonmlpRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$monmlp_Go, {
      method <- "monmlp"
      if (!requireNamespace("monmlp", quietly = TRUE)) {
        showNotification(paste("Package", "monmlp", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getMonmlpRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          y_center <- mean(y, na.rm = TRUE)
          y_scale <- stats::sd(y, na.rm = TRUE)
          req(is.finite(y_center), is.finite(y_scale), y_scale > 0)
          y_train <- as.numeric((y - y_center) / y_scale)
          model <- caret::train(x = x, y = y_train, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$outcomeCenter <- y_center
        model$outcomeScale <- y_scale
        for (metric in c("RMSE", "MAE", "RMSESD", "MAESD")) {
          if (metric %in% names(model$results)) model$results[[metric]] <- model$results[[metric]] * y_scale
          if (!is.null(model$resample) && metric %in% names(model$resample)) model$resample[[metric]] <- model$resample[[metric]] * y_scale
        }
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$monmlp_Load, { method <- "monmlp"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$monmlp_Delete, { .forget_model("monmlp") })
    output$monmlp_MethodSummary <- renderText({ description("monmlp") })
    output$monmlp_Metrics <- renderTable({ getBestMetricRow("monmlp") })
    output$monmlp_ModelTune <- renderPlot({ mod <- models[["monmlp"]]; req(mod); plot(mod) })
    output$monmlp_RecipePrint <- renderUI({ .recipe_print_ui("monmlp") })
    output$monmlp_RecipeOutput <- renderTable({ .recipe_summary_table("monmlp") })
    output$monmlp_TrainSummary <- renderPrint({ .train_summary_print("monmlp") })
    # METHOD * qrnn ---------------------------------------------------------------------------------------------------------------------------
    getQrnnRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$qrnn_Go, {
      method <- "qrnn"
      if (!requireNamespace("qrnn", quietly = TRUE)) {
        showNotification(paste("Package", "qrnn", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getQrnnRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          y_center <- mean(y, na.rm = TRUE)
          y_scale <- stats::sd(y, na.rm = TRUE)
          req(is.finite(y_center), is.finite(y_scale), y_scale > 0)
          y_train <- as.numeric((y - y_center) / y_scale)
          model <- caret::train(x = x, y = y_train, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$outcomeCenter <- y_center
        model$outcomeScale <- y_scale
        for (metric in c("RMSE", "MAE", "RMSESD", "MAESD")) {
          if (metric %in% names(model$results)) model$results[[metric]] <- model$results[[metric]] * y_scale
          if (!is.null(model$resample) && metric %in% names(model$resample)) model$resample[[metric]] <- model$resample[[metric]] * y_scale
        }
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$qrnn_Load, { method <- "qrnn"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$qrnn_Delete, { .forget_model("qrnn") })
    output$qrnn_MethodSummary <- renderText({ description("qrnn") })
    output$qrnn_Metrics <- renderTable({ getBestMetricRow("qrnn") })
    output$qrnn_ModelTune <- renderPlot({ mod <- models[["qrnn"]]; req(mod); plot(mod) })
    output$qrnn_RecipePrint <- renderUI({ .recipe_print_ui("qrnn") })
    output$qrnn_RecipeOutput <- renderTable({ .recipe_summary_table("qrnn") })
    output$qrnn_TrainSummary <- renderPrint({ .train_summary_print("qrnn") })
    # METHOD * krlsPoly ---------------------------------------------------------------------------------------------------------------------------
    getKrlsPolyRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$krlsPoly_Go, {
      method <- "krlsPoly"
      if (!requireNamespace("KRLS", quietly = TRUE)) {
        showNotification(paste("Package", "KRLS", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getKrlsPolyRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$krlsPoly_Load, { method <- "krlsPoly"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$krlsPoly_Delete, { .forget_model("krlsPoly") })
    output$krlsPoly_MethodSummary <- renderText({ description("krlsPoly") })
    output$krlsPoly_Metrics <- renderTable({ getBestMetricRow("krlsPoly") })
    output$krlsPoly_ModelTune <- renderPlot({ mod <- models[["krlsPoly"]]; req(mod); plot(mod) })
    output$krlsPoly_RecipePrint <- renderUI({ .recipe_print_ui("krlsPoly") })
    output$krlsPoly_RecipeOutput <- renderTable({ .recipe_summary_table("krlsPoly") })
    output$krlsPoly_TrainSummary <- renderPrint({ .train_summary_print("krlsPoly") })
    # METHOD * kernelpls ---------------------------------------------------------------------------------------------------------------------------
    getKernelplsRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$kernelpls_Go, {
      method <- "kernelpls"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getKernelplsRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$kernelpls_Load, { method <- "kernelpls"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$kernelpls_Delete, { .forget_model("kernelpls") })
    output$kernelpls_MethodSummary <- renderText({ description("kernelpls") })
    output$kernelpls_Metrics <- renderTable({ getBestMetricRow("kernelpls") })
    output$kernelpls_ModelTune <- renderPlot({ mod <- models[["kernelpls"]]; req(mod); plot(mod) })
    output$kernelpls_RecipePrint <- renderUI({ .recipe_print_ui("kernelpls") })
    output$kernelpls_RecipeOutput <- renderTable({ .recipe_summary_table("kernelpls") })
    output$kernelpls_TrainSummary <- renderPrint({ .train_summary_print("kernelpls") })
    # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------
    getSvmLinearRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$svmLinear_Go, {
      method <- "svmLinear"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getSvmLinearRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$svmLinear_Load, { method <- "svmLinear"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$svmLinear_Delete, { .forget_model("svmLinear") })
    output$svmLinear_MethodSummary <- renderText({ description("svmLinear") })
    output$svmLinear_Metrics <- renderTable({ getBestMetricRow("svmLinear") })
    output$svmLinear_ModelTune <- renderPlot({ mod <- models[["svmLinear"]]; req(mod); plot(mod) })
    output$svmLinear_RecipePrint <- renderUI({ .recipe_print_ui("svmLinear") })
    output$svmLinear_RecipeOutput <- renderTable({ .recipe_summary_table("svmLinear") })
    output$svmLinear_TrainSummary <- renderPrint({ .train_summary_print("svmLinear") })
    # METHOD * svmLinear3 ---------------------------------------------------------------------------------------------------------------------------
    getSvmLinear3Recipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$svmLinear3_Go, {
      method <- "svmLinear3"
      if (!requireNamespace("LiblineaR", quietly = TRUE)) {
        showNotification(paste("Package", "LiblineaR", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getSvmLinear3Recipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$svmLinear3_Load, { method <- "svmLinear3"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$svmLinear3_Delete, { .forget_model("svmLinear3") })
    output$svmLinear3_MethodSummary <- renderText({ description("svmLinear3") })
    output$svmLinear3_Metrics <- renderTable({ getBestMetricRow("svmLinear3") })
    output$svmLinear3_ModelTune <- renderPlot({ mod <- models[["svmLinear3"]]; req(mod); plot(mod) })
    output$svmLinear3_RecipePrint <- renderUI({ .recipe_print_ui("svmLinear3") })
    output$svmLinear3_RecipeOutput <- renderTable({ .recipe_summary_table("svmLinear3") })
    output$svmLinear3_TrainSummary <- renderPrint({ .train_summary_print("svmLinear3") })
    # METHOD * rvmLinear ---------------------------------------------------------------------------------------------------------------------------
    getRvmLinearRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$rvmLinear_Go, {
      method <- "rvmLinear"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRvmLinearRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$rvmLinear_Load, { method <- "rvmLinear"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$rvmLinear_Delete, { .forget_model("rvmLinear") })
    output$rvmLinear_MethodSummary <- renderText({ description("rvmLinear") })
    output$rvmLinear_Metrics <- renderTable({ getBestMetricRow("rvmLinear") })
    output$rvmLinear_ModelTune <- renderPlot({ mod <- models[["rvmLinear"]]; req(mod); plot(mod) })
    output$rvmLinear_RecipePrint <- renderUI({ .recipe_print_ui("rvmLinear") })
    output$rvmLinear_RecipeOutput <- renderTable({ .recipe_summary_table("rvmLinear") })
    output$rvmLinear_TrainSummary <- renderPrint({ .train_summary_print("rvmLinear") })
    # METHOD * rvmPoly ---------------------------------------------------------------------------------------------------------------------------
    getRvmPolyRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$rvmPoly_Go, {
      method <- "rvmPoly"
      if (!requireNamespace("kernlab", quietly = TRUE)) {
        showNotification(paste("Package", "kernlab", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRvmPolyRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$rvmPoly_Load, { method <- "rvmPoly"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$rvmPoly_Delete, { .forget_model("rvmPoly") })
    output$rvmPoly_MethodSummary <- renderText({ description("rvmPoly") })
    output$rvmPoly_Metrics <- renderTable({ getBestMetricRow("rvmPoly") })
    output$rvmPoly_ModelTune <- renderPlot({ mod <- models[["rvmPoly"]]; req(mod); plot(mod) })
    output$rvmPoly_RecipePrint <- renderUI({ .recipe_print_ui("rvmPoly") })
    output$rvmPoly_RecipeOutput <- renderTable({ .recipe_summary_table("rvmPoly") })
    output$rvmPoly_TrainSummary <- renderPrint({ .train_summary_print("rvmPoly") })
    # METHOD * extraTrees ---------------------------------------------------------------------------------------------------------------------------
    getExtraTreesRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$extraTrees_Go, {
      method <- "extraTrees"
      if (!requireNamespace("extraTrees", quietly = TRUE)) {
        showNotification(paste("Package", "extraTrees", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getExtraTreesRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$extraTrees_Load, { method <- "extraTrees"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$extraTrees_Delete, { .forget_model("extraTrees") })
    output$extraTrees_MethodSummary <- renderText({ description("extraTrees") })
    output$extraTrees_Metrics <- renderTable({ getBestMetricRow("extraTrees") })
    output$extraTrees_ModelTune <- renderPlot({ mod <- models[["extraTrees"]]; req(mod); plot(mod) })
    output$extraTrees_RecipePrint <- renderUI({ .recipe_print_ui("extraTrees") })
    output$extraTrees_RecipeOutput <- renderTable({ .recipe_summary_table("extraTrees") })
    output$extraTrees_TrainSummary <- renderPrint({ .train_summary_print("extraTrees") })
    # METHOD * Rborist ---------------------------------------------------------------------------------------------------------------------------
    getRboristRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$Rborist_Go, {
      method <- "Rborist"
      if (!requireNamespace("Rborist", quietly = TRUE)) {
        showNotification(paste("Package", "Rborist", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getRboristRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.data.frame(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$Rborist_Load, { method <- "Rborist"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$Rborist_Delete, { .forget_model("Rborist") })
    output$Rborist_MethodSummary <- renderText({ description("Rborist") })
    output$Rborist_Metrics <- renderTable({ getBestMetricRow("Rborist") })
    output$Rborist_ModelTune <- renderPlot({ mod <- models[["Rborist"]]; req(mod); plot(mod) })
    output$Rborist_RecipePrint <- renderUI({ .recipe_print_ui("Rborist") })
    output$Rborist_RecipeOutput <- renderTable({ .recipe_summary_table("Rborist") })
    output$Rborist_TrainSummary <- renderPrint({ .train_summary_print("Rborist") })
        # METHOD * ppr ---------------------------------------------------------------------------------------------------------------------------
    getPprRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$ppr_Go, {
      method <- "ppr"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getPprRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$ppr_Load, { method <- "ppr"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$ppr_Delete, { .forget_model("ppr") })
    output$ppr_MethodSummary <- renderText({ description("ppr") })
    output$ppr_Metrics <- renderTable({ getBestMetricRow("ppr") })
    output$ppr_ModelTune <- renderPlot({ mod <- models[["ppr"]]; req(mod); plot(mod) })
    output$ppr_RecipePrint <- renderUI({ .recipe_print_ui("ppr") })
    output$ppr_RecipeOutput <- renderTable({ .recipe_summary_table("ppr") })
    output$ppr_TrainSummary <- renderPrint({ .train_summary_print("ppr") })
        # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
    library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
    
    # reactive getPlsRecipe ----
    getPlsRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    
    
    # observe GO event ----
    observeEvent(
      input$pls_Go,
      {
        method <- "pls"
        models[[method]] <- NULL
        showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
        obj <- startMode(input$Parallel)
        tryCatch({
          timing <- system.time({
          model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                                tuneLength = getTuneLength(), na.action = na.pass)
          })
          training_times[[method]] <- timing[["elapsed"]]
          model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
          deleteRds(method)
          saveToRds(model, method)
          models[[method]] <- model
        }, 
        finally = {
          removeNotification(id = method)
          stopMode(obj)
        })
      }
    )
    
    observeEvent(
      input$pls_Load,
      {
        method  <- "pls"
        model <- loadRds(method, session)
        if (!is.null(model)) {
          models[[method]] <- model
        }
      }
    )
    
    observeEvent(
      input$pls_Delete,
      {
        method <- "pls"
        models[[method]] <- NULL
        gc()
      }
    )
    
    # output method summary text ----
    output$pls_MethodSummary <- renderText({
      method <- "pls"
      description(method)
    })
    
    # output resampling metrics table ----
    output$pls_Metrics <- renderTable({
      getBestMetricRow("pls")
    })
    
    # output hyperparameter tuning chart ----
    output$pls_ModelTune <- renderPlot({
      method <- "pls"
      mod <- models[[method]]
      req(mod)
      plot(mod)
    })     
    
    # output an html formatted recipe "print" ----
    output$pls_RecipePrint <- renderUI({
      method <- "pls"
      mod <- models[[method]]
      req(mod)
      html <- mod$recipe %>%
        print() %>%
        cli::cli_fmt() %>%
        cli::ansi_collapse(sep="<br>", last = "<br>") %>%
        cli::ansi_html(escape_reserved = FALSE) %>%
        gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
      css <- paste(format(ansi_html_style()), collapse= "\n")
      tagList(
        tags$head(tags$style(css)),
        tags$pre(HTML(html))
      )
    })
    
    # output the recipe-output table ----
    output$pls_RecipeOutput <- renderTable({
      method <- "pls"
      mod <- models[[method]]
      req(mod)
      terms <- as.data.frame(mod$recipe$term_info)
      n <- dim(terms)[1]
      types <- vector(mode="character", length=n)
      for (row in 1:n) {
        types[row] <- paste(collapse = " ", unlist(terms$type[row]))
      }
      terms$type <- types
      terms |>
        dplyr::filter(role == "predictor") |>
        dplyr::select(type, source) |>
        dplyr::group_by(type, source) |>
        dplyr::summarise(count = n())
    })  
    
    # output the training summary print ----
    output$pls_TrainSummary <- renderPrint({
      method <- "pls"
      mod <- models[[method]]
      req(mod)
      elapsed <- if (!is.null(training_times[[method]])) {
        training_times[[method]]
      } else {
        mod$trainingTimeSeconds
      }
      if (!is.null(elapsed)) {
        cat("Training time:", round(elapsed, 2), "seconds\n\n")
      }
      print(mod)
    })
    
    # output coefficients table ----
    output$pls_Coef <- renderTable({
      req(models$pls)
      co <- coef(models$pls$finalModel)
      as.data.frame(co, row.names = rownames(co))
    }, rownames = TRUE, colnames = FALSE)
    
    
    # METHOD * spls ---------------------------------------------------------------------------------------------------------------------------
    getSplsRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date")) %>%
        step_zv(all_predictors()) %>%
        step_nzv(all_predictors())
    })
    
    observeEvent(input$spls_Go, {
      method <- "spls"
      if (!requireNamespace("spls", quietly = TRUE)) {
        showNotification(paste("Package", "spls", "is required before training", method, "."),
                         type = "error", duration = 6)
        return(NULL)
      }
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using baked numeric predictors"), session = session, duration = NULL)
      obj <- startMode(input$Parallel)
      tryCatch({
        timing <- system.time({
          set.seed(getTrainSeed())
          rec <- getSplsRecipe()
          prep_rec <- recipes::prep(rec, training = getTrainData(), retain = TRUE)
          baked <- recipes::bake(prep_rec, new_data = NULL)
          y <- baked$Response
          x <- baked[, setdiff(names(baked), "Response"), drop = FALSE]
          x <- x[, sapply(x, is.numeric), drop = FALSE]
          ok <- complete.cases(x, y)
          x <- as.matrix(x[ok, , drop = FALSE])
          y <- y[ok]
          req(nrow(x) > 5, ncol(x) > 0)
          model <- caret::train(x = x, y = y, method = method,
                                metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength())
        })
        training_times[[method]] <- timing[["elapsed"]]
        model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
        model$recipe <- prep_rec
        model$preppedRecipe <- prep_rec
        model$bakedFeatureNames <- colnames(x)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(obj)
      })
    })
    observeEvent(input$spls_Load, { method <- "spls"; model <- loadRds(method, session); if (!is.null(model)) models[[method]] <- model })
    observeEvent(input$spls_Delete, { .forget_model("spls") })
    output$spls_MethodSummary <- renderText({ description("spls") })
    output$spls_Metrics <- renderTable({ getBestMetricRow("spls") })
    output$spls_ModelTune <- renderPlot({ mod <- models[["spls"]]; req(mod); plot(mod) })
    output$spls_RecipePrint <- renderUI({ .recipe_print_ui("spls") })
    output$spls_RecipeOutput <- renderTable({ .recipe_summary_table("spls") })
    output$spls_TrainSummary <- renderPrint({ .train_summary_print("spls") })
        # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
    library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
    library(rpart.plot)
    
    # reactive getRpartRecipe ----
    getRpartRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(getSelectedPreprocess(), getPreprocessConfig()) %>%
        step_rm(has_type("date"))
    })
    
    
    
    # observe the GO event -----
    observeEvent(
      input$rpart_Go,
      {
        method <- "rpart"
        models[[method]] <- NULL
        showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
        obj <- startMode(input$Parallel)
        tryCatch({
          timing <- system.time({
          model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                                tuneLength = getTuneLength(), na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
          })
          training_times[[method]] <- timing[["elapsed"]]
          model$trainingTimeSeconds <- round(timing[["elapsed"]], 2)
          deleteRds(method)
          saveToRds(model, method)
          models[[method]] <- model
        }, 
        finally = {
          removeNotification(id = method)
          stopMode(obj)
        })
      }
    )
    
    observeEvent(
      input$rpart_Load,
      {
        method  <- "rpart"
        model <- loadRds(method, session)
        if (!is.null(model)) {
          models[[method]] <- model
        }
      }
    )
    
    observeEvent(
      input$rpart_Delete,
      {
        method <- "rpart"
        models[[method]] <- NULL
        gc()
      }
    )
    
    # output the method summary text ----
    output$rpart_MethodSummary <- renderText({
      method <- "rpart"
      description(method)
    })
    
    # output the resampling metrics table ----
    output$rpart_Metrics <- renderTable({
      getBestMetricRow("rpart")
    })
    
    # output recipe-outputs table ----
    output$rpart_RecipeOutput <- renderTable({
      method <- "rpart"
      mod <- models[[method]]
      req(mod)
      terms <- as.data.frame(mod$recipe$term_info)
      n <- dim(terms)[1]
      types <- vector(mode="character", length=n)
      for (row in 1:n) {
        types[row] <- paste(collapse = " ", unlist(terms$type[row]))
      }
      terms$type <- types
      terms |>
        dplyr::filter(role == "predictor") |>
        dplyr::select(type, source) |>
        dplyr::group_by(type, source) |>
        dplyr::summarise(count = n())
    })  
    
    # output hyperparameter tuning chart ----
    output$rpart_ModelTune <- renderPlot({
      method <- "rpart"
      mod <- models[[method]]
      req(mod)
      plot(mod)
    })
    
    # output a model tree-chart ----
    output$rpart_ModelTree <- renderPlot({
      method <- "rpart"
      mod <- models[[method]]
      req(mod)
      rpart.plot::rpart.plot(mod$finalModel, roundint = FALSE)
    })     
    
    # output an html formatted recipe print ----
    output$rpart_RecipePrint <- renderUI({
      method <- "rpart"
      mod <- models[[method]]
      req(mod)
      html <- mod$recipe %>%
        print() %>%
        cli::cli_fmt() %>%
        cli::ansi_collapse(sep="<br>", last = "<br>") %>%
        cli::ansi_html(escape_reserved = FALSE) %>%
        gsub(pattern = "──────", replacement = "─",  x = ., fixed = TRUE)
      css <- paste(format(ansi_html_style()), collapse= "\n")
      tagList(
        tags$head(tags$style(css)),
        tags$pre(HTML(html))
      )
      
    })
    
    # output a training summary print ----
    output$rpart_TrainSummary <- renderPrint({
      method <- "rpart"
      mod <- models[[method]]
      req(mod)
      elapsed <- if (!is.null(training_times[[method]])) {
        training_times[[method]]
      } else {
        mod$trainingTimeSeconds
      }
      if (!is.null(elapsed)) {
        cat("Training time:", round(elapsed, 2), "seconds\n\n")
      }
      print(mod)
    })
    
    # maintenance point ---------------------------------------------------------------------------------------------------------------------------
    # Add further methods here.  You have the pls, glmnet and rpart templates to paste here - each has different layout 
    # and plotting characteristics so choose a good one and/or change the code more substantially
    
    
    
    # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
    
    
    
  }
# )





# =================================================================================
# Run
# =================================================================================

shinyApp(ui = ui, server = server)






































