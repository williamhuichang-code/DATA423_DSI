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

# default preprocessing selections shown in each method's dropdown (make sure to set these to my best recommendation)
glmnet_initial <- c("naomit", "month", "dummy")
pls_initial <- c("impute_knn", "dow", "dummy")
rpart_initial <- c("dow", "month")
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

ppchoices <- c("impute_knn", "impute_bag", "impute_median", "impute_mode", "YeoJohnson", "naomit", 
               "pca", "pls", "ica", "center", "scale", "month", "dow", "dateDecimal", "nzv", "zv", "other", 
               "dummy", "poly", "interact", "indicate_na", "corr")

# This function turns the method's selected preprocessing into a recipe that honours the same order. 
# You are allowed to add more recipe steps to this.
dynamicSteps <- function(recipe, preprocess) {
  if (is.null(preprocess)) {
    stop("The preprocess list is NULL - check that you are using the correct control identifier")
  }
  for (s in preprocess) {
    if (s == "impute_knn") {
      recipe <- step_impute_knn(recipe, all_numeric_predictors(), all_nominal_predictors(), neighbors = 5) # 5 is a reasonable guess
    } else if (s == "impute_bag") {
      recipe <- step_impute_bag(recipe, all_numeric_predictors(), all_nominal_predictors(), trees = 25) # 25 is a reasonable guess
    } else if (s == "impute_median") {
      recipe <- step_impute_median(recipe, all_numeric_predictors())  # use with "impute_mode"
    } else if (s == "impute_mode") {
      recipe <- recipes::step_impute_mode(recipe, all_nominal_predictors())  # use with "impute_median"
    } else if (s == "YeoJohnson") {
      recipe <- recipes::step_YeoJohnson(recipe, all_numeric_predictors()) 
    } else if (s == "naomit") {
      recipe <- recipes::step_naomit(recipe, all_predictors(), skip = TRUE)  
    } else if (s == "pca") {
      recipe <- recipes::step_pca(recipe, all_numeric_predictors(), num_comp = 25) # 25 is a big enough guess
    } else if (s == "pls") {
      recipe <- recipes::step_pls(recipe, all_numeric_predictors(), outcome = "Response", num_comp = 25) # 25 is a big enough guess
    } else if (s == "ica") {
      recipe <- recipes::step_ica(recipe, all_numeric_predictors(), num_comp = 25) # 25 is a big enough guess
    } else if (s == "center") {
      recipe <- recipes::step_center(recipe, all_numeric_predictors()) # this needs to be after any reshaping
    } else if (s == "scale") {
      recipe <- recipes::step_scale(recipe, all_numeric_predictors())
    } else if (s == "month") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("month"), ordinal = FALSE)  # uses step_date to generate month-of-year
    } else if (s == "dow") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("dow"), ordinal = FALSE)   # uses step_date to generate day-of-week
    } else if (s == "dateDecimal") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)   # uses step_date to generate decimal date
    } else if (s == "zv") {
      recipe <- recipes::step_zv(recipe, all_predictors())
    } else if (s == "nzv") {
      recipe <- recipes::step_nzv(recipe, all_predictors(), freq_cut = 95/5, unique_cut = 10)
    } else if (s == "other") {
      recipe <- recipes::step_other(recipe, all_nominal_predictors())
    } else if (s == "dummy") {
      recipe <- recipes::step_dummy(recipe, all_nominal_predictors(), one_hot = FALSE) # this needs to follow dealing with missing values
    } else if (s == "poly") {
      recipe <- recipes::step_poly(recipe, all_numeric_predictors(), degree = 2)
    } else if (s == "interact") {
      recipe <- recipes::step_interact(recipe, terms = ~ all_numeric_predictors():all_numeric_predictors())  # only numeric predictors allowed (this needs to follow dealing with categorical variables)
    } else if (s == "corr") {
      recipe <- recipes::step_corr(recipe, all_numeric_predictors(), threshold = 0.9)
    } else if (s == "indicate_na") {
      recipe <- recipes::step_indicate_na(recipe, all_predictors()) #shadow variables (this needs to precede dealing with NA)
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





# =================================================================================
# ui.R
# =================================================================================

# shinyUI(fluidPage(
ui <- fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - William Hui Chang (69051925)"),
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
                              tags$label("Random seed", style="font-weight:600;font-size:13px;color:#343a40;"),
                              numericInput("p1_seed", label=NULL, value=199, min=1),
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
                              tags$label("Random seed", style="font-weight:600;font-size:13px;color:#343a40;"),
                              numericInput("p2_seed", label=NULL, value=199, min=1),
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
                              tags$label("Random seed", style="font-weight:600;font-size:13px;color:#343a40;"),
                              numericInput("p3_seed", label=NULL, value=199, min=1),
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
                              tags$label("Random seed", style="font-weight:600;font-size:13px;color:#343a40;"),
                              numericInput("p4_seed", label=NULL, value=199, min=1),
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
                   div(style="font-size:13px;color:#343a40;background-color:white;padding:10px;border-left:4px solid #0d6efd;border-radius:6px;margin-bottom:12px;",
                       icon("info-circle", style="color:#0d6efd;"), HTML("&nbsp;"),
                       HTML("<strong>How to use:</strong><br>Filter methods using the controls below. The table updates instantly. Switch to the map tab to see where filtered methods sit relative to all others.")),
                   tags$label("Method type", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   radioButtons("av_type", label=NULL,
                                choices=c("Regression only"="reg", "Classification only"="cls", "Both"="both"),
                                selected="reg"),
                   hr(),
                   tags$label("Include tags (ALL must match)", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   selectizeInput("av_flt_include", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Ensemble Model")),
                   tags$label("Exclude tags (ANY disqualifies)", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   selectizeInput("av_flt_exclude", label=NULL, choices=NULL, multiple=TRUE,
                                  options=list(placeholder="e.g. Linear Regression")),
                   hr(),
                   tags$label("Handle missing predictors", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   checkboxInput("av_flt_missing", label=NULL, value=FALSE),
                   tags$label("Implicit feature selection", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   checkboxInput("av_flt_implicit", label=NULL, value=FALSE),
                   hr(),
                   tags$label("Map distance metric", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   selectInput("av_map_dist", label=NULL,
                               choices=c("euclidean","manhattan","binary","canberra"),
                               selected="manhattan"),
                   tags$label("Label size", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   sliderInput("av_map_label_size", label=NULL, min=1, max=5, value=5, step=0.5, width="100%"),
                   tags$label("Max label overlaps", style="font-weight:600;font-size:13px;color:#343a40;display:block;margin-bottom:4px;margin-top:10px;"),
                   sliderInput("av_map_overlaps", label=NULL, min=10, max=100, value=50, step=5, width="100%"),
                   hr(),
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
                              h4("Filtered caret methods", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              shinycssloaders::withSpinner(DT::dataTableOutput("av_method_table"))
                     ),
                     tabPanel("Method map",
                              br(),
                              h4("Similarity map — filtered methods highlighted in purple", style="border-left:3px solid #534AB7;padding-left:8px;font-size:14px;margin-top:16px;margin-bottom:8px;"),
                              shinycssloaders::withSpinner(plotOutput("av_map_plot", height="650px"))
                     )
                   )
                 )
               )
             )
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = paste("This will utilise all", detectCores(), "available CPUs during training")),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_MethodSummary"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  h3("Hyperparameter Tuning:"),
                                  plotOutput(outputId = "glmnet_ModelTune"),
                                  hr(),
                                  h3("Recipe:"),
                                  htmlOutput(outputId = "glmnet_RecipePrint"),
                                  h3("Outputs"),
                                  tableOutput(outputId = "glmnet_RecipeOutput"),
                                  
                                  fluidRow(
                                    column(width=6,
                                           h3("Training Summary:"),
                                           verbatimTextOutput(outputId = "glmnet_TrainSummary")
                                    ),
                                    column(width=6,
                                           h3("Coefficients"),   # Not all method can produce coefficients
                                           wellPanel(
                                             tableOutput(outputId = "glmnet_Coef")
                                           )
                                    )
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_MethodSummary"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  h3("Hyperparameter Tuning:"),
                                  plotOutput(outputId = "pls_ModelTune"),
                                  hr(),
                                  h3("Recipe:"),
                                  htmlOutput(outputId = "pls_RecipePrint"),
                                  h3("Outputs"),
                                  tableOutput(outputId = "pls_RecipeOutput"),
                                  fluidRow(
                                    column(width=6,
                                           h3("Training Summary:"),
                                           verbatimTextOutput(outputId = "pls_TrainSummary"),
                                    ),
                                    column(width=6,
                                           h3("Coefficients"),   # Not all method can produce coefficients
                                           wellPanel(
                                             tableOutput(outputId = "pls_Coef")
                                           )
                                    )
                                  )
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_MethodSummary"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-arrow-up")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-can")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  h3("Hyperparameter Tuning:"),
                                  plotOutput(outputId = "rpart_ModelTune"),
                                  hr(),
                                  h3("Model tree:"), #  <- this tree-plot is unique to the rpart method
                                  plotOutput(outputId = "rpart_ModelTree"),
                                  hr(),
                                  h3("Recipe:"),
                                  htmlOutput(outputId = "rpart_RecipePrint"),
                                  h3("Outputs"),
                                  tableOutput(outputId = "rpart_RecipeOutput"),
                                  fluidRow(
                                    column(width=6,
                                           h3("Training Summary:"),
                                           verbatimTextOutput(outputId = "rpart_TrainSummary")
                                    )
                                  )
                         )
                         
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         
                         
                         
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
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
      set.seed(input$p1_seed)
      base::sample(x = nrow(df), size = floor(input$p1_train * nrow(df)), replace = FALSE)
    })
    
    p1_strat <- reactive({
      df <- getData()
      req(input$p1_y %in% names(df))
      set.seed(input$p1_seed)
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
      set.seed(input$p2_seed)
      train_idx <- caret::createDataPartition(y = df[[input$p2_y]], p = input$p2_train, list = FALSE)
      remainder <- df[-train_idx, ]
      set.seed(input$p2_seed + 1)
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
      set.seed(input$p3_seed)
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
      set.seed(input$p4_seed)
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
      set.seed(199)
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
      set.seed(199)
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
        Hyperparams[i]    <- paste(paste0(d$parameter, " - ", d$label, " [", d$class, "]"), collapse = "\n")
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
              require(l, warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE))
          )
          check   <- ifelse(present, "", as.character(icon("ban")))
          html[i] <- paste(paste(libs, check), collapse = "<br/>")
        }
      }
      data.frame(Model = df$Model, Packages_html = html, stringsAsFactors = FALSE)
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
    
    observe({
      all_tags <- sort(unique(unlist(lapply(av_mi, `[[`, "tags"))))
      all_tags <- all_tags[nchar(all_tags) > 0]
      updateSelectizeInput(session, "av_flt_include", choices = all_tags, server = TRUE)
      updateSelectizeInput(session, "av_flt_exclude", choices = all_tags, server = TRUE)
    })
    
    av_filtered_df <- reactive({
      df <- av_methods_plain()
      df <- switch(input$av_type,
                   "reg"  = df[df$Regression,     ],
                   "cls"  = df[df$Classification, ],
                   "both" = df
      )
      for (tag in input$av_flt_include)
        df <- df[grepl(tag, df$Tags_plain, ignore.case = TRUE), ]
      for (tag in input$av_flt_exclude)
        df <- df[!grepl(tag, df$Tags_plain, ignore.case = TRUE), ]
      if (isTRUE(input$av_flt_missing))
        df <- df[grepl("Handle Missing Predictor Data", df$Tags_plain, ignore.case = TRUE), ]
      if (isTRUE(input$av_flt_implicit))
        df <- df[grepl("Implicit Feature Selection", df$Tags_plain, ignore.case = TRUE), ]
      df
    })
    
    output$av_filter_summary <- renderPrint({
      df <- av_filtered_df()
      cat(nrow(df), "method(s)\n")
      if (nrow(df) > 0 && nrow(df) <= 30)
        cat(paste(sort(df$Model), collapse = ", "))
    })
    
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
        stringsAsFactors = FALSE
      )
      DT::datatable(display, escape = FALSE, rownames = FALSE, selection = "none",
                    options = list(pageLength = 10, lengthMenu = c(5,10,25,50), scrollX = TRUE))
    })
    
    output$av_map_plot <- renderPlot({
      wide <- av_wide_matrix()
      wide_sub <- switch(input$av_type,
                         "reg"  = wide[wide$Regression == 1,     ],
                         "cls"  = wide[wide$Classification == 1, ],
                         "both" = wide
      )
      req(nrow(wide_sub) >= 3)
      d  <- stats::dist(wide_sub, method = input$av_map_dist)
      dd <- stats::cmdscale(d, k = 2)
      df_map <- data.frame(Model = rownames(dd), X1 = dd[,1], X2 = dd[,2],
                           stringsAsFactors = FALSE)
      df_map$Highlight <- df_map$Model %in% av_filtered_df()$Model
      df_hi <- df_map[df_map$Highlight, ]
      df_lo <- df_map[!df_map$Highlight, ]
      type_label <- switch(input$av_type, "reg"="Regression", "cls"="Classification", "both"="All")
      p <- ggplot(mapping = aes(x = X1, y = X2, label = Model)) +
        ggtitle(paste(type_label, "Methods — purple = filtered, grey = all others")) +
        xlab("Coordinate 1") + ylab("Coordinate 2") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
      if (nrow(df_lo) > 0)
        p <- p +
        geom_point(data = df_lo, color = "#cccccc", size = 1.5) +
        ggrepel::geom_text_repel(data = df_lo, size = input$av_map_label_size,
                                 color = "#aaaaaa", max.overlaps = input$av_map_overlaps, na.rm = TRUE)
      if (nrow(df_hi) > 0)
        p <- p +
        geom_point(data = df_hi, color = "#534AB7", size = 3) +
        ggrepel::geom_text_repel(data = df_hi, size = input$av_map_label_size + 1.5,
                                 color = "#534AB7", fontface = "bold",
                                 max.overlaps = input$av_map_overlaps,
                                 box.padding = 0.4, na.rm = TRUE)
      p
    })
    
    
    # reactive getTrainData ----
    getTrainData <- reactive({
      getData()[getSplit(),]
    })
    
    # reactive getTestData ----
    getTestData <- reactive({
      getData()[-getSplit(),]
    })
    
    # reactive getTrControl ----
    getTrControl <- reactive({
      # shared bootstrap specification i.e. 25 x bootstrap
      y <- getTrainData()[,"Response"]
      n <- 25
      set.seed(673)
      seeds <- vector(mode = "list", length = n + 1)
      for (i in 1:n) {
        seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
      }
      seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
      trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                   index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                   trim = TRUE)
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
      bwplot(mod, notch = input$Notch)
    })
    
    # output Title (UI) ----
    output$Title <- renderUI({
      tags$h3(paste("Unseen data results for chosen model:", input$Choice))
    })
    
    # reactive getTestResults ----
    getTestResults <- reactive({
      dat <- getTestData()
      req(input$Choice)
      mod <- models[[input$Choice]]
      predictions <- predict(mod, newdata = dat)
      d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
      colnames(d) <- c("obs", "pred")
      d
    })
    
    # reactive getTrainResults ----
    getTrainResults <- reactive({
      dat <- getTrainData()
      req(input$Choice)
      mod <- models[[input$Choice]]
      predictions <- predict(mod, newdata = dat)
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
          model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
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
      method <- "null"
      mod <- models[[method]]
      req(mod)
      mod$results[ which.min(mod$results[, "RMSE"]), ]
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
        dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
        step_rm(has_type("date"))   # remove original date variables
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
          model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
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
      method <- "glmnet"
      mod <- models[[method]]
      req(mod)
      mod$results[ which.min(mod$results[, "RMSE"]), ]
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
      print(mod)
    })
    
    # output coefficient print ----
    output$glmnet_Coef <- renderTable({
      req(models$glmnet)
      co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
      df <- as.data.frame(co, row.names = rownames(co))
      df[df$s1 != 0.000, ,drop=FALSE]
    }, rownames = TRUE, colnames = FALSE)
    
    
    
    # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
    library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
    
    # reactive getPlsRecipe ----
    getPlsRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
        step_rm(has_type("date"))   # remove original date variables
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
          model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                                tuneLength = 25, na.action = na.pass)
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
      method <- "pls"
      mod <- models[[method]]
      req(mod)
      mod$results[ which.min(mod$results[, "RMSE"]), ]
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
      print(mod)
    })
    
    # output coefficients table ----
    output$pls_Coef <- renderTable({
      req(models$pls)
      co <- coef(models$pls$finalModel)
      as.data.frame(co, row.names = rownames(co))
    }, rownames = TRUE, colnames = FALSE)
    
    
    # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
    library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
    library(rpart.plot)
    
    # reactive getRpartRecipe ----
    getRpartRecipe <- reactive({
      form <- formula(Response ~ .)
      recipes::recipe(form, data = getTrainData()) %>%
        dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
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
          model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                                tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
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
      method <- "rpart"
      mod <- models[[method]]
      req(mod)
      mod$results[ which.min(mod$results[, "RMSE"]), ]
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
