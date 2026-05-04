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


# ── MODULE LOADING LOGIC ─────────────────────────────────────────────────────

# look inside "A3_modules" folder and its subs, load all files with .R according to their full paths
list.files("A3_modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)


# ── CONFIGS ──────────────────────────────────────────────────────────────────

# sets R to display numbers with 3 significant digits globally
options(digits = 3)

# default preprocessing selections shown in each method's dropdown (make sure to set these to my best recommendation)
glmnet_initial <- c("naomit", "month", "dummy") 
pls_initial <- c("impute_knn", "dow", "dummy")
rpart_initial <- c("dow", "month")
# add further preprocessing choices for the new methods here


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

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}


# attempts to keep the model file size small by not saving the global environment with each model
saveToRds <- function(model, name) {
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
    
    # ── UI DATA ───────────────────────────────────────────────────────────────
    
    tabPanel("Data",
             mainPanel(width = 12,
                       tabsetPanel(id = "DataTabs",
                                   tabPanel("Summary",
                                            eda_summary_ui("eda_summary")
                                   ),
                                   tabPanel("Boxplots",
                                            eda_boxplot_ui("eda_boxplot")
                                   ),
                                   tabPanel("Missing",
                                            eda_vis_ui("eda_vis")
                                   ),
                                   tabPanel("Correlation",
                                            eda_heatmap_ui("eda_heatmap")
                                   ),
                                   tabPanel("Table",
                                            eda_datatable_ui("eda_datatable")
                                   )
                       )
             )
    ),
    
    
    # ── UI SPLIT ──────────────────────────────────────────────────────────────
    
    tabPanel("Split",
             split_ui("split")
    ),
    tabPanel("Available methods",
             available_ui("available")
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
  
  # reactive getRaw ----
  getRaw <- reactive({
    read.csv(file = "Ass3Data.csv", stringsAsFactors = TRUE)
  })
  
  # module: eda summary ----
  eda_summary_server("eda_summary", getData)
  
  # output BoxPlots ----
  eda_boxplot_server("eda_boxplot", getData)
  
  # output Missing ----
  eda_vis_server("eda_vis", getData)
  
  # output Corr ----
  eda_heatmap_server("eda_heatmap", getData)
  
  # output Table ----
  eda_datatable_server("eda_datatable", getData, getRaw)
  
  # split module — returns train indices from whichever tab is active
  splitIndices <- split_server("split", getData)
  
  # getSplit reads from the module — feeds getTrainData and getTestData downstream
  getSplit <- reactive({ splitIndices() })
  
  # replace reactive getMethods and output Available with discovery module ----
  available_server("available")
  
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
  
  # output SplitSummary ----
  # using module now
  
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
