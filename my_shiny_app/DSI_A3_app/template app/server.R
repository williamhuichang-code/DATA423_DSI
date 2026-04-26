shinyServer(function(input, output, session) {
  
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
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
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
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
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

  
  
})
