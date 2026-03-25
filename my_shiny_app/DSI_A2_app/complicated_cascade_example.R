# =============================================================================
# Reactive Cascade Pipeline for Ass2Data.csv
# Strategy: Missing Values → Outliers → Centring & Scaling
# =============================================================================

library(shiny)
library(tidyverse)
library(DT)
library(plotly)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  
  titlePanel("DATA423 — Assignment 2: Data Preprocessing Pipeline"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 # ── STAGE 1: Missing Values ───────────────────────────────────────────
                 h4("Stage 1: Missing Values"),
                 
                 sliderInput("var_miss_thresh",
                             "Drop variables with missingness % above:",
                             min = 0, max = 100, value = 50, step = 5),
                 
                 radioButtons("row_miss_strategy", "Row missingness strategy:",
                              choices = c(
                                "Keep all rows"             = "none",
                                "Drop rows with any NA"     = "drop_any",
                                "Drop rows above threshold" = "drop_thresh"
                              ),
                              selected = "none"),
                 
                 conditionalPanel(
                   condition = "input.row_miss_strategy == 'drop_thresh'",
                   sliderInput("row_miss_thresh",
                               "Drop rows with missingness % above:",
                               min = 0, max = 100, value = 50, step = 5)
                 ),
                 
                 radioButtons("impute_strategy", "Impute remaining NAs:",
                              choices = c(
                                "No imputation"     = "none",
                                "Impute with mean"  = "mean",
                                "Impute with median"= "median"
                              ),
                              selected = "none"),
                 
                 hr(),
                 
                 # ── STAGE 2: Outliers ─────────────────────────────────────────────────
                 h4("Stage 2: Outliers"),
                 
                 checkboxInput("outlier_on", "Apply outlier treatment", value = FALSE),
                 
                 conditionalPanel(
                   condition = "input.outlier_on == true",
                   
                   sliderInput("iqr_multiplier",
                               "IQR multiplier (Tukey fence):",
                               min = 0.5, max = 5, value = 1.5, step = 0.5),
                   
                   radioButtons("outlier_strategy", "Outlier treatment:",
                                choices = c(
                                  "Winsorise (cap to fence)" = "winsorise",
                                  "Replace with NA"          = "na",
                                  "Remove rows"              = "remove"
                                ),
                                selected = "winsorise")
                 ),
                 
                 hr(),
                 
                 # ── STAGE 3: Centring & Scaling ───────────────────────────────────────
                 h4("Stage 3: Centring & Scaling"),
                 
                 radioButtons("scale_strategy", "Transform numeric columns:",
                              choices = c(
                                "None"        = "none",
                                "Centre only" = "center",
                                "Standardise (z-score)" = "standardise",
                                "Normalise (min-max)"   = "normalise"
                              ),
                              selected = "none"),
                 
                 hr(),
                 actionButton("reset", "Reset all to defaults",
                              icon = icon("rotate-left"), width = "100%")
    ),
    
    mainPanel(width = 9,
              
              tabsetPanel(
                
                tabPanel("Pipeline Summary",
                         br(),
                         fluidRow(
                           column(4, wellPanel(
                             h5("After Stage 1 — Missing Values"),
                             verbatimTextOutput("summary_stage1")
                           )),
                           column(4, wellPanel(
                             h5("After Stage 2 — Outliers"),
                             verbatimTextOutput("summary_stage2")
                           )),
                           column(4, wellPanel(
                             h5("After Stage 3 — Scaling"),
                             verbatimTextOutput("summary_stage3")
                           ))
                         ),
                         hr(),
                         h5("Final processed data (first 10 rows)"),
                         DT::dataTableOutput("final_table")
                ),
                
                tabPanel("Missingness Detail",
                         br(),
                         plotlyOutput("miss_bar", height = "500px"),
                         hr(),
                         verbatimTextOutput("miss_text")
                ),
                
                tabPanel("Outlier Detail",
                         br(),
                         selectInput("outlier_var", "Variable to inspect:", choices = NULL),
                         plotlyOutput("outlier_box", height = "400px"),
                         verbatimTextOutput("outlier_text")
                ),
                
                tabPanel("Distribution Before / After",
                         br(),
                         selectInput("dist_var", "Variable to inspect:", choices = NULL),
                         fluidRow(
                           column(6, plotlyOutput("dist_before", height = "350px")),
                           column(6, plotlyOutput("dist_after",  height = "350px"))
                         )
                )
              )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  
  # ── STAGE 0: Raw data ────────────────────────────────────────────────────
  
  getRawData <- shiny::reactive({
    df <- read.csv("Ass2Data.csv", header = TRUE, stringsAsFactors = FALSE)
    # coerce obviously numeric columns
    df[] <- lapply(df, function(x) {
      if (is.character(x)) {
        num <- suppressWarnings(as.numeric(x))
        if (!all(is.na(num))) return(num)
      }
      x
    })
    df
  })
  
  
  # ── STAGE 1: Missing value treatment ─────────────────────────────────────
  #   Depends on: input$var_miss_thresh, input$row_miss_strategy,
  #               input$row_miss_thresh, input$impute_strategy
  #   When any of these change, only Stage 1 onwards reruns.
  #   getRawData() is unaffected.
  
  getCleanData <- shiny::reactive({
    d <- getRawData()
    
    # 1a. Drop variables above column missingness threshold
    pMiss_col <- function(x) sum(is.na(x)) / length(x) * 100
    col_ratio  <- sapply(d, pMiss_col)
    d <- d[, col_ratio <= input$var_miss_thresh, drop = FALSE]
    
    # 1b. Row missingness strategy
    if (input$row_miss_strategy == "drop_any") {
      d <- d[complete.cases(d), , drop = FALSE]
      
    } else if (input$row_miss_strategy == "drop_thresh") {
      pMiss_row <- function(x) sum(is.na(x)) / length(x) * 100
      row_ratio  <- apply(d, 1, pMiss_row)
      d <- d[row_ratio <= input$row_miss_thresh, , drop = FALSE]
    }
    
    # 1c. Imputation (numeric cols only)
    if (input$impute_strategy != "none") {
      num_cols <- names(d)[sapply(d, is.numeric)]
      d[num_cols] <- lapply(d[num_cols], function(x) {
        if (!anyNA(x)) return(x)
        fill <- if (input$impute_strategy == "mean") mean(x, na.rm = TRUE)
        else median(x, na.rm = TRUE)
        x[is.na(x)] <- fill
        x
      })
    }
    
    d
  })
  
  
  # ── STAGE 2: Outlier treatment ────────────────────────────────────────────
  #   Depends on: getCleanData(), input$outlier_on,
  #               input$iqr_multiplier, input$outlier_strategy
  #   When outlier controls change, Stage 1 is NOT re-run.
  
  getOutlierData <- shiny::reactive({
    d <- getCleanData()
    
    if (!isTRUE(input$outlier_on)) return(d)
    
    num_cols <- names(d)[sapply(d, is.numeric)]
    k        <- input$iqr_multiplier
    
    if (input$outlier_strategy == "remove") {
      # flag any row that has an outlier in any numeric col, then remove
      is_outlier_row <- rep(FALSE, nrow(d))
      for (v in num_cols) {
        x   <- d[[v]]
        q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        is_outlier_row <- is_outlier_row | (!is.na(x) & (x < q[1] - k*iqr | x > q[2] + k*iqr))
      }
      d <- d[!is_outlier_row, , drop = FALSE]
      
    } else {
      for (v in num_cols) {
        x   <- d[[v]]
        q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        lo  <- q[1] - k * iqr
        hi  <- q[2] + k * iqr
        
        if (input$outlier_strategy == "winsorise") {
          d[[v]] <- pmax(pmin(x, hi), lo)
        } else if (input$outlier_strategy == "na") {
          d[[v]][x < lo | x > hi] <- NA
        }
      }
    }
    
    d
  })
  
  
  # ── STAGE 3: Centring & Scaling ───────────────────────────────────────────
  #   Depends on: getOutlierData(), input$scale_strategy
  #   When scale_strategy changes, Stages 1 & 2 are NOT re-run.
  
  getScaledData <- shiny::reactive({
    d        <- getOutlierData()
    num_cols <- names(d)[sapply(d, is.numeric)]
    
    if (input$scale_strategy == "none" || length(num_cols) == 0) return(d)
    
    d[num_cols] <- lapply(d[num_cols], function(x) {
      switch(input$scale_strategy,
             "center"      = x - mean(x, na.rm = TRUE),
             "standardise" = as.numeric(scale(x, center = TRUE, scale = TRUE)),
             "normalise"   = {
               mn <- min(x, na.rm = TRUE); mx <- max(x, na.rm = TRUE)
               if (mx == mn) return(x)
               (x - mn) / (mx - mn)
             }
      )
    })
    
    d
  })
  
  
  # ── RESET ────────────────────────────────────────────────────────────────
  
  observeEvent(input$reset, {
    updateSliderInput(session,   "var_miss_thresh",   value = 50)
    updateRadioButtons(session,  "row_miss_strategy", selected = "none")
    updateSliderInput(session,   "row_miss_thresh",   value = 50)
    updateRadioButtons(session,  "impute_strategy",   selected = "none")
    updateCheckboxInput(session, "outlier_on",        value = FALSE)
    updateSliderInput(session,   "iqr_multiplier",    value = 1.5)
    updateRadioButtons(session,  "outlier_strategy",  selected = "winsorise")
    updateRadioButtons(session,  "scale_strategy",    selected = "none")
  })
  
  
  # ── VARIABLE SELECTORS ────────────────────────────────────────────────────
  
  observe({
    num_vars <- names(getCleanData())[sapply(getCleanData(), is.numeric)]
    updateSelectInput(session, "outlier_var", choices = num_vars,
                      selected = if (length(num_vars) > 0) num_vars[1] else NULL)
    updateSelectInput(session, "dist_var",    choices = num_vars,
                      selected = if (length(num_vars) > 0) num_vars[1] else NULL)
  })
  
  
  # ── PIPELINE SUMMARY ─────────────────────────────────────────────────────
  
  fmt_summary <- function(d_before, d_after, label) {
    rows_dropped <- nrow(d_before) - nrow(d_after)
    cols_dropped <- ncol(d_before) - ncol(d_after)
    na_before    <- sum(is.na(d_before))
    na_after     <- sum(is.na(d_after))
    paste0(
      "Rows : ", nrow(d_after),
      if (rows_dropped > 0) paste0("  (−", rows_dropped, ")") else "",
      "\nCols : ", ncol(d_after),
      if (cols_dropped > 0) paste0("  (−", cols_dropped, ")") else "",
      "\nNAs  : ", na_after,
      if (na_before != na_after) paste0("  (was ", na_before, ")") else ""
    )
  }
  
  output$summary_stage1 <- renderPrint({
    cat(fmt_summary(getRawData(), getCleanData(), "Stage 1"))
  })
  
  output$summary_stage2 <- renderPrint({
    cat(fmt_summary(getCleanData(), getOutlierData(), "Stage 2"))
  })
  
  output$summary_stage3 <- renderPrint({
    d  <- getScaledData()
    num_cols <- names(d)[sapply(d, is.numeric)]
    cat(
      "Rows : ", nrow(d),
      "\nCols : ", ncol(d),
      "\nNumeric transformed:", length(num_cols),
      "\nStrategy:", input$scale_strategy
    )
  })
  
  output$final_table <- DT::renderDataTable({
    DT::datatable(head(getScaledData(), 10),
                  rownames = FALSE,
                  options  = list(scrollX = TRUE, dom = "t"))
  })
  
  
  # ── MISSINGNESS DETAIL ────────────────────────────────────────────────────
  
  output$miss_bar <- renderPlotly({
    d        <- getRawData()
    pct      <- sort(sapply(d, function(x) sum(is.na(x)) / nrow(d) * 100),
                     decreasing = TRUE)
    df_miss  <- data.frame(Variable = names(pct), Pct = pct)
    threshold <- input$var_miss_thresh
    df_miss$Dropped <- df_miss$Pct > threshold
    
    p <- ggplot(df_miss, aes(x = reorder(Variable, Pct), y = Pct,
                             fill = Dropped,
                             text = paste0(Variable, ": ", round(Pct, 1), "%"))) +
      geom_col() +
      geom_hline(yintercept = threshold, linetype = "dashed", colour = "red") +
      scale_fill_manual(values = c("FALSE" = "#4A90D9", "TRUE" = "#C41E3A"),
                        labels = c("Kept", "Dropped")) +
      coord_flip() +
      labs(title = "Variable Missingness %",
           x = NULL, y = "Missing (%)", fill = NULL) +
      theme_minimal(base_size = 12)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$miss_text <- renderPrint({
    d      <- getRawData()
    d_clean <- getCleanData()
    cat("Raw data       :", nrow(d),     "rows ×", ncol(d),      "cols\n")
    cat("After Stage 1  :", nrow(d_clean), "rows ×", ncol(d_clean), "cols\n")
    cat("Total NAs raw  :", sum(is.na(d)), "\n")
    cat("Total NAs after:", sum(is.na(d_clean)), "\n")
  })
  
  
  # ── OUTLIER DETAIL ────────────────────────────────────────────────────────
  
  output$outlier_box <- renderPlotly({
    req(input$outlier_var)
    d_before <- getCleanData()
    d_after  <- getOutlierData()
    v        <- input$outlier_var
    req(v %in% names(d_before), v %in% names(d_after))
    
    df_plot <- data.frame(
      Value = c(d_before[[v]], d_after[[v]]),
      Stage = rep(c("Before outlier treatment", "After outlier treatment"),
                  each = nrow(d_before))
    )
    
    p <- ggplot(df_plot, aes(x = Stage, y = Value, fill = Stage)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 1.5) +
      scale_fill_manual(values = c("Before outlier treatment" = "#4A90D9",
                                   "After outlier treatment"  = "#2CA02C")) +
      labs(title = paste("Outlier treatment —", v), x = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$outlier_text <- renderPrint({
    req(input$outlier_var)
    v  <- input$outlier_var
    x  <- getCleanData()[[v]]
    k  <- input$iqr_multiplier
    q  <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lo  <- q[1] - k * iqr
    hi  <- q[2] + k * iqr
    n_out <- sum(!is.na(x) & (x < lo | x > hi))
    cat("Variable  :", v, "\n")
    cat("IQR fence : [", round(lo, 3), ",", round(hi, 3), "]\n")
    cat("Outliers  :", n_out, "of", sum(!is.na(x)), "non-NA values\n")
    cat("           (", round(n_out / sum(!is.na(x)) * 100, 1), "%)\n")
  })
  
  
  # ── DISTRIBUTION BEFORE / AFTER ───────────────────────────────────────────
  
  dist_plot <- function(x, title, colour) {
    df <- data.frame(v = x[!is.na(x)])
    p  <- ggplot(df, aes(x = v)) +
      geom_histogram(fill = colour, colour = "white", bins = 30, alpha = 0.85) +
      labs(title = title, x = NULL, y = "Count") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  }
  
  output$dist_before <- renderPlotly({
    req(input$dist_var)
    v <- input$dist_var
    req(v %in% names(getRawData()))
    dist_plot(getRawData()[[v]], paste("Raw —", v), "#4A90D9")
  })
  
  output$dist_after <- renderPlotly({
    req(input$dist_var)
    v <- input$dist_var
    req(v %in% names(getScaledData()))
    dist_plot(getScaledData()[[v]], paste("Processed —", v), "#2CA02C")
  })
  
}


# =============================================================================
shinyApp(ui, server)