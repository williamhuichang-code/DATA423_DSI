# =================================================================================
# mod_available.R
# Available Methods tab — faithful to Tutorial 2 (Method Discovery)
# Sections: datatable (f3/f4), tag filter (f7), method map (f5/f6)
# =================================================================================

# ── LIBRARIES ─────────────────────────────────────────────────────────────────
library(DT)
library(ggplot2)
library(ggrepel)

# ── SHARED STYLE HELPERS ──────────────────────────────────────────────────────

.av_sidebar_style <- "background-color: #f4f6fb; border-left: 3px solid #6a9fd8;
                      min-height: 100vh; padding: 16px 14px;"

.av_info_box <- function(...) {
  div(
    style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
    icon("info-circle", style = "color:#0d6efd;"),
    HTML("&nbsp;"),
    ...
  )
}

.av_ctrl_label <- function(text) {
  tags$label(text, style = "font-weight:600; font-size:13px; color:#343a40;")
}

.av_section_head <- function(text) {
  h4(text, style = "border-left: 3px solid #534AB7; padding-left: 8px;
                    font-size:14px; margin-top:16px; margin-bottom:8px;")
}

.av_label_fix <- tags$style(HTML("
  .shiny-input-container label,
  .radio label,
  .checkbox label {
    font-weight: 400 !important;
    font-size: 13px;
  }
"))

# full list of caret tags (from tutorial)
.all_tags <- c(
  "Accepts Case Weights", "Bagging", "Bayesian Model", "Binary Predictors Only",
  "Boosting", "Categorical Predictors Only", "Cost Sensitive Learning",
  "Discriminant Analysis", "Distance Weighted Discrimination", "Ensemble Model",
  "Feature Extraction", "Feature Selection Wrapper", "Gaussian Process",
  "Generalized Additive Model", "Generalized Linear Model",
  "Handle Missing Predictor Data", "Implicit Feature Selection", "Kernel Method",
  "L1 Regularization", "L2 Regularization", "Linear Classifier",
  "Linear Regression", "Logic Regression", "Logistic Regression", "Mixture Model",
  "Model Tree", "Multivariate Adaptive Regression Splines", "Neural Network",
  "Oblique Tree", "Ordinal Outcomes", "Partial Least Squares",
  "Patient Rule Induction Method", "Polynomial Model", "Prototype Models",
  "Quantile Regression", "Radial Basis Function", "Random Forest",
  "Regularization", "Relevance Vector Machines", "Ridge Regression",
  "Robust Methods", "Robust Model", "ROC Curves", "Rule-Based Model",
  "Self-Organising Maps", "String Kernel", "Support Vector Machines",
  "Supports Class Probabilities", "Text Mining", "Tree-Based Model",
  "Two Class Only"
)


# =================================================================================
# UI
# =================================================================================

available_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    .av_label_fix,
    tabsetPanel(
      type = "pills",
      
      # ── TAB A: Method table ───────────────────────────────────────────────
      tabPanel("Method table",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .av_sidebar_style,
                   .av_info_box(HTML("<strong>Tab note:</strong><br>
              Full caret method catalogue as a searchable table. Ban icon
              indicates the package is not yet installed. Use column search
              and filters to explore.")),
                   hr(),
                   .av_ctrl_label("Show methods"),
                   radioButtons(ns("tbl_type"), label = NULL,
                                choices = c("Regression only"      = "reg",
                                            "Classification only"  = "cls",
                                            "Both"                 = "both"),
                                selected = "reg"),
                   hr(),
                   .av_ctrl_label("Show columns"),
                   checkboxGroupInput(ns("tbl_cols"), label = NULL,
                                      choices  = c("Label", "Packages", "Tags",
                                                   "Hyperparams", "Regression",
                                                   "Classification", "ClassProbs"),
                                      selected = c("Label", "Packages", "Tags",
                                                   "Hyperparams")),
                   hr(),
                   .av_ctrl_label("Rows per page"),
                   selectInput(ns("tbl_pagesize"), label = NULL,
                               choices = c(5, 10, 15, 25, 50), selected = 10)
                 ),
                 mainPanel(
                   width = 9,
                   .av_section_head("Caret method catalogue"),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput(ns("method_table"))
                   )
                 )
               )
      ),
      
      # ── TAB B: Tag filter ─────────────────────────────────────────────────
      tabPanel("Tag filter",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .av_sidebar_style,
                   .av_info_box(HTML("<strong>Tab note:</strong><br>
              Filter methods by combining tag rules — mirrors the tutorial's
              <code>grepl()</code> approach. Use Include/Exclude lists to
              narrow down candidate methods for your assignment.")),
                   hr(),
                   .av_ctrl_label("Must be regression"),
                   checkboxInput(ns("flt_regression"), label = NULL, value = TRUE),
                   hr(),
                   .av_ctrl_label("Include tags (ALL must match)"),
                   selectizeInput(ns("flt_include"), label = NULL,
                                  choices  = .all_tags,
                                  multiple = TRUE,
                                  options  = list(placeholder = "e.g. Ensemble Model")),
                   hr(),
                   .av_ctrl_label("Exclude tags (ANY will disqualify)"),
                   selectizeInput(ns("flt_exclude"), label = NULL,
                                  choices  = .all_tags,
                                  multiple = TRUE,
                                  options  = list(placeholder = "e.g. Linear Regression")),
                   hr(),
                   .av_ctrl_label("Must handle missing predictors"),
                   checkboxInput(ns("flt_missing"), label = NULL, value = FALSE),
                   hr(),
                   .av_ctrl_label("Must support implicit feature selection"),
                   checkboxInput(ns("flt_implicit"), label = NULL, value = FALSE),
                   hr(),
                   actionButton(ns("flt_apply"), "Apply filters",
                                icon  = icon("filter"), width = "100%",
                                style = "background:#534AB7; color:white;
                                  border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   .av_section_head("Filtered methods"),
                   verbatimTextOutput(ns("flt_summary")),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput(ns("flt_table"))
                   )
                 )
               )
      ),
      
      # ── TAB C: Method map ─────────────────────────────────────────────────
      tabPanel("Method map",
               br(),
               sidebarLayout(
                 position = "right",
                 sidebarPanel(
                   width = 3,
                   style = .av_sidebar_style,
                   .av_info_box(HTML("<strong>Tab note:</strong><br>
              Builds a wide binary tag matrix (f5), computes distances between
              methods, then reduces to 2D with <code>cmdscale()</code> (f6).
              Similar methods cluster together — use this to choose a
              <em>diverse</em> set of candidate models.")),
                   hr(),
                   .av_ctrl_label("Method type"),
                   radioButtons(ns("map_type"), label = NULL,
                                choices = c("Regression"     = "reg",
                                            "Classification" = "cls"),
                                selected = "reg"),
                   hr(),
                   .av_ctrl_label("Distance metric"),
                   selectInput(ns("map_dist"), label = NULL,
                               choices  = c("euclidean", "manhattan",
                                            "binary", "canberra"),
                               selected = "euclidean"),
                   hr(),
                   .av_ctrl_label("Label size"),
                   sliderInput(ns("map_label_size"), label = NULL,
                               min = 1, max = 5, value = 2, step = 0.5),
                   .av_ctrl_label("Max label overlaps"),
                   sliderInput(ns("map_overlaps"), label = NULL,
                               min = 10, max = 100, value = 50, step = 5),
                   hr(),
                   .av_ctrl_label("Highlight methods (by short name)"),
                   selectizeInput(ns("map_highlight"), label = NULL,
                                  choices  = NULL,
                                  multiple = TRUE,
                                  options  = list(placeholder = "e.g. glmnet, rpart")),
                   hr(),
                   actionButton(ns("map_build"), "Build map",
                                icon  = icon("map"), width = "100%",
                                style = "background:#534AB7; color:white;
                                  border:none; font-size:13px;")
                 ),
                 mainPanel(
                   width = 9,
                   .av_section_head("Method similarity map"),
                   shinycssloaders::withSpinner(
                     plotOutput(ns("map_plot"), height = "650px")
                   ),
                   hr(),
                   .av_section_head("Wide tag matrix (basis for distances)"),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput(ns("wide_table"))
                   )
                 )
               )
      )
      
    ) # end tabsetPanel
  )
}


# =================================================================================
# SERVER
# =================================================================================

available_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Core data: caretMethodsDF (tutorial f3) ───────────────────────────────
    # Built once on startup — expensive, so cached in a reactive
    methods_df <- reactive({
      mi <- caret::getModelInfo()
      
      Label          <- vector(mode = "character", length = length(mi))
      Packages       <- vector(mode = "character", length = length(mi))
      Packages_plain <- vector(mode = "character", length = length(mi))
      Hyperparams    <- vector(mode = "character", length = length(mi))
      Regression     <- vector(mode = "logical",   length = length(mi))
      Classification <- vector(mode = "logical",   length = length(mi))
      Tags           <- vector(mode = "character", length = length(mi))
      ClassProbs     <- vector(mode = "logical",   length = length(mi))
      
      for (row in seq_along(mi)) {
        Label[row] <- mi[[row]]$label
        libs       <- mi[[row]]$library
        libs       <- na.omit(libs[libs != ""])
        
        if (length(libs) > 0) {
          # plain version for filtering
          Packages_plain[row] <- paste(libs, collapse = "\n")
          # HTML version with ban icon for uninstalled packages
          present <- suppressWarnings(
            sapply(libs, function(l)
              require(package = l, warn.conflicts = FALSE,
                      character.only = TRUE, quietly = TRUE))
          )
          check        <- ifelse(present, "", as.character(icon(name = "ban")))
          Packages[row] <- paste(paste(libs, check), collapse = "<br/>")
        }
        
        d              <- mi[[row]]$parameters
        Hyperparams[row] <- paste(
          paste0(d$parameter, " - ", d$label, " [", d$class, "]"),
          collapse = "<br/>")
        Regression[row]     <- "Regression"     %in% mi[[row]]$type
        Classification[row] <- "Classification" %in% mi[[row]]$type
        Tags[row]           <- paste(mi[[row]]$tags, collapse = "<br/>")
        ClassProbs[row]     <- is.function(mi[[row]]$prob)
      }
      
      data.frame(
        Model          = names(mi),
        Label,
        Packages,
        Packages_plain,
        Regression,
        Classification,
        Tags,
        Hyperparams,
        ClassProbs,
        stringsAsFactors = FALSE
      )
    })
    
    # ── Wide tag matrix (tutorial f5) ─────────────────────────────────────────
    wide_matrix <- reactive({
      modelInfo      <- caret::getModelInfo()
      tags           <- vector(mode = "list", length = length(modelInfo))
      Classification <- Regression <- ClassProbs <- rep(NA, length(modelInfo))
      
      for (i in seq_along(modelInfo)) {
        tags[[i]]        <- modelInfo[[i]]$tags
        Classification[i] <- ifelse("Classification" %in% modelInfo[[i]]$type, 1, 0)
        Regression[i]    <- ifelse("Regression"     %in% modelInfo[[i]]$type, 1, 0)
        ClassProbs[i]    <- ifelse(is.null(modelInfo[[i]]$prob), 0, 1)
      }
      
      tabs  <- table(unlist(tags))
      tabs  <- tabs[order(tolower(names(tabs)))]
      terms <- names(tabs)
      terms <- terms[terms != ""]
      
      dat <- matrix(0, ncol = length(terms), nrow = length(tags))
      colnames(dat) <- terms
      hasTag <- lapply(tags, function(x, y) which(y %in% x), y = terms)
      for (i in seq_along(hasTag)) dat[i, hasTag[[i]]] <- 1
      
      dat  <- cbind(Classification, Regression, dat)
      wide <- as.data.frame(dat, row.names = names(modelInfo))
      wide
    })
    
    # Populate map highlight selector with method short names
    observe({
      df <- methods_df()
      updateSelectizeInput(session, "map_highlight",
                           choices  = df$Model,
                           selected = c("glmnet", "pls", "rpart"),
                           server   = TRUE)
    })
    
    
    # ── TAB A: Method table ───────────────────────────────────────────────────
    
    output$method_table <- DT::renderDataTable({
      df <- methods_df()
      
      # filter by type
      df <- switch(input$tbl_type,
                   "reg"  = df[df$Regression == TRUE, ],
                   "cls"  = df[df$Classification == TRUE, ],
                   "both" = df
      )
      
      # choose columns — always keep Model
      show_cols <- c("Model", intersect(input$tbl_cols,
                                        c("Label", "Packages", "Tags",
                                          "Hyperparams", "Regression",
                                          "Classification", "ClassProbs")))
      df <- df[, show_cols, drop = FALSE]
      
      DT::datatable(
        df,
        escape    = FALSE,
        rownames  = FALSE,
        selection = "none",
        options   = list(
          pageLength = as.integer(input$tbl_pagesize),
          lengthMenu = c(5, 10, 15, 25, 50),
          scrollX    = TRUE
        )
      )
    })
    
    
    # ── TAB B: Tag filter ─────────────────────────────────────────────────────
    
    flt_result <- eventReactive(input$flt_apply, {
      df <- methods_df()
      
      # use plain tags for grepl matching (no HTML)
      mi    <- caret::getModelInfo()
      Tags_plain <- sapply(mi, function(m) paste(m$tags, collapse = "|"))
      df$Tags_plain <- Tags_plain
      
      # regression filter
      if (isTRUE(input$flt_regression)) df <- df[df$Regression == TRUE, ]
      
      # include tags — ALL must match
      for (tag in input$flt_include) {
        df <- df[grepl(tag, df$Tags_plain, ignore.case = TRUE, fixed = TRUE), ]
      }
      
      # exclude tags — ANY disqualifies
      for (tag in input$flt_exclude) {
        df <- df[!grepl(tag, df$Tags_plain, ignore.case = TRUE, fixed = TRUE), ]
      }
      
      # handle missing
      if (isTRUE(input$flt_missing)) {
        df <- df[grepl("Handle Missing Predictor Data", df$Tags_plain,
                       fixed = TRUE), ]
      }
      
      # implicit feature selection
      if (isTRUE(input$flt_implicit)) {
        df <- df[grepl("Implicit Feature Selection", df$Tags_plain,
                       fixed = TRUE), ]
      }
      
      df[, c("Model", "Label", "Packages", "Tags", "Hyperparams",
             "Regression", "Classification", "ClassProbs")]
    }, ignoreNULL = FALSE)
    
    output$flt_summary <- renderPrint({
      df <- flt_result()
      cat(nrow(df), "method(s) match the current filters.\n")
      if (nrow(df) > 0) cat("Models:", paste(df$Model, collapse = ", "), "\n")
    })
    
    output$flt_table <- DT::renderDataTable({
      df <- flt_result()
      DT::datatable(
        df,
        escape    = FALSE,
        rownames  = FALSE,
        selection = "none",
        options   = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    
    # ── TAB C: Method map ─────────────────────────────────────────────────────
    
    map_data <- eventReactive(input$map_build, {
      wide <- wide_matrix()
      
      # filter to chosen type
      if (input$map_type == "reg") {
        wide <- wide[wide$Regression == 1, ]
      } else {
        wide <- wide[wide$Classification == 1, ]
      }
      
      # compute distances and reduce to 2D (tutorial f6)
      d  <- stats::dist(wide, method = input$map_dist)
      dd <- stats::cmdscale(d, k = 2)
      df <- data.frame(
        Model = rownames(dd),
        X1    = dd[, 1],
        X2    = dd[, 2],
        stringsAsFactors = FALSE
      )
      
      # mark highlighted methods
      df$Highlight <- df$Model %in% input$map_highlight
      df
    }, ignoreNULL = FALSE)
    
    output$map_plot <- renderPlot({
      df    <- map_data()
      title <- if (input$map_type == "reg") "Regression Methods" else "Classification Methods"
      
      # split into highlighted and normal
      df_hi <- df[df$Highlight, ]
      df_lo <- df[!df$Highlight, ]
      
      p <- ggplot(mapping = aes(x = X1, y = X2, label = Model)) +
        ggtitle(label = title) +
        xlab("Coordinate 1") +
        ylab("Coordinate 2") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
      
      # normal points — grey, small labels
      if (nrow(df_lo) > 0) {
        p <- p +
          geom_point(data = df_lo, color = "#aaaaaa", size = 1.5) +
          ggrepel::geom_text_repel(
            data         = df_lo,
            size         = input$map_label_size,
            color        = "#888888",
            max.overlaps = input$map_overlaps,
            na.rm        = TRUE
          )
      }
      
      # highlighted points — purple, larger labels
      if (nrow(df_hi) > 0) {
        p <- p +
          geom_point(data = df_hi, color = "#534AB7", size = 3) +
          ggrepel::geom_text_repel(
            data         = df_hi,
            size         = input$map_label_size + 1,
            color        = "#534AB7",
            fontface     = "bold",
            max.overlaps = input$map_overlaps,
            na.rm        = TRUE
          )
      }
      
      p
    })
    
    # wide tag matrix table
    output$wide_table <- DT::renderDataTable({
      wide <- wide_matrix()
      # filter to chosen type for context
      if (input$map_type == "reg") {
        wide <- wide[wide$Regression == 1, ]
      } else {
        wide <- wide[wide$Classification == 1, ]
      }
      DT::datatable(
        wide,
        rownames  = TRUE,
        selection = "none",
        options   = list(
          pageLength = 10,
          scrollX    = TRUE,
          scrollY    = "300px"
        )
      )
    })
    
  })
}