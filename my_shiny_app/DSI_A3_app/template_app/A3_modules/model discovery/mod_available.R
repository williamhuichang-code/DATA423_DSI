# =================================================================================
# mod_available.R  (v2)
# Available Methods tab
# Tab 1: Filterable method table
# Tab 2: Method map — filtered methods highlighted automatically
# Shared right sidebar controls both tabs
# =================================================================================

library(DT)
library(ggplot2)
library(ggrepel)

# ── STYLE HELPERS ─────────────────────────────────────────────────────────────

.av_sidebar_style <- "background-color: #f4f6fb; border-left: 3px solid #6a9fd8;
                      min-height: 100vh; padding: 16px 14px;"

.av_info_box <- function(...) {
  div(
    style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
    icon("info-circle", style = "color:#0d6efd;"), HTML("&nbsp;"), ...
  )
}

.av_ctrl_label <- function(text) {
  tags$label(text, style = "font-weight:600; font-size:13px; color:#343a40;
                             display:block; margin-bottom:4px; margin-top:10px;")
}

.av_section_head <- function(text) {
  h4(text, style = "border-left: 3px solid #534AB7; padding-left: 8px;
                    font-size:14px; margin-top:16px; margin-bottom:8px;")
}

.av_label_fix <- tags$style(HTML("
  .shiny-input-container label,
  .radio label, .checkbox label {
    font-weight: 400 !important;
    font-size: 13px;
  }
"))


# =================================================================================
# UI
# =================================================================================

available_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    .av_label_fix,
    sidebarLayout(
      position = "right",
      
      # ── RIGHT SIDEBAR — shared across both subtabs ────────────────────────
      sidebarPanel(
        width = 3,
        style = .av_sidebar_style,
        
        .av_info_box(HTML("<strong>How to use:</strong><br>
          Filter methods using the controls below. The table updates
          instantly. Switch to the map tab to see where filtered
          methods sit relative to all others.")),
        
        # ── Type ──────────────────────────────────────────────────────────
        .av_ctrl_label("Method type"),
        radioButtons(ns("type"), label = NULL,
                     choices  = c("Regression only"     = "reg",
                                  "Classification only" = "cls",
                                  "Both"                = "both"),
                     selected = "reg"),
        hr(),
        
        # ── Tag filters ───────────────────────────────────────────────────
        .av_ctrl_label("Include tags (ALL must match)"),
        selectizeInput(ns("flt_include"), label = NULL,
                       choices  = NULL, multiple = TRUE,
                       options  = list(placeholder = "e.g. Ensemble Model")),
        
        .av_ctrl_label("Exclude tags (ANY disqualifies)"),
        selectizeInput(ns("flt_exclude"), label = NULL,
                       choices  = NULL, multiple = TRUE,
                       options  = list(placeholder = "e.g. Linear Regression")),
        hr(),
        
        # ── Quick checkboxes ──────────────────────────────────────────────
        .av_ctrl_label("Handle missing predictors"),
        checkboxInput(ns("flt_missing"), label = NULL, value = FALSE),
        
        .av_ctrl_label("Implicit feature selection"),
        checkboxInput(ns("flt_implicit"), label = NULL, value = FALSE),
        hr(),
        
        # ── Map controls ──────────────────────────────────────────────────
        .av_ctrl_label("Map distance metric"),
        selectInput(ns("map_dist"), label = NULL,
                    choices  = c("euclidean", "manhattan", "binary", "canberra"),
                    selected = "euclidean"),
        
        .av_ctrl_label("Label size"),
        sliderInput(ns("map_label_size"), label = NULL,
                    min = 1, max = 5, value = 2, step = 0.5, width = "100%"),
        
        .av_ctrl_label("Max label overlaps"),
        sliderInput(ns("map_overlaps"), label = NULL,
                    min = 10, max = 100, value = 50, step = 5, width = "100%"),
        hr(),
        
        # ── Live filter summary ───────────────────────────────────────────
        .av_ctrl_label("Matching methods"),
        verbatimTextOutput(ns("filter_summary"))
      ),
      
      # ── MAIN PANEL — two subtabs ──────────────────────────────────────────
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "pills",
          id   = ns("subtabs"),
          
          # Subtab 1: table
          tabPanel("Method table",
                   br(),
                   .av_section_head("Filtered caret methods"),
                   shinycssloaders::withSpinner(
                     DT::dataTableOutput(ns("method_table"))
                   )
          ),
          
          # Subtab 2: map
          tabPanel("Method map",
                   br(),
                   .av_section_head("Similarity map — filtered methods highlighted in purple"),
                   shinycssloaders::withSpinner(
                     plotOutput(ns("map_plot"), height = "650px")
                   )
          )
        )
      )
    )
  )
}


# =================================================================================
# SERVER
# =================================================================================

available_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Model info — built once ───────────────────────────────────────────────
    mi <- caret::getModelInfo()
    
    # Plain data frame (no HTML) — used for filtering
    methods_plain <- reactive({
      n <- length(mi)
      Label <- Tags_plain <- Packages_plain <- Hyperparams <-
        character(n)
      Regression <- Classification <- ClassProbs <- logical(n)
      
      for (i in seq_len(n)) {
        m               <- mi[[i]]
        Label[i]        <- m$label
        libs            <- na.omit(m$library[m$library != ""])
        Packages_plain[i] <- paste(libs, collapse = "\n")
        Tags_plain[i]   <- paste(m$tags, collapse = "|")
        d               <- m$parameters
        Hyperparams[i]  <- paste(
          paste0(d$parameter, " - ", d$label, " [", d$class, "]"),
          collapse = "\n")
        Regression[i]     <- "Regression"     %in% m$type
        Classification[i] <- "Classification" %in% m$type
        ClassProbs[i]     <- is.function(m$prob)
      }
      
      data.frame(
        Model = names(mi), Label, Packages_plain, Tags_plain,
        Hyperparams, Regression, Classification, ClassProbs,
        stringsAsFactors = FALSE
      )
    })
    
    # HTML packages column (ban icon) — merged in for display only
    packages_html <- reactive({
      df <- methods_plain()
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
          check  <- ifelse(present, "", as.character(icon("ban")))
          html[i] <- paste(paste(libs, check), collapse = "<br/>")
        }
      }
      data.frame(Model = df$Model, Packages_html = html,
                 stringsAsFactors = FALSE)
    })
    
    # Wide binary tag matrix for cmdscale
    wide_matrix <- reactive({
      tags <- lapply(mi, `[[`, "tags")
      Reg  <- sapply(mi, function(m) as.integer("Regression"     %in% m$type))
      Cls  <- sapply(mi, function(m) as.integer("Classification" %in% m$type))
      
      all_tags <- sort(unique(unlist(tags)))
      all_tags <- all_tags[nchar(all_tags) > 0]
      
      dat <- matrix(0L, nrow = length(mi), ncol = length(all_tags),
                    dimnames = list(names(mi), all_tags))
      for (i in seq_along(tags)) {
        matched <- intersect(tags[[i]], all_tags)
        if (length(matched) > 0) dat[i, matched] <- 1L
      }
      
      as.data.frame(cbind(Regression = Reg, Classification = Cls, dat))
    })
    
    # Populate tag selectors
    observe({
      all_tags <- sort(unique(unlist(lapply(mi, `[[`, "tags"))))
      all_tags <- all_tags[nchar(all_tags) > 0]
      updateSelectizeInput(session, "flt_include",
                           choices = all_tags, server = TRUE)
      updateSelectizeInput(session, "flt_exclude",
                           choices = all_tags, server = TRUE)
    })
    
    # ── Shared filter reactive ────────────────────────────────────────────────
    filtered_df <- reactive({
      df <- methods_plain()
      
      # type
      df <- switch(input$type,
                   "reg"  = df[df$Regression,     ],
                   "cls"  = df[df$Classification, ],
                   "both" = df
      )
      
      # include tags
      for (tag in input$flt_include) {
        df <- df[grepl(tag, df$Tags_plain, ignore.case = TRUE), ]
      }
      
      # exclude tags
      for (tag in input$flt_exclude) {
        df <- df[!grepl(tag, df$Tags_plain, ignore.case = TRUE), ]
      }
      
      # missing predictor handling
      if (isTRUE(input$flt_missing)) {
        df <- df[grepl("Handle Missing Predictor Data",
                       df$Tags_plain, ignore.case = TRUE), ]
      }
      
      # implicit feature selection
      if (isTRUE(input$flt_implicit)) {
        df <- df[grepl("Implicit Feature Selection",
                       df$Tags_plain, ignore.case = TRUE), ]
      }
      
      df
    })
    
    # ── Sidebar summary ───────────────────────────────────────────────────────
    output$filter_summary <- renderPrint({
      df <- filtered_df()
      cat(nrow(df), "method(s)\n")
      if (nrow(df) > 0 && nrow(df) <= 30) {
        cat(paste(sort(df$Model), collapse = ", "))
      }
    })
    
    # ── Subtab 1: Method table ────────────────────────────────────────────────
    output$method_table <- DT::renderDataTable({
      filt <- filtered_df()
      html <- packages_html()
      df   <- merge(filt, html, by = "Model")
      
      # tidy for display
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
      
      DT::datatable(
        display,
        escape    = FALSE,
        rownames  = FALSE,
        selection = "none",
        options   = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50),
          scrollX    = TRUE
        )
      )
    })
    
    # ── Subtab 2: Method map ──────────────────────────────────────────────────
    output$map_plot <- renderPlot({
      wide <- wide_matrix()
      
      # restrict map to chosen type
      wide_sub <- switch(input$type,
                         "reg"  = wide[wide$Regression == 1,     ],
                         "cls"  = wide[wide$Classification == 1, ],
                         "both" = wide
      )
      
      req(nrow(wide_sub) >= 3)
      
      # distance + cmdscale
      d  <- stats::dist(wide_sub, method = input$map_dist)
      dd <- stats::cmdscale(d, k = 2)
      
      df_map <- data.frame(
        Model     = rownames(dd),
        X1        = dd[, 1],
        X2        = dd[, 2],
        stringsAsFactors = FALSE
      )
      
      # highlighted = current filter result
      df_map$Highlight <- df_map$Model %in% filtered_df()$Model
      df_hi <- df_map[df_map$Highlight, ]
      df_lo <- df_map[!df_map$Highlight, ]
      
      type_label <- switch(input$type,
                           "reg"  = "Regression",
                           "cls"  = "Classification",
                           "both" = "All"
      )
      
      p <- ggplot(mapping = aes(x = X1, y = X2, label = Model)) +
        ggtitle(paste(type_label,
                      "Methods — purple = filtered, grey = all others")) +
        xlab("Coordinate 1") + ylab("Coordinate 2") +
        theme_minimal(base_size = 13) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
      
      # grey background — all methods
      if (nrow(df_lo) > 0) {
        p <- p +
          geom_point(data = df_lo, color = "#cccccc", size = 1.5) +
          ggrepel::geom_text_repel(
            data         = df_lo,
            size         = input$map_label_size,
            color        = "#aaaaaa",
            max.overlaps = input$map_overlaps,
            na.rm        = TRUE
          )
      }
      
      # purple foreground — filtered methods
      if (nrow(df_hi) > 0) {
        p <- p +
          geom_point(data = df_hi, color = "#534AB7", size = 3) +
          ggrepel::geom_text_repel(
            data         = df_hi,
            size         = input$map_label_size + 1.5,
            color        = "#534AB7",
            fontface     = "bold",
            max.overlaps = input$map_overlaps,
            box.padding  = 0.4,
            na.rm        = TRUE
          )
      }
      
      p
    })
    
  })
}