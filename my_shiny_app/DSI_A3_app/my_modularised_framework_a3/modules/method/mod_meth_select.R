# =================================================================================
# mod_meth_select.R
# =================================================================================
# Receives:  get_models — reactive returning a named list of trained caret models
#            (aggregated in the app file from all method modules)
# Returns:   list(choice = reactive(selected model name))

library(caret)


# ── UI ───────────────────────────────────────────────────────────────────────

meth_select_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      width = 3,
      style = "background-color:#e8f0fe; border-left:2px solid #a8c0fd;
               min-height:100vh; padding:15px; font-size:13px;",

      div(
        style = "background:white; padding:10px; border-left:4px solid #0d6efd;
                 border-radius:6px; margin-bottom:12px; font-size:13px; color:#343a40;
                 box-shadow:0 1px 2px rgba(0,0,0,0.05);",
        icon("circle-info", style = "color:#0d6efd;"),
        HTML("&nbsp;<b>Model Selection</b><br><br>
              Compare resampled performance across all trained models.
              Normalise against the null model to see relative improvement.
              Select your best model to evaluate on unseen test data.")
      ),

      # ── Model Choice ──────────────────────────────────────────────────────
      .section_hdr("Model Choice", "#fff3cd", "#ffc107"),

      uiOutput(ns("choice_ui")),
      helpText("Select the model to evaluate on test data in the Performance tab."),

      hr(),

      # ── Cross Validation Results ──────────────────────────────────────────
      .section_hdr("Cross Validation Results", "#d1ecf1", "#17a2b8"),

      checkboxInput(ns("notch"),
                    "Show notch",
                    value = FALSE),
      checkboxInput(ns("normalise"),
                    "Normalise",
                    value = TRUE),
      helpText("Scale RMSE and MAE relative to the null model so all methods are comparable."),
      checkboxInput(ns("hide_worse"),
                    "Hide models worse than null model",
                    value = TRUE),
      helpText("Removes methods whose mean resampled RMSE exceeds the null model's.")
    ),

    mainPanel(
      width = 9,
      uiOutput(ns("status_ui")),
      plotOutput(ns("boxplot"), height = "60vh")
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_select_server <- function(id, get_models) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Resamples (mirrors template getResamples) ─────────────────────────────

    get_resamples <- reactive({
      models2 <- Filter(Negate(is.null), get_models())
      req(length(models2) > 1)

      results <- tryCatch(
        caret::resamples(models2),
        error = function(e) NULL
      )
      req(results)

      # ── Normalise against null (template logic) ───────────────────────────
      null_model <- "null"
      if (isTRUE(input$normalise) && null_model %in% results$models) {
        actual_names <- colnames(results$values)
        for (metric in c("RMSE", "MAE")) {
          col <- paste(null_model, metric, sep = "~")
          if (col %in% actual_names) {
            null_metric <- mean(results$values[, col], na.rm = TRUE)
            if (!is.na(null_metric) && null_metric != 0) {
              for (mod_name in results$models) {
                mcol <- paste(mod_name, metric, sep = "~")
                if (mcol %in% actual_names)
                  results$values[, mcol] <- results$values[, mcol] / null_metric
              }
            }
          }
        }
      }

      # ── Hide worse than null (template logic) ─────────────────────────────
      if (isTRUE(input$hide_worse) && null_model %in% names(models2)) {
        actual_names <- colnames(results$values)
        col          <- paste(null_model, "RMSE", sep = "~")
        keep         <- rep(TRUE, length(results$models))
        if (col %in% actual_names) {
          null_metric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(null_metric)) {
            for (i in seq_along(results$models)) {
              mcol <- paste(results$models[i], "RMSE", sep = "~")
              if (mcol %in% actual_names)
                keep[i] <- mean(results$values[, mcol], na.rm = TRUE) <= null_metric
            }
          }
        }
        results$models <- results$models[keep]
      }

      results
    })

    # ── Status message ────────────────────────────────────────────────────────

    output$status_ui <- renderUI({
      models2 <- Filter(Negate(is.null), get_models())
      n <- length(models2)
      if (n == 0) {
        div(
          style = "background:#fff3e0; border-left:4px solid #ffc107;
                   border-radius:6px; padding:12px 16px; margin-bottom:12px;
                   font-size:13px; color:#856404;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML("&nbsp; No models trained yet. Train at least <b>2</b> models
                in the Methods tabs to enable comparison.")
        )
      } else if (n == 1) {
        div(
          style = "background:#fff3e0; border-left:4px solid #ffc107;
                   border-radius:6px; padding:12px 16px; margin-bottom:12px;
                   font-size:13px; color:#856404;",
          icon("triangle-exclamation", style = "color:#ffc107;"),
          HTML(paste0("&nbsp; Only <b>1</b> model trained (<b>", names(models2)[1],
                      "</b>). Train at least <b>2</b> models to compare."))
        )
      } else {
        NULL  # plot takes over
      }
    })

    # ── Boxplot (template: bwplot) ────────────────────────────────────────────

    output$boxplot <- renderPlot({
      res <- get_resamples()
      bwplot(res, notch = isTRUE(input$notch),
             scales = list(
               x = list(cex = 1.3, font = 2),   # axis numbers: larger + bold
               y = list(cex = 1.3, font = 2)    # model name labels: larger + bold
             ),
             par.settings = list(
               par.strip.text = list(cex = 1.3, font = 2)  # metric strip labels (RMSE etc): larger + bold
             ))
    })

    # ── Model choice radio buttons ────────────────────────────────────────────

    output$choice_ui <- renderUI({
      res <- get_resamples()
      choices <- res$models
      if (length(choices) == 0) {
        tags$span(style = "color:#adb5bd; font-size:12px;",
                  "No models available after filtering.")
      } else {
        radioButtons(ns("choice"), label = NULL,
                     choices  = choices,
                     selected = if (length(choices) > 0) choices[1] else character(0),
                     inline   = FALSE)
      }
    })

    # ── Return ────────────────────────────────────────────────────────────────

    return(list(
      choice = reactive({ input$choice })
    ))
  })
}
