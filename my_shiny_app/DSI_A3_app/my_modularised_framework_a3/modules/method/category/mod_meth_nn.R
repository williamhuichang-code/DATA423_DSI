# =================================================================================
# mod_meth_nn.R  — Neural Network category  (QRNN + BRNN)
# =================================================================================
# Two methods assembled under one shared sidebar.
#
# UI:     meth_nn_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_nn_server(id, get_data, roles, seed, model_seed,
#                         general_preprocess, qrnn_preprocess,
#                         brnn_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_nn_ui <- function(id,
                       pp_choices         = character(0),
                       default_preprocess = character(0),
                       model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("qrnn", value = "qrnn", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "qrnn", has_tuning = TRUE)),
        tabPanel("brnn", value = "brnn", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "brnn", has_tuning = TRUE))
      )
    ),
    column(3,
      .meth_sidebar_ui(ns,
                       model_seed         = model_seed,
                       pp_choices         = pp_choices,
                       default_preprocess = default_preprocess,
                       specific_panels    = NULL)
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

meth_nn_server <- function(id, get_data, roles,
                            seed               = reactive(2026),
                            model_seed         = NULL,
                            general_preprocess = NULL,
                            qrnn_preprocess    = NULL,
                            brnn_preprocess    = NULL,
                            pp_choices         = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "qrnn" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "qrnn", models, ns)
    .meth_register_outputs(output, "brnn", models, ns)

    # ── QRNN tuning plot: facet by bag, x = n.hidden, colour = log10(penalty) ─
    # Three tuning params: n.hidden, penalty (weight decay), bag (bootstrap agg.)
    output[["qrnn_tune_plot"]] <- renderPlot({
      mod <- models[["qrnn"]]; req(mod)
      df  <- mod$results

      has_sd     <- !all(is.na(df$RMSESD))
      best_nh    <- mod$bestTune$n.hidden
      best_pen   <- mod$bestTune$penalty
      best_bag   <- mod$bestTune$bag

      df$bag_label <- ifelse(df$bag, "bag = TRUE", "bag = FALSE")

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = n.hidden,
                                        y      = RMSE,
                                        colour = log10(penalty + 1e-10),
                                        group  = penalty))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = log10(penalty + 1e-10)),
          alpha = 0.12, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(bag_label = ifelse(best_bag, "bag = TRUE", "bag = FALSE"),
                            xint      = best_nh),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(penalty)", option = "plasma") +
        ggplot2::scale_fill_viridis_c(guide = "none",             option = "plasma") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ bag_label) +
        ggplot2::labs(x        = "Hidden units (n.hidden)",
                      y        = "RMSE (Bootstrap)",
                      title    = "QRNN tuning: hidden units vs RMSE, faceted by bagging",
                      subtitle = "Colour = weight-decay penalty  |  dashed = best n.hidden") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── BRNN tuning plot: RMSE vs neurons ─────────────────────────────────────
    # Single tuning parameter (neurons).
    output[["brnn_tune_plot"]] <- renderPlot({
      mod    <- models[["brnn"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))
      best_n <- mod$bestTune$neurons

      p <- ggplot2::ggplot(df, ggplot2::aes(x = neurons, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#fd7e14", alpha = 0.2
        )
      p +
        ggplot2::geom_line(colour = "#fd7e14", linewidth = 1) +
        ggplot2::geom_point(colour = "#fd7e14", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_n,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text",
                          x     = best_n,
                          y     = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best neurons = ", best_n),
                          hjust = -0.1, vjust = 1,
                          colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::labs(x        = "Number of neurons",
                      y        = "RMSE (Bootstrap)",
                      title    = "BRNN tuning: neurons vs RMSE",
                      subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best neurons") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── Preprocessing selector update ─────────────────────────────────────────
    observe({
      method   <- current_method()
      mode     <- input$config_mode
      selected <- if (is.null(mode) || mode == "general") {
        general_preprocess %||% character(0)
      } else {
        switch(method,
               "qrnn" = qrnn_preprocess %||% general_preprocess %||% character(0),
               "brnn" = brnn_preprocess %||% general_preprocess %||% character(0),
               general_preprocess %||% character(0))
      }
      updateSelectizeInput(session, "preprocess",
                           choices  = pp_choices,
                           selected = selected)
    }) |> bindEvent(current_method(), input$config_mode)

    # ── Train / Load / Delete ─────────────────────────────────────────────────
    .meth_action_dispatcher(
      input, output, session,
      models         = models,
      current_method = current_method,
      train_fns = list(

        qrnn = function() {
          library(qrnn)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "qrnn",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.omit)
        },

        brnn = function() {
          library(brnn)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "brnn",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.omit)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
