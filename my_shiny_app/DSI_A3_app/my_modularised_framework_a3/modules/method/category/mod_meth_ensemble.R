# =================================================================================
# mod_meth_ensemble.R  — Ensemble Methods category  (Cubist + Ranger)
# =================================================================================
# Two methods assembled under one shared sidebar.
#
# UI:     meth_ensemble_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_ensemble_server(id, get_data, roles, seed, model_seed,
#                               general_preprocess, cubist_preprocess,
#                               ranger_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_ensemble_ui <- function(id,
                              pp_choices         = character(0),
                              default_preprocess = character(0),
                              model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("ranger", value = "ranger", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "ranger", has_tuning = TRUE))
        # tabPanel("cubist", value = "cubist", style = "padding-top:12px;",
        #          .meth_subtabs_ui(ns, "cubist", has_tuning = TRUE))

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

meth_ensemble_server <- function(id, get_data, roles,
                                  seed               = reactive(2026),
                                  model_seed         = NULL,
                                  general_preprocess = NULL,
                                  cubist_preprocess  = NULL,
                                  ranger_preprocess  = NULL,
                                  pp_choices         = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "ranger" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "cubist", models, ns)
    .meth_register_outputs(output, "ranger", models, ns)

    # ── Cubist tuning plot: facet by committees, x = neighbors ───────────────
    output[["cubist_tune_plot"]] <- renderPlot({
      mod <- models[["cubist"]]; req(mod)
      df  <- mod$results

      comm_vals <- sort(unique(df$committees))
      n_facets  <- min(9, length(comm_vals))
      idx       <- round(seq(1, length(comm_vals), length.out = n_facets))
      comm_sub  <- comm_vals[idx]

      df_sub <- df[df$committees %in% comm_sub, ]
      df_sub$comm_label <- factor(
        paste0("committees = ", df_sub$committees),
        levels = paste0("committees = ", comm_sub)
      )

      has_sd     <- !all(is.na(df_sub$RMSESD))
      best_comm  <- mod$bestTune$committees
      best_neigh <- mod$bestTune$neighbors

      p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = neighbors, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#e83e8c", alpha = 0.2)
      p +
        ggplot2::geom_line(colour = "#e83e8c", linewidth = 0.9) +
        ggplot2::geom_point(colour = "#e83e8c", size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(comm_label = paste0("committees = ", best_comm),
                            xint       = best_neigh),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ comm_label, scales = "free_y") +
        ggplot2::labs(x = "Neighbors (k for instance correction)",
                      y = "RMSE (Bootstrap)",
                      title    = "Cubist tuning: neighbors vs RMSE, faceted by committees",
                      subtitle = "Each panel: 'under this ensemble size, how many neighbors?'  |  dashed = best") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text    = ggplot2::element_text(face = "bold", size = 11),
                       plot.title    = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text     = ggplot2::element_text(size = 13),
                       axis.title    = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── Ranger tuning plot: facet by splitrule, x = mtry, colour = min.node.size
    output[["ranger_tune_plot"]] <- renderPlot({
      mod <- models[["ranger"]]; req(mod)
      df  <- mod$results

      has_sd <- !all(is.na(df$RMSESD))

      best_mtry      <- mod$bestTune$mtry
      best_splitrule <- mod$bestTune$splitrule
      best_mns       <- mod$bestTune$min.node.size

      p <- ggplot2::ggplot(df,
                           ggplot2::aes(x      = mtry,
                                        y      = RMSE,
                                        colour = factor(min.node.size),
                                        group  = factor(min.node.size)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD,
                       fill = factor(min.node.size)),
          alpha = 0.1, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(splitrule = best_splitrule, xint = best_mtry),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_d(name = "min.node.size", option = "viridis") +
        ggplot2::scale_fill_viridis_d(guide = "none",           option = "viridis") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
        ggplot2::facet_wrap(~ splitrule) +
        ggplot2::labs(x        = "Variables per split (mtry)",
                      y        = "RMSE (Bootstrap)",
                      title    = "Ranger tuning: mtry vs RMSE, faceted by split rule",
                      subtitle = "Lines = min.node.size levels  |  dashed = best mtry") +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(strip.text      = ggplot2::element_text(face = "bold", size = 11),
                       legend.position = "right",
                       plot.title      = ggplot2::element_text(face = "bold", size = 14),
                       plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
                       axis.text       = ggplot2::element_text(size = 13),
                       axis.title      = ggplot2::element_text(size = 13, face = "bold"))
    })

    # ── Preprocessing selector update ─────────────────────────────────────────
    observe({
      method   <- current_method()
      mode     <- input$config_mode
      selected <- if (is.null(mode) || mode == "general") {
        general_preprocess %||% character(0)
      } else {
        switch(method,
               "cubist" = cubist_preprocess %||% general_preprocess %||% character(0),
               "ranger" = ranger_preprocess %||% general_preprocess %||% character(0),
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

        cubist = function() {
          library(Cubist)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "cubist",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.omit)
        },

        ranger = function() {
          library(ranger)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(input, eseed, train_df[[names(r)[r == "outcome"][1]]])
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          # NOTE: na.action is intentionally omitted for ranger.
          # ranger internally does `na.action == "na.fail"` as a string comparison,
          # which throws "comparison is possible only for atomic and list types" when
          # na.action is a function object (e.g. na.omit). Handle NAs via recipe
          # preprocessing steps (naomit / imputation) instead.
          caret::train(rec, data = train_df, method = "ranger",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5,
                       num.threads = 1)   # disable ranger's own threading — caret handles parallelism
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
