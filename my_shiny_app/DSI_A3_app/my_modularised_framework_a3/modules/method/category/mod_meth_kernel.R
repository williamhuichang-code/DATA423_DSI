# =================================================================================
# mod_meth_kernel.R  — Kernel Methods category  (svmRadialSigma + gaussprRadial)
# =================================================================================
# Two methods assembled under one shared sidebar.
#
# UI:     meth_kernel_ui(id, pp_choices, default_preprocess, model_seed)
# Server: meth_kernel_server(id, get_data, roles, seed, model_seed,
#                             general_preprocess, svm_preprocess,
#                             gp_preprocess, pp_choices)
# Returns: list(models = reactiveValues, effective_seed = reactive)
# =================================================================================


# ── UI ───────────────────────────────────────────────────────────────────────

meth_kernel_ui <- function(id,
                            pp_choices         = character(0),
                            default_preprocess = character(0),
                            model_seed         = NULL) {
  ns <- NS(id)

  fluidRow(
    column(9,
      tabsetPanel(type = "tabs", id = ns("method_inner"),
        tabPanel("Support Vector Machine — Radial", value = "svmRadialSigma", style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "svmRadialSigma", has_tuning = TRUE)),
        tabPanel("Gaussian Process — Radial",       value = "gaussprRadial",  style = "padding-top:12px;",
                 .meth_subtabs_ui(ns, "gaussprRadial",  has_tuning = TRUE))
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

meth_kernel_server <- function(id, get_data, roles,
                                seed               = reactive(2026),
                                model_seed         = NULL,
                                general_preprocess = NULL,
                                svm_preprocess     = NULL,
                                gp_preprocess      = NULL,
                                pp_choices         = character(0)) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    models <- reactiveValues()

    # ── Common setup ──────────────────────────────────────────────────────────
    setup          <- .meth_common_server_setup(input, output, session, get_data, roles, seed,
                                                model_seed = model_seed)
    effective_seed <- setup$effective_seed
    get_train      <- setup$get_train

    current_method <- reactive({ input$method_inner %||% "svmRadialSigma" })

    # ── Standard output renders ───────────────────────────────────────────────
    .meth_register_outputs(output, "svmRadialSigma", models, ns)
    .meth_register_outputs(output, "gaussprRadial",  models, ns)

    # ── SVM tuning plot: facet by σ, x = log10(C) ────────────────────────────
    # Two tuning params (sigma, C) → same faceted approach as GLMnet.
    # Pick up to 9 representative σ values; within each panel x = log10(C).
    output[["svmRadialSigma_tune_plot"]] <- renderPlot({
      mod    <- models[["svmRadialSigma"]]; req(mod)
      df     <- mod$results

      sigma_vals <- sort(unique(df$sigma))
      n_facets   <- min(9, length(sigma_vals))
      idx        <- round(seq(1, length(sigma_vals), length.out = n_facets))
      sig_sub    <- sigma_vals[idx]

      df_sub <- df[df$sigma %in% sig_sub, ]
      df_sub$sig_label <- factor(
        paste0("σ = ", signif(df_sub$sigma, 3)),
        levels = paste0("σ = ", signif(sig_sub, 3))
      )

      has_sd   <- !all(is.na(df_sub$RMSESD))
      best_C   <- mod$bestTune$C
      best_sig <- mod$bestTune$sigma

      p <- ggplot2::ggplot(df_sub, ggplot2::aes(x = log10(C), y = RMSE,
                                                  colour = log10(C), fill = log10(C)))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          alpha = 0.15, colour = NA
        )
      p +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_vline(
          data = data.frame(sig_label = paste0("σ = ", signif(best_sig, 3)),
                            xint      = log10(best_C)),
          ggplot2::aes(xintercept = xint),
          linetype = "dashed", colour = "#dc3545", linewidth = 0.7,
          inherit.aes = FALSE
        ) +
        ggplot2::scale_colour_viridis_c(name = "log₁₀(C)", option = "viridis") +
        ggplot2::scale_fill_viridis_c(guide = "none", option = "viridis") +
        ggplot2::facet_wrap(~ sig_label, scales = "free_y") +
        ggplot2::labs(
          x        = expression(log[10](C)),
          y        = "RMSE (Bootstrap)",
          title    = "SVM Radial tuning: RMSE by C, faceted by σ",
          subtitle = "Each panel: ‘under this σ, what C works best?’  |  dashed = best C"
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          strip.text      = ggplot2::element_text(face = "bold", size = 11),
          legend.position = "right",
          plot.title      = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle   = ggplot2::element_text(colour = "#6c757d", size = 11),
          axis.text       = ggplot2::element_text(size = 13),
          axis.title      = ggplot2::element_text(size = 13, face = "bold")
        )
    })

    # ── GP Radial tuning plot: RMSE vs σ ─────────────────────────────────────
    # Single tuning parameter (sigma) — simple styled line plot.
    output[["gaussprRadial_tune_plot"]] <- renderPlot({
      mod    <- models[["gaussprRadial"]]; req(mod)
      df     <- mod$results
      has_sd <- !all(is.na(df$RMSESD))

      best_sigma <- mod$bestTune$sigma

      p <- ggplot2::ggplot(df, ggplot2::aes(x = sigma, y = RMSE))
      if (has_sd)
        p <- p + ggplot2::geom_ribbon(
          ggplot2::aes(ymin = RMSE - RMSESD, ymax = RMSE + RMSESD),
          fill = "#6610f2", alpha = 0.2
        )
      p +
        ggplot2::geom_line(colour = "#6610f2", linewidth = 1) +
        ggplot2::geom_point(colour = "#6610f2", size = 2.5) +
        ggplot2::geom_vline(xintercept = best_sigma,
                            linetype = "dashed", colour = "#dc3545", linewidth = 0.8) +
        ggplot2::annotate("text",
                          x     = best_sigma,
                          y     = max(df$RMSE, na.rm = TRUE),
                          label = paste0("best σ = ", signif(best_sigma, 3)),
                          hjust = -0.1, vjust = 1,
                          colour = "#dc3545", size = 4, fontface = "bold") +
        ggplot2::labs(
          x        = "Length-scale parameter (σ)",
          y        = "RMSE (Bootstrap)",
          title    = "Gaussian Process Radial tuning: σ vs RMSE",
          subtitle = "Resampled RMSE ± 1 SD  |  dashed line = best σ"
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          plot.title    = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle = ggplot2::element_text(colour = "#6c757d", size = 11),
          axis.text     = ggplot2::element_text(size = 13),
          axis.title    = ggplot2::element_text(size = 13, face = "bold")
        )
    })

    # ── Preprocessing selector update ─────────────────────────────────────────
    observe({
      method   <- current_method()
      mode     <- input$config_mode
      selected <- if (is.null(mode) || mode == "general") {
        general_preprocess %||% character(0)
      } else {
        switch(method,
               "svmRadialSigma" = svm_preprocess %||% general_preprocess %||% character(0),
               "gaussprRadial"  = gp_preprocess  %||% general_preprocess %||% character(0),
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

        svmRadialSigma = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(
            input, eseed, train_df[[names(r)[r == "outcome"][1]]]
          )
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "svmRadialSigma",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.omit)
        },

        gaussprRadial = function() {
          library(kernlab)
          train_df <- get_train(); req(train_df, nrow(train_df) > 0)
          eseed    <- effective_seed()
          r        <- roles()
          tr_ctrl  <- .meth_build_tr_control(
            input, eseed, train_df[[names(r)[r == "outcome"][1]]]
          )
          rec <- .meth_build_recipe(train_df, input$preprocess, .meth_get_cfg(input), r)
          set.seed(eseed)
          caret::train(rec, data = train_df, method = "gaussprRadial",
                       metric = "RMSE", trControl = tr_ctrl,
                       tuneLength = input$tune_length %||% 5, na.action = na.omit)
        }

      )
    )

    # ── Return ────────────────────────────────────────────────────────────────
    return(list(models = models, effective_seed = effective_seed))
  })
}
