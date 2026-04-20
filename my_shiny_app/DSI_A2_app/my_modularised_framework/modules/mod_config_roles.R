# =================================================================================
# mod_config_roles.R
# =================================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── UI ───────────────────────────────────────────────────────────────────────

data_roles_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      width = 3,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd; min-height: 100vh; padding-left: 20px;",
      
      # tab note
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br>
             Assign variable roles by dragging pills between buckets. <br><br>
             <b>Shield</b> icons mark domain-important variables — protected
             from automatic exclusion during missingness and feature selection steps.")
      ),
      hr(),
      
      # ── 1. Seed ────────────────────────────────────────────────────────
      tags$label("Global Seed:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      numericInput(ns("seed"), label = NULL,
                   value = as.integer(format(Sys.Date(), "%Y")),
                   min = 0, step = 1, width = "100%"),
      helpText("Affects ratio split and all stochastic steps."),
      hr(),
      
      # ── 2. Split ratio ─────────────────────────────────────────────────
      tags$label("Train-Test Split Ratio:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      sliderInput(ns("train_ratio"), label = NULL,
                  min = 50, max = 95, value = 80, step = 5,
                  post = "%", width = "100%"),
      checkboxInput(ns("use_ratio_split"),
                    "Generate a ratio_splitted column from this ratio", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("use_ratio_split"), "'] == true"),
        textInput(ns("split_col_name"), label = NULL,
                  value = "ratio_splitted", placeholder = "Column name e.g. ratio_splitted")
      ),
      uiOutput(ns("split_counts_ui")),
      hr(),
      
      # ── 3. Important vars ──────────────────────────────────────────────
      tags$label("Domain Important Variables:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      helpText("These will show a shield icon in the role buckets."),
      selectizeInput(ns("important_vars"), label = NULL,
                     choices  = NULL,
                     multiple = TRUE,
                     options  = list(placeholder = "Select important variables")),
      hr(),
      
      # ── 4. Custom title ────────────────────────────────────────────────
      tags$label("Custom panel title:",
                 style = "font-weight:600; font-size:13px; color:#343a40;"),
      textInput(ns("custom_title"), label = NULL,
                placeholder = "Auto-generated if empty"),
      hr(),
      
      # ── 5. Reset ───────────────────────────────────────────────────────
      actionButton(
        ns("reset"),
        label = "Reset All",
        icon  = icon("rotate-left"),
        width = "100%"
      )
      
    ),
    mainPanel(
      width = 9,
      uiOutput(ns("roles_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

data_roles_server <- function(id, get_raw) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ── Role definitions ────────────────────────────────────────────────────
    
    ROLES <- list(
      list(id = "predictor",  label = "Predictor",       hdr = "#185FA5", bg = "#E6F1FB", txt = "#0C447C", bdr = "#B5D4F4"),
      list(id = "outcome",    label = "Outcome",          hdr = "#0F6E56", bg = "#E1F5EE", txt = "#085041", bdr = "#9FE1CB"),
      list(id = "obs_id",     label = "Observation ID",   hdr = "#534AB7", bg = "#EEEDFE", txt = "#3C3489", bdr = "#CECBF6"),
      list(id = "split",      label = "Train-Test Split", hdr = "#BA7517", bg = "#FAEEDA", txt = "#633806", bdr = "#FAC775"),
      list(id = "weight",     label = "Case Weight",      hdr = "#3B6D11", bg = "#EAF3DE", txt = "#27500A", bdr = "#C0DD97"),
      list(id = "stratifier", label = "Stratifier",       hdr = "#3C3489", bg = "#EEEDFE", txt = "#26215C", bdr = "#AFA9EC"),
      list(id = "sensitive",  label = "Sensitive",        hdr = "#993556", bg = "#FBEAF0", txt = "#72243E", bdr = "#F4C0D1"),
      list(id = "ignore",     label = "Ignore",           hdr = "#5F5E5A", bg = "#F1EFE8", txt = "#444441", bdr = "#D3D1C7")
    )
    
    role_ids <- sapply(ROLES, `[[`, "id")
    
    # ── Internal data with optional ratio_splitted col injected ───────────────────────
    
    get_data <- reactive({
      df <- get_raw(); req(df)
      
      if (isTRUE(input$use_ratio_split)) {
        col_name <- trimws(input$split_col_name %||% "ratio_splitted")
        if (!nzchar(col_name)) col_name <- "ratio_splitted"
        
        n <- nrow(df)
        set.seed(as.integer(input$seed %||% 42))
        train_idx <- sample(seq_len(n), floor(n * input$train_ratio / 100))
        
        df[[col_name]] <- factor(
          ifelse(seq_len(n) %in% train_idx, "Train", "Test"),
          levels = c("Train", "Test")
        )
      }
      df
    })
    
    # ── Split counts hint ────────────────────────────────────────────────────
    
    output$split_counts_ui <- renderUI({
      req(isTRUE(input$use_ratio_split))
      df <- get_data(); req(df)
      col_name <- trimws(input$split_col_name %||% "ratio_splitted")
      if (!col_name %in% names(df)) return(NULL)
      
      n_train <- sum(df[[col_name]] == "Train")
      n_test  <- sum(df[[col_name]] == "Test")
      
      div(
        style = "font-size:12px; color:#185FA5; background:#E6F1FB;
                 border-radius:6px; padding:6px 10px; margin-top:6px;",
        icon("circle-info"),
        HTML(paste0(" Train: <b>", n_train, "</b> &nbsp;|&nbsp; Test: <b>", n_test, "</b>"))
      )
    })
    
    # ── Important vars selector ──────────────────────────────────────────────
    
    observe({
      df <- get_data(); req(df)
      updateSelectizeInput(session, "important_vars",
                           choices  = names(df),
                           selected = isolate(input$important_vars),
                           server   = TRUE)
    })
    
    # ── Assignments state ────────────────────────────────────────────────────
    
    assignments <- reactiveVal(list())
    
    init_assignments <- function(df) {
      vars    <- names(df)
      cur     <- assignments()
      new_out <- setNames(rep("predictor", length(vars)), vars)
      
      # preserve existing assignments for variables still present
      shared <- intersect(names(new_out), names(cur))
      new_out[shared] <- cur[shared]
      
      # auto-assign generated ratio_splitted col to split role
      col_name <- trimws(input$split_col_name %||% "ratio_splitted")
      if (isTRUE(input$use_ratio_split) && col_name %in% vars) {
        new_out[[col_name]] <- "split"
      }
      
      assignments(new_out)
    }
    
    observeEvent(get_data(), {
      req(get_data())
      init_assignments(get_data())
    })
    
    observeEvent(input$reset, {
      req(get_raw())
      df      <- get_raw()
      new_out <- setNames(rep("predictor", ncol(df)), names(df))
      assignments(new_out)
      updateCheckboxInput(session, "use_ratio_split", value = FALSE)
      updateNumericInput(session,  "seed",            value = as.integer(format(Sys.Date(), "%Y")))
      updateSliderInput(session,   "train_ratio",     value = 80)
      updateTextInput(session,     "split_col_name",  value = "ratio_splitted")
      updateSelectizeInput(session, "important_vars", selected = character(0))
      updateTextInput(session,     "custom_title",    value = "")
    })
    
    observeEvent(input$dropped, {
      req(input$dropped)
      info <- input$dropped
      cur  <- assignments()
      cur[[info$var]] <- info$to
      assignments(cur)
    })
    
    # ── Roles UI ─────────────────────────────────────────────────────────────
    
    output$roles_ui <- renderUI({
      req(get_data(), length(assignments()) > 0)
      cur           <- assignments()
      vars          <- names(cur)
      important     <- input$important_vars %||% character(0)
      
      # pill helper
      pill <- function(v, role_id) {
        r   <- if (role_id == "unassigned") NULL else ROLES[[which(role_ids == role_id)]]
        bg  <- if (is.null(r)) "#f1f3f5" else r$bg
        txt <- if (is.null(r)) "#495057" else r$txt
        bdr <- if (is.null(r)) "#dee2e6" else r$bdr
        
        # shield icon for domain-important variables
        icon_html <- if (v %in% important)
          "<i class='fas fa-shield-alt' style='color:#993556;font-size:14px;margin-right:3px;'></i>"
        else ""
        
        tags$div(
          class       = "rv-pill",
          draggable   = "true",
          `data-var`  = v,
          `data-zone` = role_id,
          style       = paste0(
            "display:inline-flex;align-items:center;gap:5px;",
            "padding:4px 10px;border-radius:999px;font-size:12px;font-weight:500;",
            "cursor:grab;user-select:none;border:0.5px solid ", bdr, ";",
            "background:", bg, ";color:", txt, ";margin:2px;"
          ),
          HTML(paste0(icon_html, v))
        )
      }
      
      # unassigned pool
      unassigned_vars <- vars[cur == "unassigned"]
      unassigned_pool <- tags$div(
        tags$p("Unassigned",
               style = "font-size:12px;color:#6c757d;margin-bottom:6px;font-weight:500;"),
        tags$div(
          id    = "zone-unassigned",
          class = "rv-zone",
          style = paste0(
            "display:flex;flex-wrap:wrap;gap:4px;padding:8px;min-height:44px;",
            "background:#f8f9fa;border-radius:8px;border:0.5px solid #dee2e6;",
            "margin-bottom:16px;"
          ),
          if (length(unassigned_vars) > 0)
            lapply(unassigned_vars, pill, role_id = "unassigned")
          else
            tags$span("(all assigned)", style = "font-size:12px;color:#adb5bd;")
        )
      )
      
      # role grid
      title_text <- if (nzchar(trimws(input$custom_title))) input$custom_title else "Assigned Variable Roles"
      
      role_grid <- tagList(
        tags$h4(title_text,
                style = "font-weight:600; margin-bottom:20px; color:#343a40; text-align:center;"),
        tags$div(
          style = "display:grid;grid-template-columns:1fr 1fr;gap:12px;",
          lapply(ROLES, function(r) {
            zone_vars <- vars[cur == r$id]
            tags$div(
              style = "border-radius:10px;border:0.5px solid #dee2e6;overflow:hidden;",
              tags$div(
                r$label,
                style = paste0(
                  "padding:8px 14px;font-size:13px;font-weight:500;",
                  "background:", r$hdr, ";color:white;"
                )
              ),
              tags$div(
                id    = paste0("zone-", r$id),
                class = "rv-zone",
                style = paste0(
                  "display:flex;flex-wrap:wrap;gap:4px;padding:8px;min-height:52px;",
                  "background:white;border-top:0.5px solid #dee2e6;"
                ),
                if (length(zone_vars) > 0)
                  lapply(zone_vars, pill, role_id = r$id)
                else
                  tags$span("Drop here", style = "font-size:12px;color:#adb5bd;")
              )
            )
          })
        )
      )
      
      # drag-and-drop JS
      drag_js <- tags$script(HTML(sprintf("
        (function() {
          var dragging = null, fromZone = null;

          function attach() {
            document.querySelectorAll('.rv-pill').forEach(function(pill) {
              pill.addEventListener('dragstart', function(e) {
                dragging = pill.dataset.var;
                fromZone = pill.dataset.zone;
                e.dataTransfer.effectAllowed = 'move';
              });
              pill.addEventListener('dragend', function() {
                dragging = null; fromZone = null;
              });
            });

            document.querySelectorAll('.rv-zone').forEach(function(zone) {
              zone.addEventListener('dragover', function(e) {
                e.preventDefault();
                e.stopPropagation();
                zone.style.outline = '2px dashed #4a90d9';
              });
              zone.addEventListener('dragleave', function(e) {
                if (!zone.contains(e.relatedTarget)) zone.style.outline = '';
              });
              zone.addEventListener('drop', function(e) {
                e.preventDefault();
                e.stopPropagation();
                zone.style.outline = '';
                var toZone = zone.id.replace('zone-', '');
                if (dragging && fromZone !== toZone) {
                  Shiny.setInputValue('%s', {var: dragging, to: toZone}, {priority: 'event'});
                }
                dragging = null; fromZone = null;
              });
            });
          }

          $(document).on('shiny:value', function(e) {
            if (e.name === '%s') setTimeout(attach, 80);
          });

          setTimeout(attach, 200);
        })();
      ", ns("dropped"), ns("roles_ui"))))
      
      tagList(unassigned_pool, role_grid, drag_js)
    })
    
    # ── Return 4 things ──────────────────────────────────────────────────────
    
    return(list(
      data           = get_data,
      roles          = reactive(assignments()),
      important_vars = reactive(input$important_vars %||% character(0)),
      seed           = reactive(as.integer(input$seed %||% 42))
    ))
    
  })
}