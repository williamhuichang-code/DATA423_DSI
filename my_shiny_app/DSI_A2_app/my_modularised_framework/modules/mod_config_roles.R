# =================================================================================
# mod_config_roles.R
# =================================================================================

# ── UI ───────────────────────────────────────────────────────────────────────

data_roles_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    position = "right",
    sidebarPanel(
      
      # layout
      
      width = 2,
      style = "background-color: #e8f0fe; border-left: 2px solid #a8c0fd; min-height: 100vh; padding-left: 20px;",
      
      # tab notes
      
      div(
        style = "font-size: 13px; color: #343a40; background-color: white;
             padding: 10px; border-left: 4px solid #0d6efd; border-radius: 6px;
             margin-bottom: 12px; box-shadow: 0 1px 2px rgba(0,0,0,0.05);",
        icon("info-circle", style = "color:#0d6efd;"),
        HTML("&nbsp; Tab Note: <br><br> Placeholder for now.")
      ),
      hr(),
      
      # controls
      
      actionButton(
        ns("reset"),
        label = "Reset All",
        icon  = icon("rotate-left"),
        width = "100%"
      ),
      hr(),
      
      # custom title
      
      tags$div(
        style = "margin-bottom:12px;",
        tags$label("Custom panel title:",
                   style = "font-weight:600; font-size:13px; color:#343a40;"),
        textInput(ns("custom_title"), label = NULL,
                  placeholder = "Auto-generated if empty")
      )
      
    ),
    mainPanel(
      width = 10,
      uiOutput(ns("roles_ui"))
    )
  )
}


# ── SERVER ───────────────────────────────────────────────────────────────────

data_roles_server <- function(id, get_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # role definitions
    
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
    
    # state
    
    assignments <- reactiveVal(list())
    
    init_assignments <- function(df) {
      vars <- names(df)
      out  <- setNames(rep("predictor", length(vars)), vars)
      assignments(out)
    }
    
    # observers
    
    observeEvent(get_data(), {
      req(get_data())
      init_assignments(get_data())
    })
    
    observeEvent(input$reset, {
      req(get_data())
      init_assignments(get_data())
    })
    
    observeEvent(input$dropped, {
      req(input$dropped)
      info <- input$dropped
      cur  <- assignments()
      cur[[info$var]] <- info$to
      assignments(cur)
    })
    
    # ui output
    
    output$roles_ui <- renderUI({
      req(get_data(), length(assignments()) > 0)
      cur  <- assignments()
      vars <- names(cur)
      
      # pill helper
      
      pill <- function(v, role_id) {
        r   <- if (role_id == "unassigned") NULL else ROLES[[which(role_ids == role_id)]]
        bg  <- if (is.null(r)) "#f1f3f5" else r$bg
        txt <- if (is.null(r)) "#495057" else r$txt
        bdr <- if (is.null(r)) "#dee2e6" else r$bdr
        tags$div(
          class          = "rv-pill",
          draggable      = "true",
          `data-var`     = v,
          `data-zone`    = role_id,
          style          = paste0(
            "display:inline-flex;align-items:center;gap:5px;",
            "padding:4px 10px;border-radius:999px;font-size:12px;font-weight:500;",
            "cursor:grab;user-select:none;border:0.5px solid ", bdr, ";",
            "background:", bg, ";color:", txt, ";margin:2px;"
          ),
          v
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
              style = paste0("border-radius:10px;border:0.5px solid #dee2e6;overflow:hidden;"),
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
      
      # drag-and-drop js
      
      drag_js <- tags$script(HTML(sprintf("
        (function() {
          var dragging = null, fromZone = null;

          function getZone(el) {
            while (el) {
              if (el.classList && el.classList.contains('rv-zone')) return el;
              el = el.parentElement;
            }
            return null;
          }

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
                if (!zone.contains(e.relatedTarget)) {
                  zone.style.outline = '';
                }
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
    
    # return
    
    return(reactive(assignments()))
    
  })
}