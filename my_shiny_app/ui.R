# =============================================================================
# ui.R
# Tabbed layout — one tab per visualisation type.
# Add your name below.
# =============================================================================

ui <- navbarPage(
  title = "DATA423 Assignment 1 — [Your Name]",
  theme = NULL,   # swap in shinythemes::shinytheme("flatly") if you install it

  # ── 0. DATA OVERVIEW ────────────────────────────────────────────────────────
  tabPanel("Overview",
    fluidPage(
      h3("Dataset Overview"),
      fluidRow(
        column(4, wellPanel(
          h4("Dataset Dimensions"),
          verbatimTextOutput("overview_dim"),
          h4("Variable Types"),
          verbatimTextOutput("overview_types")
        )),
        column(8,
          h4("Summary Statistics"),
          verbatimTextOutput("overview_summary")
        )
      )
    )
  ),

  # ── 1. MOSAIC CHART ─────────────────────────────────────────────────────────
  tabPanel("Mosaic",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Controls"),
        # Only factor columns make sense in a mosaic
        selectInput("mosaic_x", "Variable 1 (X):",
                    choices = NULL),   # populated in server
        selectInput("mosaic_y", "Variable 2 (Y):",
                    choices = NULL),
        checkboxInput("mosaic_shade", "Shade residuals", value = TRUE),
        hr(),
        helpText("Mosaic plots show relationships between two categorical variables.")
      ),
      mainPanel(
        plotOutput("mosaic_plot", height = "500px"),
        hr(),
        p(em("Figure 1: Mosaic chart of two selected categorical variables. Cell area is proportional
              to frequency; shading highlights Pearson residuals (blue = more than expected,
              red = fewer than expected)."))
      )
    )
  ),

  # ── 2. GGPAIRS ──────────────────────────────────────────────────────────────
  tabPanel("GGPairs",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Controls"),
        radioButtons("ggpairs_group", "Variable group:",
                     choices  = c("Metadata (non-sensor numerics)" = "meta",
                                  "Sensors 1–15"  = "sens_a",
                                  "Sensors 16–30" = "sens_b"),
                     selected = "meta"),
        selectInput("ggpairs_colour", "Colour by:",
                    choices  = NULL),   # populated in server (factor cols)
        checkboxInput("ggpairs_smooth", "Add smoother", value = FALSE),
        hr(),
        helpText("Tip: ggpairs on all 44 columns is unusably slow — use the groups above.")
      ),
      mainPanel(
        plotOutput("ggpairs_plot", height = "600px"),
        hr(),
        p(em("Figure 2: GGally ggpairs plot for the selected variable group, coloured by the
              chosen factor. The diagonal shows distributions; upper/lower panels show
              pairwise relationships and correlations."))
      )
    )
  ),

  # ── 3. CORRELATION PLOT ─────────────────────────────────────────────────────
  tabPanel("Correlation",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Controls"),
        selectInput("corr_vars", "Variable group:",
                    choices  = c("Metadata numerics" = "meta",
                                 "Sensors 1–15"      = "sens_a",
                                 "Sensors 16–30"     = "sens_b",
                                 "All numeric"       = "all"),
                    selected = "meta"),
        selectInput("corr_type", "Panel type:",
                    choices  = c("shade", "pie", "ellipse", "number"),
                    selected = "shade"),
        checkboxInput("corr_abs", "Use |correlation| (absolute)", value = FALSE),
        hr(),
        helpText("Blue = positive correlation, red = negative.")
      ),
      mainPanel(
        plotOutput("corr_plot", height = "550px"),
        hr(),
        p(em("Figure 3: Corrgram of numeric variables. Each cell encodes the pairwise
              Pearson correlation using the chosen panel type."))
      )
    )
  ),

  # ── 4. MISSINGNESS ──────────────────────────────────────────────────────────
  tabPanel("Missingness",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Controls"),
        sliderInput("miss_thresh", "Highlight if missing > (%):",
                    min = 0, max = 100, value = 5, step = 1),
        checkboxInput("miss_cluster", "Cluster rows/cols", value = FALSE),
        hr(),
        helpText("Shows which cells in the dataset are NA.")
      ),
      mainPanel(
        plotOutput("miss_plot", height = "500px"),
        hr(),
        p(em("Figure 4: visdat vis_miss chart. Grey cells are present; black cells are missing.
              The percentage missing per column is shown on the x-axis."))
      )
    )
  ),

  # ── 5. BOXPLOT ──────────────────────────────────────────────────────────────
  tabPanel("Boxplot",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Controls"),
        selectInput("box_vars", "Variables to plot:",
                    choices  = NULL, multiple = TRUE),   # numeric cols
        sliderInput("box_coef", "Outlier threshold (IQR multiplier):",
                    min = 0.5, max = 5, value = 1.5, step = 0.5),
        checkboxInput("box_center", "Centre (subtract mean)", value = FALSE),
        checkboxInput("box_scale",  "Scale (divide by SD)",   value = FALSE),
        hr(),
        helpText("Uses car::Boxplot — outlier labels are shown automatically.")
      ),
      mainPanel(
        plotOutput("box_plot", height = "500px"),
        hr(),
        p(em("Figure 5: Boxplot of selected numeric variables. The IQR multiplier slider
              controls how aggressively points are flagged as outliers."))
      )
    )
  ),

  # ── 6. RISING ORDER CHART ───────────────────────────────────────────────────
  tabPanel("Rising Order",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Controls"),
        selectInput("rise_var", "Variable:", choices = NULL),   # numeric cols
        checkboxInput("rise_points", "Show points", value = TRUE),
        checkboxInput("rise_gaps",   "Highlight gaps (>3 SD jump)", value = TRUE),
        hr(),
        helpText("Values sorted ascending. Sudden jumps indicate gaps or outliers in the
                 continuous range.")
      ),
      mainPanel(
        plotOutput("rise_plot", height = "500px"),
        hr(),
        p(em("Figure 6: Rising-order (rank) chart for the selected variable. Values are sorted
              from smallest to largest; unusual jumps suggest gaps or outliers."))
      )
    )
  ),

  # ── 7. DATA TABLE ───────────────────────────────────────────────────────────
  tabPanel("Data Table",
    fluidPage(
      fluidRow(
        column(12,
          h4("Interactive Data Listing"),
          checkboxGroupInput("dt_cols", "Show columns:",
                             choices  = NULL,   # populated in server
                             inline   = TRUE),
          DT::dataTableOutput("data_table"),
          hr(),
          p(em("Table 1: Interactive listing of the dataset. Use the search box and column
                filters to explore individual records."))
        )
      )
    )
  ),

  # ── 8. MODEL (preview for Assignments 2 & 3) ────────────────────────────────
  tabPanel("Model [A2/A3 Preview]",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Simple Linear Regression"),
        selectInput("mod_y",  "Response (Y):",   choices = NULL),   # numeric
        selectInput("mod_x",  "Predictor (X):",  choices = NULL),   # numeric
        checkboxInput("mod_ci",    "Show confidence interval",   value = TRUE),
        checkboxInput("mod_resid", "Show residual plot",         value = TRUE),
        hr(),
        helpText("This tab is a placeholder for the modelling work in Assignments 2 & 3.
                 A2 will use glmnet (regularised regression);
                 A3 will extend to multiple methods via caret.")
      ),
      mainPanel(
        h4("Scatter + Regression Line"),
        plotOutput("mod_scatter", height = "350px"),
        conditionalPanel("input.mod_resid",
          h4("Residual Plot"),
          plotOutput("mod_resid_plot", height = "300px")
        ),
        hr(),
        verbatimTextOutput("mod_summary"),
        hr(),
        p(em("Figure 7: Simple linear regression of the selected response on the selected
              predictor. This tab will evolve into the glmnet / caret pipeline in A2 & A3."))
      )
    )
  )

)  # end navbarPage
