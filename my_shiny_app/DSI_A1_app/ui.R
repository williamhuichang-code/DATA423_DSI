# =================================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =================================================================================

ui <- fluidPage(
  
  # ── GLOBAL UI CONTROLS ─────────────────────────────────────────────────────
  
  # dataset stage selector + privacy lock always visible at the top
  fluidRow(
    
    # dataset stage selector (debug stage only injected when unlocked - see server)
    column(2,
           uiOutput("dataset_selector_ui")   # rendered server-side so debug is gated
    ),
    
    # info button
    column(1,
           actionButton("dataset_info", label = NULL,
                        icon  = icon("circle-info"),
                        style = "margin-top:25px; font-size:20px;
                               color:#0d6efd; background:none;
                               border:none; padding:0;")
    ),
    
    # assignment info and student info
    column(6,
           div(
             style = "margin-top:28px; text-align:center; font-size:24px; font-weight:600; color:#495057;",
             
             "DATA423-26S1 Assignment 1 (EDA with Shiny)",
             br(),
             "William Hui Chang (69051925)"
           )
    ),
    
    # passphrase + lock toggle
    column(3,
           div(style = "margin-top:24px; display:flex; align-items:center; gap:8px;",
               passwordInput("privacy_pass", label = NULL,
                             placeholder = "Passphrase 123 to unlock",
                             width = "200px"),
               actionButton("privacy_unlock", label = NULL, icon = icon("lock"),
                            style = "padding:6px 10px;"),
               uiOutput("privacy_status_ui")
           ))
  ),
  
  hr(),
  
  
  # ── TABS ────────────────────────────────────────────────────────────────────
  
  tabsetPanel(
    
    # ── UI Data Table ─────────────────────────────────────────────────────
    tabPanel("Data Table",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Data Table: <br><br>
                                         Interactive table with filtering, export, and display controls."),
                            hr(),
                            # column to show
                            selectizeInput("dt_cols", "Columns to show:",
                                           choices  = NULL,
                                           multiple = TRUE,
                                           options  = list(placeholder = "All columns shown by default")),
                            selectInput("dt_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            # row count cap
                            sliderInput("dt_row_cap", "Observations (export ignores this cap):",
                                        min = 0, max = nrow(eda_dataset), value = 360, step = 50, width = "100%"),
                            # obs per page
                            selectInput("dt_page_len", "Observations per page:",
                                        choices  = c(10, 20, 25, 50, 100, 200),
                                        selected = 25),
                            # compact style
                            checkboxInput("dt_compact", "Compact (dense) style", value = TRUE),
                            hr(),
                            # variable value filter
                            radioButtons("dt_filter", "Column filters:",
                                         choices  = c("None" = "none", "Top" = "top", "Bottom" = "bottom"),
                                         selected = "none",
                                         inline   = TRUE),
                            # freeze first N columns
                            numericInput("dt_freeze", "Freeze left N columns:", value = 0, min = 0, max = 10),
                            hr(),
                            # instance selection
                            radioButtons("dt_selection", "Instance selection:",
                                         choices  = c("None" = "none", "Single" = "single", "Multiple" = "multiple"),
                                         selected = "single",
                                         inline   = TRUE)
               ),
               mainPanel(width = 9,
                         DT::dataTableOutput("data_table")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI SUMMARY ────────────────────────────────────────────────────────
    
    tabPanel("Summary",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Data Summary: <br><br>
                                         Can select a style to inspect the dataset structure."),
                            hr(),
                            radioButtons("summary_style", "Style:",
                                         choices = c("base R"  = "base",
                                                     "glimpse" = "glimpse",
                                                     "dfSummary"   = "dfsummary"),
                                         selected = "glimpse"),
                            hr(),
                            sidebar_note("My EDA Notes: Some Level of Seriousness in Data Integrity - 
                                         <br><br>
                                         1. missingness and potential reasons
                                         <br>
                                         2. collected, but obvious improper value
                                         <br>
                                         3. seemingly good existing values, but hidden gaps
                                         <br>
                                         4. proper values, but improper grouping logic
                                         <br>
                                         5. proper values, but redundant (justified by primary key)
                                         "),
                            hr(),
                            sidebar_note("My Futher Inference Notes: Some Level of Seriousness in Inference — 
                                         <br><br>
                                         1. five crucial assumptions as independence in obs, linearity, 
                                         normality, constant variance, and no influential outliers
                                         <br>
                                         2. potential multi-labelled y overlapping
                                         <br>
                                         3. independence in features
                                         <br>
                                         4. interactions of features on y response
                                         <br>
                                         5. multilinearity in features
                                         "),
                            hr(),
                            sidebar_note("My Framework Notes: Conceptualised Dataset Stages — 
                                         <br><br>
                                         1. raw dataset
                                         <br>
                                         2. eda dataset (anything related to integrity and patterns)
                                         <br>
                                         3. enriched dataset (add derived cols)
                                         <br>
                                         3. model dataset (remove unnecessary cols)
                                         <br>
                                         4. debug dataset (testing with flag cols and flagging logic)
                                         "),
               ),
               mainPanel(width = 9,
                         # verbatimTextOutput("summary_output")
                         uiOutput("summary_output")
               )
             )
    ), # end of tab panel
    
    
    # ── UI WORD CLOUD ─────────────────────────────────────────────────────
    
    tabPanel("Word Cloud",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Word Cloud: <br><br>
                                         This word cloud helps identify inconsistencies in variable names 
                                         or categorical values, depending on the selected mode.
                                         Typical scenarios are like col name cleaning 
                                         (should check raw_dataset stage instead), distinct variable values 
                                         and y label overlapping"),
                            hr(),
                            checkboxInput("wc_varnames_mode", "Check variable names instead", value = FALSE),
                            hr(),
                            conditionalPanel(
                              condition = "input.wc_varnames_mode == false",
                              selectInput("wc_var", "Categorical variable:", choices = NULL)
                            ),
                            hr(),
                            checkboxInput("wc_case", "Case sensitive", value = TRUE),
                            hr(),
                            radioButtons("wc_split_mode", "Token split mode:",
                                         choices = c(
                                           "None (whole value)"   = "none",
                                           "Individual characters" = "chars",
                                           "Alpha / numeric runs"  = "alphanum"
                                         ),
                                         selected = "none"),
                            hr(),
                            sliderInput("wc_max_words", "Max words to show:",
                                        min = 10, max = 500, value = 360, step = 10),
                            sliderInput("wc_min_freq", "Min frequency:",
                                        min = 1, max = 50, value = 1, step = 1),
                            helpText("Font size is proportional to frequency."),
                            hr(),
                            sliderInput("wc_scale", "Plot scale:",
                                        min = 0.3, max = 3.0, value = 1.0, step = 0.1),
                            hr(),
                            selectInput("wc_palette", "Colour palette:",
                                        choices = c("Dark2", "Set1", "Set2", "Set3",
                                                    "Paired", "Accent", "Spectral"),
                                        selected = "Dark2")
               ),
               mainPanel(width = 9,
                         plotOutput("wc_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI MISSINGNESS ────────────────────────────────────────────────────
    tabPanel("Missingness",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Missingness: <br><br>
                          Visualise missing data patterns across the dataset.
                          Group by a categorical variable to reveal whether 
                          missingness differs across subgroups."),
                            hr(),
                            selectizeInput("ms_vars", "Variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            selectInput("ms_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            radioButtons("ms_mode", "View:",
                                         choices = c("vis_dat (type + missingness)" = "visdat",
                                                     "vis_miss (missingness only)"  = "vismiss"),
                                         selected = "visdat"),
                            hr(),
                            checkboxInput("ms_group_on", "Group by categorical variable", value = TRUE),
                            conditionalPanel(
                              condition = "input.ms_group_on == true",
                              selectInput("ms_group_var", "Grouping variable (primary):", choices = NULL),
                              selectizeInput("ms_group_levels", "Primary levels to include:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels included by default")),
                              hr(),
                              selectInput("ms_group_var2", "Grouping variable (secondary):", choices = NULL),
                              selectizeInput("ms_group_levels2", "Secondary levels to include:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels included by default"))
                            ),
                            hr(),
                            checkboxInput("ms_mcar", "Test for MCAR (Little's test)", value = FALSE),
                            conditionalPanel(
                              condition = "input.ms_mcar == true",
                              sidebar_note("Little's MCAR test: <br><br>
                            H0: data is Missing Completely At Random. 
                            A significant p-value (< 0.05) suggests 
                            missingness is NOT random — i.e. MAR or MNAR.")
                            ),
                            hr(),
                            textInput("ms_title", "Custom plot title:", placeholder = "Auto-generated if empty")
               ),
               mainPanel(width = 9,
                         plotOutput("ms_output", height = "70vh"),
                         conditionalPanel(
                           condition = "input.ms_mcar == true",
                           hr(),
                           h4("Little's MCAR Test Result"),
                           verbatimTextOutput("ms_mcar_output")
                         )
               )
             )
    ), # end of tab panel
    
    
    # ── UI RISING VALUE ───────────────────────────────────────────────────
    
    tabPanel("Rising Value",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Rising Value: <br><br>
                                         This rising value chart is useful for examining continuity.
                                         If a variable is truly continuous, the sorted values should increase smoothly; 
                                         visible gaps or steps may therefore signal suspicious patterns in the data. 
                                         Unlike histograms, rising value charts do not rely on binning (which may hide gaps) 
                                         and can support comparison across multiple variables.
                                         "),
                            hr(),
                            selectizeInput("rv_vars", "Numeric variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            selectInput("rv_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            checkboxInput("rv_omit_na", "Ignore NAs", value = FALSE),
                            hr(),
                            radioButtons("rv_transform", "Transform:",
                                         choices = c("None"         = "none",
                                                     "Centre"       = "center",
                                                     "Standardise"  = "standardise",
                                                     "Normalise"    = "normalise"),
                                         selected = "normalise"),
                            hr(),
                            textInput("rv_title", "Custom plot title:", placeholder = "Auto-generated if empty"),
                            hr(),
                            sliderInput("rv_lwd", "Line width:",
                                        min = 0.2, max = 5, value = 1.6, step = 0.1, width = "100%"),
                            hr(),
                            radioButtons("rv_lty", "Line type:",
                                         choices = c(
                                           "Solid"    = "solid",
                                           "Dashed"   = "dashed",
                                           "Dotted"   = "dotted",
                                           "Dotdash"  = "dotdash",
                                           "Longdash" = "longdash"
                                         ),
                                         selected = "dotdash")
               ),
               mainPanel(width = 9,
                         plotlyOutput("rv_output", height = "80vh")
               )
             )
    ), # end of tab panel
    
    
    # ── UI TABPLOT ────────────────────────────────────────────────────────
    
    tabPanel("Tabplot",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Tabplot: <br><br>
      Visualises distributions and relationships of multiple 
      variables simultaneously, sorted by a target variable. 
      Useful for spotting patterns across many variables at once 
      and identifying how they relate to the outcome Y."),
                            hr(),
                            selectizeInput("tp_vars", "Variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            selectInput("tp_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            checkboxInput("tp_sort_on", "Sort by variable", value = TRUE),
                            conditionalPanel(
                              condition = "input.tp_sort_on == true",
                              selectInput("tp_sortvar", "Sort by variable:", choices = NULL),
                              checkboxInput("tp_decreasing", "Sort descending", value = FALSE)
                            ),
                            hr(),
                            radioButtons("tp_transform", "Transform numeric columns:",
                                         choices = c("None"        = "none",
                                                     "Centre"      = "center",
                                                     "Standardise" = "standardise",
                                                     "Normalise"   = "normalise"),
                                         selected = "normalise"),
                            hr(),
                            sliderInput("tp_nbin", "Number of bins:",
                                        min = 10, max = 500, value = 60, step = 10, width = "100%"),
                            hr(),
                            textInput("tp_title", "Custom plot title:", placeholder = "Auto-generated if empty")
               ),
               mainPanel(width = 9,
                         plotOutput("tp_output", height = "80vh")
               )
             )
    ), # end of tab panel
    
    
    # ── UI BOXPLOT 1 ──────────────────────────────────────────────────────
    
    tabPanel("Boxplot 1",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Boxplot: <br><br>
                        Visualise distributions and outliers across numeric variables.
                        Uses car::Boxplot with automatic outlier labelling by row index.
                        Group by a categorical variable to compare across levels."),
                            hr(),
                            selectizeInput("bx_vars", "Numeric variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            selectInput("bx_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            radioButtons("bx_transform", "Transform:",
                                         choices = c("None"        = "none",
                                                     "Centre"      = "center",
                                                     "Standardise" = "standardise",
                                                     "Normalise"   = "normalise"),
                                         selected = "none"),
                            hr(),
                            sliderInput("bx_coef", "IQR multiplier (outlier criterion):",
                                        min = 0, max = 5, value = 1.5, step = 0.5, width = "100%"),
                            helpText("1.5 = standard Tukey fences. Higher = fewer outliers flagged."),
                            hr(),
                            checkboxInput("bx_group_on", "Group by categorical variable", value = TRUE),
                            conditionalPanel(
                              condition = "input.bx_group_on == true",
                              selectInput("bx_group_var", "Grouping variable:", choices = NULL),
                              selectizeInput("bx_group_levels", "Levels to include:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels included by default"))
                            ),
                            hr(),
                            textInput("bx_title", "Custom plot title:", placeholder = "Auto-generated if empty")
               ),
               mainPanel(width = 9,
                         plotOutput("bx_output", height = "80vh")
               )
             )
    ), # end of tab panel
    
    
    # ── UI BOXPLOT 2 ──────────────────────────────────────────────────────
    
    tabPanel("Boxplot 2",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Boxplot 2 (Interactive): <br><br>
                        Visualise distributions and outliers across numeric variables.
                        Violin mode reveals distribution shape and density.
                        Group by a categorical variable to compare across levels."),
                            hr(),
                            selectizeInput("box_vars", "Numeric variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            selectInput("box_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            radioButtons("box_transform", "Transform:",
                                         choices = c("None"        = "none",
                                                     "Centre"      = "center",
                                                     "Standardise" = "standardise",
                                                     "Normalise"   = "normalise"),
                                         selected = "none"),
                            # hr(),
                            # sliderInput("box_iqr", "IQR multiplier (outlier criterion):",
                            #             min = 0, max = 5, value = 1.5, step = 0.5, width = "100%"),
                            # helpText("1.5 = standard Tukey fences. Higher = fewer outliers flagged."),
                            hr(),
                            checkboxInput("box_violin", "Show as violin", value = TRUE),
                            hr(),
                            checkboxInput("box_group_on", "Group by categorical variable", value = TRUE),
                            conditionalPanel(
                              condition = "input.box_group_on == true",
                              selectInput("box_group_var", "Grouping variable:", choices = NULL),
                              selectizeInput("box_group_levels", "Levels to include:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels included by default"))
                            ),
                            hr(),
                            textInput("box_title", "Custom plot title:", placeholder = "Auto-generated if empty")
               ),
               mainPanel(width = 9,
                         plotlyOutput("box_plot", height = "85vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI MOSAIC ─────────────────────────────────────────────────────────
    
    tabPanel("Mosaic",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Mosaic: <br><br>
                            Visualise dependency between categorical variables.
                            Tile area reflects joint frequency; shading by residuals 
                            reveals where observed counts deviate from independence.
                            Use the Pair Advisor to rank variable combinations 
                            by association strength (Cramér's V)."),
                            hr(),
                            sliderInput("mosaic_max_levels", "Max levels per variable (cardinality filter):",
                                        min = 2, max = 10, value = 4, step = 1, width = "100%"),
                            helpText("Variables with more unique levels than this are excluded from the dropdowns."),
                            hr(),
                            uiOutput("mosaic_x_ui"),
                            uiOutput("mosaic_y_ui"),
                            uiOutput("mosaic_z_ui"),
                            hr(),
                            checkboxInput("mosaic_shade", "Shade (colour by residuals)", value = TRUE),
                            hr(),
                            sliderInput("mosaic_rot_labels", "Rotate Variable 1 labels (degrees):",
                                        min = 0, max = 360, value = 0, step = 15, width = "100%"),
                            checkboxInput("mosaic_abbreviate", "Abbreviate labels", value = TRUE),
                            sliderInput("mosaic_fontsize", "Label font size:",
                                        min = 6, max = 24, value = 10, step = 1, width = "100%"),
                            hr(),
                            textInput("mosaic_title", "Custom plot title:", 
                                      placeholder = "Auto-generated if empty"),
                            hr(),
                            sidebar_note("Pair Advisor: <br><br>
                            Ranks variable combinations by Cramér's V (effect size).
                            Higher = stronger association.
                            0.1 = weak, 0.3 = moderate, 0.5+ = strong.
                            3-way score = average Cramér's V across all 3 pairs in the trio.
                            <br><br>
                            Click a row in the results table to load variables into the plot."),
                            hr(),
                            radioButtons("pairs_way", "Combinations:",
                                         choices = c("2-way", "3-way"), selected = "2-way",
                                         inline = TRUE),
                            numericInput("pairs_top", "Show top N:", value = 15, min = 5, max = 200),
                            actionButton("pairs_search", "Find Pairs", 
                                         icon = icon("search"), width = "100%")
               ),
               mainPanel(width = 9,
                         plotOutput("mosaic_plot", height = "70vh"),
                         conditionalPanel(
                           condition = "input.pairs_search > 0",
                           hr(),
                           h4("Pair Advisor Results — ranked by Cramér's V"),
                           DTOutput("pairs_table")
                         )
               )
             )
    ),  # end of tab panel
    
    
    # ── UI GGPAIRS ────────────────────────────────────────────────────────
    
    tabPanel("GGPairs",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("GGPairs: <br><br>
                                         This GGPairs graph provides a quick overview of pairwise relationships, 
                                         correlations, and marginal density distributions across multiple 
                                         variables, allowing potential suspicious relationships, outliers, or 
                                         structural irregularities in the data to be detected early."),
                            hr(),
                            selectizeInput("gg_vars", "Variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            selectInput("gg_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            checkboxInput("gg_group_on", "Group by levels", value = TRUE),
                            conditionalPanel(
                              condition = "input.gg_group_on == true",
                              selectInput("gg_group_var", "Grouping variable:", choices = NULL),
                              selectizeInput("gg_group_levels", "Levels of Interest:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels shown by default"))
                            ),   # conditionalPanel closed here
                            hr(),
                            textInput("gg_title", "Custom plot title:", placeholder = "Auto-generated if empty"),
                            hr(),
                            actionButton("gg_run", "Plot", icon = icon("play"), width = "100%"),
                            helpText("Select variables then click Plot. Large selections may be slow."),
               ),
               mainPanel(width = 9,
                         plotOutput("gg_output", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI CORRELATION HEATMAP ────────────────────────────────────────────
    
    tabPanel("Heatmap",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Correlation Heatmap (Corrgram): <br><br>
                                 Pairwise correlations visualised as a corrgram.
                                 Pie panels show direction and magnitude above the diagonal,
                                 shaded panels below. Collinearity threshold trims
                                 highly correlated variables greedily before plotting."),
                            hr(),
                            selectizeInput("hm_vars", "Numeric variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            selectInput("hm_preset", "Quick variable preset:", choices = NULL),
                            hr(),
                            selectInput("hm_cor", "Correlation method:",
                                        choices  = c("Pearson"  = "pearson",
                                                     "Spearman" = "spearman",
                                                     "Kendall"  = "kendall"),
                                        selected = "spearman"),
                            hr(),
                            selectInput("hm_order", "Variable ordering:",
                                        choices  = c("Original"               = "FALSE",
                                                     "AOE (Eigenvector)"      = "TRUE",
                                                     "HC (Hierarchical)"      = "HC",
                                                     "OLO (Optimal Leaf)"     = "OLO"),
                                        selected = "HC"),
                            hr(),
                            checkboxInput("hm_abs", "Absolute correlation (abs)", value = TRUE),
                            checkboxInput("hm_missing", "Missingness correlation (NAs)", value = FALSE),
                            hr(),
                            textInput("hm_title", "Custom plot title:", placeholder = "Auto-generated if empty")
               ),
               mainPanel(width = 9,
                         plotOutput("hm_output", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI COMING SOON ─────────────────────────────────────────────────────
    
    tabPanel("Coming Soon",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Note: <br><br>This feature is under development."),
                            hr(),
                            selectInput("x_var", "Variable:", choices = NULL)
               ),
               mainPanel(width = 9,
                         plotOutput("x_output", height = "85vh")
               )
             )
    ), # end of tab panel
    
    
    # ── UI R CONSOLE ───────────────────────────────────────────────────────
    
    # the entire tab body is rendered server-side to stay hidden when locked
    #   for myself use only (debugging / testing / developing)
    #   reason is I cant access R terminal when using shiny app lol
    #   so, kinda cool to make this anyway :D
    
    tabPanel("R Console", uiOutput("rconsole_body_ui")) # end of tab panel
    
    
  ) # end tabsetPanel
)   # end fluidPage