# =================================================================================
# ui.R
# Tabbed layout — one tab per visualisation style.
# =================================================================================

ui <- fluidPage(
  
  # ── GLOBAL UI CONTROLS ─────────────────────────────────────────────────────
  
  # dataset stage selector + privacy lock always visible at the top
  fluidRow(
    
    # dataset stage selector (debug stage only injected when unlocked — see server)
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
    
    tabPanel("Data Table",   DT::dataTableOutput("data_table")),  # end of tab panel
    
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
                            sidebar_note("My EDA Notes 1: Some Level of Seriousness in Data Integrity — 
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
                            sidebar_note("My EDA Notes 2: Some Level of Seriousness in Inference — 
                                         <br><br>
                                         a. potential multi-labelled y overlapping
                                         <br>
                                         b. the assumed distribution of y response
                                         <br>
                                         c. interactions of features on y response
                                         <br>
                                         d. dependent features (multilinearity)
                                         "),
                            hr(),
                            sidebar_note("My EDA Notes 3: Conceptualised Dataset Stages — 
                                         <br><br>
                                         1. raw dataset
                                         <br>
                                         2. enriched dataset (add derived cols)
                                         <br>
                                         3. model dataset (remove unnecessary cols)
                                         <br>
                                         4. debug dataset (with flag cols and flagging logic)
                                         "),
               ),
               mainPanel(width = 9,
                         verbatimTextOutput("summary_output")
               )
             )
    ), # end of tab panel
    
    
    # ── UI WORD CLOUD ─────────────────────────────────────────────────────
    
    tabPanel("Word Cloud",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Col Names & Missingness Cooccurrence & Improper Collecting: <br><br>
                                         This word cloud helps identify inconsistencies in variable names 
                                         or categorical values, depending on the selected mode.
                                         Typical scenarios are like col name cleaning, distinct variable values 
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
    
    
    # ── UI VIS MISS ───────────────────────────────────────────────────────
    
    tabPanel("Vis Miss",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Missingness Overview: <br><br>
                     vis_miss gives a full-dataset pixel view of where 
                     NAs occur. Each column is a variable, each row is 
                     an observation. Black = missing, grey = present."),
                            hr(),
                            selectizeInput("vmiss_cols", "Columns to include:",
                                           choices  = NULL,
                                           multiple = TRUE,
                                           options  = list(placeholder = "Default: all columns")),
                            hr(),
                            checkboxInput("vmiss_cluster", "Cluster rows by missingness pattern", value = FALSE),
                            checkboxInput("vmiss_sort",    "Sort columns by missingness %",        value = FALSE),
                            hr(),
                            sliderInput("vmiss_text_size", "Axis text size:",
                                        min = 4, max = 24, value = 12, step = 1),
                            hr(),
                            checkboxInput("vmiss_group_on", "Facet by categorical variable", value = FALSE),
                            conditionalPanel(
                              condition = "input.vmiss_group_on == true",
                              selectInput("vmiss_group_var", "Group by:", choices = NULL),
                              selectizeInput("vmiss_group_levels", "Show levels:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels shown by default"))
                              )
                            ),
               mainPanel(width = 9,
                         plotOutput("vmiss_plot", height = "85vh")
                         )
               )
    ),  # end of tab panel
    
    
    # ── UI UPSET ──────────────────────────────────────────────────────────
    
    tabPanel("UpSet",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Missingness and General Content Exploring: <br><br>The UpSet plot 
                                     helps identify the number of missing values across variables, 
                                     as well as patterns of missingness that occur together."),
                            hr(),
                            
                            # ~~ column selector ~~
                            selectizeInput("upset_cols", "Columns to include:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            
                            # ~~ anomaly detectors ~~
                            tags$label("Detect as missing:"),
                            checkboxInput("upset_det_na",    "True NA in R (is.na)",     value = TRUE),
                            checkboxInput("upset_det_empty", "Whitespace / Empty string",  value = TRUE),
                            
                            # preset pseudo-NA strings
                            checkboxGroupInput("upset_det_text", "Text pseudo-NAs:",
                                               choices  = c("na", "n/a", "null", "none", "nan", "nil", "-", "?"),
                                               selected = NULL,
                                               inline   = TRUE),
                            
                            # preset sentinel numbers
                            checkboxGroupInput("upset_det_sentinel", "Sentinel numbers:",
                                               choices  = c("9999", "-9999", "999", "-999", "0"),
                                               selected = NULL,
                                               inline   = TRUE),
                            hr(),
                            
                            # custom numbers
                            textInput("upset_custom_num", "Custom numeric sentinels:",
                                      placeholder = "e.g.  99, -1, 9999999"),
                            
                            # negative numbers (numeric columns only)
                            checkboxInput("upset_det_negative", "Flag all negative numbers", value = FALSE),
                            
                            # custom text
                            textInput("upset_custom_text", "Custom text values:",
                                      placeholder = "e.g.  unknown, missing, tbd"),
                            
                            # case sensitivity toggle
                            checkboxInput("upset_case", "Case sensitive (text matching)", value = FALSE),
                            hr(),
                            
                            # ~~ display options ~~
                            numericInput("upset_top", "Show top N combinations:", value = 20, min = 1),
                            radioButtons("upset_sort", "Sort bars by:",
                                         choices  = c("Descending" = "desc", "Ascending" = "asc"),
                                         selected = "desc")
               ),
               mainPanel(width = 9,
                         plotlyOutput("upset_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI LOLLIPOP ───────────────────────────────────────────────────────
    
    tabPanel("Lollipop",
             sidebarLayout(
               sidebarPanel(width = 3,
                            
                            sidebar_note("Missingness and Reasoning: <br><br>This lollipop chart helps 
                                         explore potential reasons for missingness in the selected 
                                         variable by examining it across different contextual groups."),
                            hr(),
                            
                            textInput("vc_value", "Value to count:",
                                      value = "NA",
                                      placeholder = "e.g. Yes, High, NA"),
                            
                            checkboxInput("vc_case", "Case sensitive", value = FALSE),
                            hr(),
                            
                            selectizeInput("vc_cols", "Search in columns:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            
                            hr(),
                            checkboxInput("vc_group_on", "Group by variable", value = FALSE),
                            conditionalPanel(
                              condition = "input.vc_group_on == true",
                              selectInput("vc_group_var", "Group by:", choices = NULL),
                              selectizeInput("vc_group_levels", "Show levels:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels shown by default"))
                            ),
                            hr(),
                            
                            selectInput("vc_metric", "Show as:",
                                        choices = c("Raw Count" = "count", "Percentage (%)" = "pct")),
                            
                            sliderInput("vc_min_count", "Min count to display:",
                                        min = 0, max = 50, value = 0, step = 1),
                            hr(),
                            
                            radioButtons("vc_sort", "Sort by:",
                                         choices  = c("Column order"     = "col",
                                                      "Ascending value"  = "asc",
                                                      "Descending value" = "desc"),
                                         selected = "desc"),
               ),
               mainPanel(width = 9,
                         plotlyOutput("vc_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI RISING VALUE ───────────────────────────────────────────────────
    
    tabPanel("Rising Value",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Complete but Improper Value: <br><br>
                                         This rising value chart helps identify incorrectly collected or 
                                         inconsistent numeric values.
                                         "),
                            selectizeInput("rising_var", "Select numeric variable:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            sliderInput("rising_lwd", "Line width:",
                                        min = 0.2, max = 5, value = 1.6, step = 0.1, width = "100%"),
                            hr(),
                            radioButtons("rising_lty", "Line type:",
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
                         plotlyOutput("rising_output", height = "85vh")
               )
             )
    ), # end of tab panel
    
    
    # ── UI GGPAIRS ────────────────────────────────────────────────────────
    
    tabPanel("GGPairs",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Suspicious Relationship and Density: <br><br>
                                         This GGPairs graph is useful for quickly inspecting 
                                         pairwise relationships, correlations, and marginal 
                                         density distributions across multiple variables."),
                            hr(),
                            selectizeInput("gg_vars", "Variables to plot:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            checkboxInput("gg_group_on", "Group by variable", value = FALSE),
                            conditionalPanel(
                              condition = "input.gg_group_on == true",
                              selectInput("gg_group_var", "Group by:", choices = NULL),
                              selectizeInput("gg_group_levels", "Show levels:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels shown by default"))
                            ),   # conditionalPanel closed here
                            hr(),
                            actionButton("gg_run", "Plot", icon = icon("play"), width = "100%"),
                            helpText("Select variables then click Plot. Large selections may be slow.")
               ),
               mainPanel(width = 9,
                         plotOutput("gg_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI BOXPLOT 1 ──────────────────────────────────────────────────────
    
    tabPanel("Boxplot 1",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Distribution and Improper Value: <br><br>
                                         Boxplots detect outliers and compare medians. 
                                         Violin plots reveal distribution shape and density. 
                                         Grouping by categorical variables shows how distributions 
                                         vary across categories."),
                            hr(),
                            selectizeInput("box_vars", "Numeric variables:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            hr(),
                            checkboxInput("box_group_on", "Group by variable", value = FALSE),
                            conditionalPanel(
                              condition = "input.box_group_on == true",
                              selectInput("box_group_var", "Group by:", choices = NULL),
                              selectizeInput("box_group_levels", "Show levels:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels shown by default"))
                            ),
                            hr(),
                            sliderInput("box_iqr",
                                        HTML("Outlier threshold &nbsp;<small>(IQR multiplier)</small>"),
                                        min   = 0,
                                        max   = 5,
                                        value = 1.5,
                                        step  = 0.1),
                            helpText("Whiskers extend to the furthest point within",
                                     "± multiplier × IQR from the box edges.",
                                     "Points beyond are plotted individually."),
                            hr(),
                            checkboxGroupInput("box_transform",
                                               "Standardisation:",
                                               choices  = c("Center (subtract mean)" = "center",
                                                            "Scale (divide by SD)"   = "scale"),
                                               selected = NULL),
                            hr(),
                            checkboxInput("box_violin", "Show as violin", value = FALSE),
               ),
               mainPanel(width = 9,
                         plotlyOutput("box_plot", height = "85vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI BOXPLOT 2 ──────────────────────────────────────────────────────
    
    tabPanel("Boxplot 2",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Boxplot 2: <br><br>
                                     car::Boxplot alternative."),
                            hr(),
                            
                            selectizeInput("bp2_vars", "Numeric variables:",
                                           choices  = NULL,
                                           selected = paste0("Sensor", 1:30),
                                           multiple = TRUE),
                            hr(),
                            
                            checkboxInput("bp2_group_on", "Group by variable", value = FALSE),
                            conditionalPanel(
                              condition = "input.bp2_group_on == true",
                              selectInput("bp2_group_var", "Group by:", choices = NULL),
                              selectizeInput("bp2_group_levels", "Show levels:",
                                             choices  = NULL,
                                             multiple = TRUE,
                                             options  = list(placeholder = "All levels shown by default"))
                            ),
                            hr(),
                            
                            sliderInput("bp2_iqr",
                                        HTML("Outlier threshold &nbsp;<small>(IQR multiplier)</small>"),
                                        min   = 0,
                                        max   = 5,
                                        value = 1.5,
                                        step  = 0.1),
                            helpText("Whiskers extend to the furthest point within",
                                     "± multiplier × IQR from the box edges.",
                                     "Points beyond are plotted individually."),
                            hr(),
                            
                            checkboxGroupInput("bp2_transform",
                                               "Standardisation:",
                                               choices  = c("Center (subtract mean)" = "center",
                                                            "Scale (divide by SD)"   = "scale"),
                                               selected = NULL)
                            ),
               mainPanel(width = 9,
                         plotOutput("bp2_plot", height = "85vh"),
                         uiOutput("bp2_warning")
               )
             )
    ), # end of tab panel
    
    
    # ── UI Q-Q PLOT ───────────────────────────────────────────────────────
    
    tabPanel("Q-Q Plot",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("The Distribution of Y Response: <br><br>
                                         Q-Q plot evaluates distributional assumptions. 
                                         Particularly useful for assessing the Y response before modeling."),
                            hr(),
                            selectInput("qq_var", "Select Variable:", choices = NULL),
                            selectInput("qq_dist", "Distribution:", 
                                        choices = c("Normal" = "norm", 
                                                    "Exponential" = "exp", 
                                                    "Log-normal" = "lnorm", 
                                                    "Gamma" = "gamma", 
                                                    "Weibull" = "weibull")),
                            hr(),
                            h4("Distribution Parameters"),
                            uiOutput("qq_params"),
                            hr(),
                            checkboxInput("qq_line", "Show Reference Line", value = TRUE)
               ),
               mainPanel(width = 9,
                         plotlyOutput("qq_plot", height = "80vh")
               )
             )
    ), # end of tab panel
    
    # ── UI INTERACTION PLOT ───────────────────────────────────────────────
    
    tabPanel("Interaction Plot",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Feature Interactions on Y: <br><br>
                            Select a continuous predictor and a moderator variable to see how 
                            their interaction affects Y. For example, season could change how 
                                         the cyclic month signal influences Y."),
                            hr(),
                            selectInput("ip_y", "Y response:", choices = NULL),
                            hr(),
                            selectInput("ip_x", "X predictor:", choices = NULL),
                            hr(),
                            checkboxInput("ip_use_mod", "Use moderator", value = TRUE),
                            conditionalPanel(
                              condition = "input.ip_use_mod == true",
                              selectInput("ip_mod", "Moderator (colour/facet):", choices = NULL),
                              hr(),
                              radioButtons("ip_mod_type", "Moderator type:",
                                           choices = c("Categorical" = "cat", "Continuous (binned)" = "cont"),
                                           selected = "cat"),
                              conditionalPanel(
                                condition = "input.ip_mod_type == 'cont'",
                                sliderInput("ip_bins", "Number of bins:", min = 2, max = 5, value = 3, step = 1)
                              ),
                              hr(),
                              checkboxInput("ip_facet", "Facet instead of colour", value = TRUE)
                            ), 
                            hr(),
                            checkboxInput("ip_se", "Show confidence ribbon", value = TRUE)
               ),
               mainPanel(width = 9,
                         plotlyOutput("ip_plot", height = "80vh")
               )
             )
    ), # end of tab panel
    
    
    # ── UI MOSAIC ─────────────────────────────────────────────────────────
    
    tabPanel("Mosaic",
             sidebarLayout(
               sidebarPanel(width = 3,   # 3/12 = 25%, narrow sidebar
                            sidebar_note("Feature Dependency: <br><br>
                                         This Mosaic Plot is good for exploring dependency 
                                         between categorical features."),
                            hr(),
                            # side bar for mosaic plot controls
                            uiOutput("mosaic_x_ui"),
                            uiOutput("mosaic_y_ui"),
                            uiOutput("mosaic_z_ui"),
                            checkboxInput("mosaic_shade", "Shade (colour by residuals)", value = TRUE),
                            hr(),
                            
                            # font adjusting
                            sliderInput("mosaic_rot_labels", "Rotate Variable 1 labels (degrees):",
                                        min = 0, max = 360, value = 90, step = 15),
                            checkboxInput("mosaic_abbreviate", "Abbreviate labels", value = TRUE),
                            sliderInput("mosaic_fontsize", "Label font size:",
                                        min = 6, max = 24, value = 12, step = 1),
                            hr(),
                            
                            # side bar for mosaic pair advisor controls
                            strong("Pair Advisor"),
                            br(),
                            helpText("Ranks variable combinations by Cramér's V (effect size). Higher = stronger association."),
                            br(),
                            radioButtons("pairs_way", "Combinations:",
                                         choices = c("2-way", "3-way"), selected = "2-way",
                                         inline = TRUE),
                            numericInput("pairs_top", "Show top N:", value = 15, min = 5, max = 200),
                            actionButton("pairs_search", "Find Pairs", icon = icon("search"), width = "100%"),
                            br(), br(),
                            helpText("Click a row to load variables into the plot above.")
               ),
               
               mainPanel(
                 width = 9,     # must add up to 12
                 
                 # mosaic plotting
                 plotOutput("mosaic_plot", height = "90vh"),
                 
                 hr(),
                 
                 # pair advisor results (hidden until Search clicked)
                 conditionalPanel(
                   condition = "input.pairs_search > 0",
                   h4("Pair Advisor Results — ranked by Cramér's V"),
                   helpText("Cramér's V: 0.1 = weak, 0.3 = moderate, 0.5+ = strong. Not inflated by sample size."),
                   helpText("3-way score = average Cramér's V across the 3 possible pairs within the trio."),
                   DTOutput("pairs_table")
                 )
               )
             )
    ),  # end of tab panel
    
    
    # ── UI CORRELATION HEATMAP 1 ──────────────────────────────────────────
    
    tabPanel("Heatmap 1",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Note: <br><br>
                                         This correlation heatmap is good for diagnosing multicollinearity 
                                         and detect highly correlated predictors before modeling."),
                            hr(),
                            
                            # variable selection
                            selectizeInput("cor_vars", "Numeric variables:",
                                           choices  = NULL,
                                           multiple = TRUE,
                                           options  = list(placeholder = "Default: all numeric")),
                            hr(),
                            
                            # method
                            radioButtons("cor_method", "Correlation method:",
                                         choices  = c("Pearson"  = "pearson",
                                                      "Spearman" = "spearman",
                                                      "Kendall"  = "kendall"),
                                         selected = "pearson"),
                            hr(),
                            
                            # threshold filter
                            sliderInput("cor_threshold", "Collinearity threshold:",
                                        min   = 0,
                                        max   = 1,
                                        value = 1,      # default = keep all
                                        step  = 0.01),
                            helpText("1.00 = keep all variables.",
                                     "0.80 = drop variables with |r| > 0.80 (pairwise greedy).",
                                     "0.00 = extremely strict, keeps only uncorrelated variables."),
                            hr(),
                            
                            # NA handling
                            selectInput("cor_na", "Handle NAs:",
                                        choices  = c("pairwise.complete.obs", "complete.obs"),
                                        selected = "pairwise.complete.obs")
               ),
               
               mainPanel(width = 9,
                         plotlyOutput("cor_plot_gg", height = "90vh"),
                         hr(),
                         h4("Correlation Matrix"),
                         DTOutput("cor_table")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI CORRELATION HEATMAP 2 ──────────────────────────────────────────
    
    tabPanel("Heatmap 2",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Correlation Heatmap (Corrgram version): <br><br>
                                     A visual alternative to Heatmap 1 using the corrgram library."),
                            hr(),
                            
                            selectizeInput("cg_vars", "Numeric variables:",
                                           choices  = NULL,
                                           multiple = TRUE,
                                           options  = list(placeholder = "Default: all numeric")),
                            hr(),
                            
                            radioButtons("cg_cor", "Correlation method:",
                                         choices  = c("Pearson"  = "pearson",
                                                      "Spearman" = "spearman",
                                                      "Kendall"  = "kendall"),
                                         selected = "pearson"),
                            hr(),
                            
                            sliderInput("cg_threshold", "Collinearity threshold:",
                                        min = 0, max = 1, value = 1, step = 0.01),
                            helpText("1.00 = keep all variables.",
                                     "0.80 = drop variables with |r| > 0.80 (pairwise greedy).",
                                     "0.00 = extremely strict.")
               ),
               mainPanel(width = 9,
                         plotOutput("cg_plot", height = "85vh"),
                         hr(),
                         h4("Variable Index Legend"),
                         DTOutput("cg_legend_table")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI HDBSCAN ────────────────────────────────────────────────────────
    
    tabPanel("HDBSCAN",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Hierarchical Clustering: <br><br>
                                     HDBSCAN identifies dense clusters and flags sparse 
                                     observations as outliers. The hierarchy plot shows 
                                     how clusters form and persist across eps values."),
                            hr(),
                            
                            selectizeInput("hdb_vars", "Variables (numeric):",
                                           choices  = NULL,
                                           multiple = TRUE,
                                           options  = list(placeholder = "Select numeric variables")),
                            hr(),
                            
                            numericInput("hdb_minpts", "minPts:",
                                         value = 5, min = 2, max = 100, step = 1),
                            helpText("Minimum cluster size. Higher = fewer, denser clusters."),
                            hr(),
                            
                            checkboxInput("hdb_scale", "Scale variables before clustering", value = TRUE),
                            hr(),
                            
                            actionButton("hdb_run", "Run HDBSCAN", icon = icon("play"), width = "100%"),
                            helpText("Select variables then click Run. Large selections may be slow.")
               ),
               mainPanel(width = 9,
                         
                         # summary stat row
                         fluidRow(
                           column(3,
                                  div(style = "margin-top:10px;",
                                      h2(textOutput("hdb_n_clusters")),
                                      p("Number of clusters"))),
                           column(3,
                                  div(style = "margin-top:10px;",
                                      h2(textOutput("hdb_n_outliers")),
                                      p("Number of outliers")))
                         ),
                         
                         hr(),
                         
                         # base R hierarchy plot
                         plotOutput("hdb_plot", height = "70vh")
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


