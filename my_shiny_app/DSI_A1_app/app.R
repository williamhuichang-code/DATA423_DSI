# =================================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(shinyAce)  # R console ace editor
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
library(paletteer) # global colour theme
library(colorspace)

# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

# ·· SECURITY LOCK ························································

# What the lock controls (all three require authentication):
#   PRIVATE_COLS  — hidden in all dataset views until unlocked (for stakeholder or authentised users)
#   R Console tab — completely hidden until unlocked (for user or developer me lol)
#   Debug Dataset — only appears in the dataset dropdown when unlocked (for developer me lol)

# # unlock pw
UNLOCK_PASSPHRASE <- "123"

# columns to hide from unauthenticated users
PRIVATE_COLS <- c("ID")


# ·· GLOBAL COL ORDERING ··················································

# ~~ pre-defined global column order ~~

# global master column order, that all datasets could follow 

# Note:
#   every time when I derived a col to enrich, I should add it here in a position
#   it will also skip cols that don't exist in that dataset

MASTER_COL_ORDER <- c(
  # target
  "Y",
  # identifiers and operators
  "IdGroup", "ID", "Operator", 
  # date and derived factors
  "Date", "Year", "Season", "Month", "Day", "WeekNum", "Weekday", 
  # cyclic encodings
  "Season_sin", "Season_cos", "Month_sin", "Month_cos",
  # context
  "Priority", "Price", "Speed", "Duration", "Temp",
  "Location", "Agreed", "State", "Class", "Surface",
  # raw sensors
  paste0("Sensor", 1:30), 
  # D idgrouped sensors
  "Sensor4D", "Sensor8D", "Sensor11D", "Sensor16D", "Sensor22D", "Sensor24D", "Sensor28D",
  # G idgrouped sensors
  "Sensor4G", "Sensor8G", "Sensor11G", "Sensor16G", "Sensor22G", "Sensor24G", "Sensor28G",
  # sensor group 1 (averaged raw)
  "SensorGroup1", "SensorGroup2", "SensorGroup3",
  # centred sensor group (subtract mean)
  "CenterSG1", "CenterSG2", "CenterSG3",
  # standardised sensor group (z-score)
  "StandardisedSG1", "StandardisedSG2", "StandardisedSG3",
  # normalised sensor group (min-max)
  "NormalisedSG1", "NormalisedSG2", "NormalisedSG3"
)

# ~~ reordering helper function ~~

# reorder a df by global master order
reorder_cols <- function(df) {
  keep <- MASTER_COL_ORDER[MASTER_COL_ORDER %in% names(df)]  # only cols that exist, skipping missing cols
  df[, keep]
}


# ── GLOBAL COLOUR THEME ──────────────────────────────────────────────────────

# grouping color strategy
# interleave light/dark within each group for maximum within-group contrast
sensor1_cols <- colorspace::sequential_hcl(10, h = c(200, 260), c = c(100, 60), l = c(25, 85), power = 0.7)
sensor1_cols <- sensor1_cols[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]   # interleave dark/light

sensor2_cols <- colorspace::sequential_hcl(10, h = c(90, 150),  c = c(100, 55), l = c(25, 85), power = 0.7)
sensor2_cols <- sensor2_cols[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]

sensor3_cols <- colorspace::sequential_hcl(10, h = c(270, 330), c = c(100, 55), l = c(25, 85), power = 0.7)
sensor3_cols <- sensor3_cols[c(1, 6, 2, 7, 3, 8, 4, 9, 5, 10)]

THEME_COLOURS <- c(
  sensor1_cols,
  sensor2_cols,
  sensor3_cols,
  as.character(paletteer::paletteer_d("ggthemes::Tableau_20"))
)

# named designations (only pin variables that need a meaningful fixed colour)
THEME_DESIGNATED <- c(
  "Y" = "#C41E3A"   # target variable always red
)

# helper: give it any character vector of names -> get back a named colour vector
#   designated names always get their fixed colour
#   everything else samples from THEME_COLOURS automatically

theme_colours_for <- function(vars) {
  colours <- character(length(vars))
  names(colours) <- vars
  
  for (v in vars) {
    
    # 1️⃣ designated colours
    if (v %in% names(THEME_DESIGNATED)) {
      colours[v] <- THEME_DESIGNATED[v]
      next
    }
    
    # 2️⃣ raw sensors 1–30 (true grouping logic)
    if (grepl("^Sensor[0-9]+$", v)) {
      num <- as.integer(sub("Sensor", "", v))
      
      if (num <= 10) {
        colours[v] <- sensor1_cols[num]
      } else if (num <= 20) {
        colours[v] <- sensor2_cols[num - 10]
      } else {
        colours[v] <- sensor3_cols[num - 20]
      }
      
      next
    }
    
    # 3️⃣ everything else (fallback deterministic mapping)
    idx <- abs(sum(utf8ToInt(v)))
    colours[v] <- THEME_COLOURS[((idx - 1) %% length(THEME_COLOURS)) + 1]
  }
  
  colours
}


# ── GENERAL HELPER ───────────────────────────────────────────────────────────

# sidebar note
sidebar_note <- function(text) {
  div(
    style = "
      font-size: 13px;
      font-weight: 500;
      color: #343a40;
      background-color: white;
      padding: 10px;
      border-left: 4px solid #0d6efd;
      border-radius: 6px;
      margin-bottom: 12px;
      box-shadow: 0 1px 2px rgba(0,0,0,0.05);
    ",
    icon("info-circle", style = "color:#0d6efd;"),
    HTML(paste("&nbsp;", text))
  )
}

# plot title
plot_title <- function(type, context = NULL) {
  if (is.null(context) || context == "") {
    type
  } else {
    paste(type, "—", context)
  }
}


# ── DATA STAGE ───────────────────────────────────────────────────────────────

# ·· RAW DATASET ··························································

# load raw exactly as-is from disk
raw_dataset <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = FALSE)

# create a copied ds for further manipulations (R use copy-on-modify)
ds_copied <- raw_dataset


# ·· PROPER COL NAME DATASET ··············································

# immediately solve col naming issues to prevent further chaos
ds_renamed <- ds_copied %>% 
  rename_with(~ tools::toTitleCase(.x))  # enforce title case


# ·· TYPED DATASET ························································

# ~~ pre-defined schema ~~

schema <- list(
  
  numeric_cols = c("Y", grep("^Sensor", names(ds_copied), value = TRUE)),
  date_cols    = "Date",
  
  factor_schema = list(
    
    # nominal factors
    ID       = list(levels = NULL, ordered = FALSE),
    Operator = list(levels = NULL, ordered = FALSE),
    Location = list(levels = NULL, ordered = FALSE),
    State    = list(levels = NULL, ordered = FALSE),
    Class    = list(levels = NULL, ordered = FALSE),
    Surface  = list(levels = NULL, ordered = FALSE),
    
    # ordinal factors
    Priority = list(
      levels  = c("Low", "Medium", "High"),
      ordered = TRUE
    ),
    
    Price = list(
      levels  = c("Cheap", "Fair", "Expensive"),
      ordered = TRUE
    ),
    
    Speed = list(
      levels  = c("Slow", "Medium", "Fast"),
      ordered = TRUE
    ),
    
    Duration = list(
      levels  = c("Short", "Long", "Very Long"),
      ordered = TRUE
    ),
    
    Temp = list(
      levels  = c("Cold", "Warm", "Hot"),
      ordered = TRUE
    ),
    
    Agreed = list(
      levels  = c("No", "Yes"),
      ordered = FALSE
    )
  )
)


# ~~ assigning type with applied schema ~~

ds_typed <- ds_renamed %>%
  mutate(
    across(all_of(schema$numeric_cols), as.numeric),
    across(all_of(schema$date_cols),    as.Date),
    across(
      all_of(names(schema$factor_schema)),
      ~ {
        col  <- cur_column()
        spec <- schema$factor_schema[[col]]
        if (is.null(spec$levels)) factor(.x)
        else factor(.x, levels = spec$levels, ordered = spec$ordered)
      }
    )
  )


# ·· ENRICHED DATASET ·····················································

enriched_dataset <- ds_typed %>%
  
  # derive IdGroup (G and D)
  mutate(
    
    # derive IdGroup (G and D)
    IdGroup = as.factor(substr(as.character(ID), 1, 1)),
    
    # too few years prefer factor, purely categorical regimes
    Year   = factor(format(Date, "%Y")),
    
    # temporary integer month for deriving season, weekday and cyclic encoding
    Month  = as.integer(format(Date, "%m")),
    
    # month, cyclic encoded
    Month_sin  = sin(2 * pi * Month / 12),
    Month_cos  = cos(2 * pi * Month / 12),
    
    # temporary nominal season before cyclic encoding
    Season = factor(
      case_when(
        Month %in% 1:3   ~ "S1",
        Month %in% 4:6   ~ "S2",
        Month %in% 7:9   ~ "S3",
        Month %in% 10:12 ~ "S4"
      )),
    
    # season, cyclic encoded
    Season_sin = sin(2 * pi * as.integer(Season) / 4),  # S1=1 … S4=4
    Season_cos = cos(2 * pi * as.integer(Season) / 4),
    
    # factorise the integer month
    Month      = factor(Month),
    
    # get weekdays potentially from Monday to Sunday
    Weekday = factor(format(Date, "%A")),
    
    # ISO week number 1-53
    WeekNum = as.integer(format(Date, "%V")),
    
    # # day-of-month is likely just noise (data weekly collected on Thursdays and Fridays)
    # Day    = as.integer(format(Date, "%d")),
    
    # sensor patterns, including grouping and feature scaling
    #   potential dimensionality reduction with sensor groups
    #   based on value pattern similarities across sensors
    #   making it a perfect choice and can handle NAs
    
    # # potential sensor groups based on boxplot patterns
    # SensorGroup1 = as.numeric(rowMeans(across(Sensor1:Sensor10), na.rm = TRUE)),
    # SensorGroup2 = as.numeric(rowMeans(across(Sensor11:Sensor20), na.rm = TRUE)),
    # SensorGroup3 = as.numeric(rowMeans(across(Sensor21:Sensor30), na.rm = TRUE)),
    # 
    # # centering (subtract mean, mean becomes 0, spread preserved)
    # CenterSG1 = SensorGroup1 - mean(SensorGroup1, na.rm = TRUE),
    # CenterSG2 = SensorGroup2 - mean(SensorGroup2, na.rm = TRUE),
    # CenterSG3 = SensorGroup3 - mean(SensorGroup3, na.rm = TRUE),
    # 
    # # Z-score standardisation (mean=0, SD=1)
    # StandardisedSG1 = as.numeric(scale(SensorGroup1, center = TRUE, scale = TRUE)),
    # StandardisedSG2 = as.numeric(scale(SensorGroup2, center = TRUE, scale = TRUE)),
    # StandardisedSG3 = as.numeric(scale(SensorGroup3, center = TRUE, scale = TRUE)),
    # 
    # # min-max normalisation (rescales to [0, 1])
    # NormalisedSG1 = (SensorGroup1 - min(SensorGroup1, na.rm = TRUE)) /
    #   (max(SensorGroup1, na.rm = TRUE) - min(SensorGroup1, na.rm = TRUE)),
    # NormalisedSG2 = (SensorGroup2 - min(SensorGroup2, na.rm = TRUE)) /
    #   (max(SensorGroup2, na.rm = TRUE) - min(SensorGroup2, na.rm = TRUE)),
    # NormalisedSG3 = (SensorGroup3 - min(SensorGroup3, na.rm = TRUE)) /
    #   (max(SensorGroup3, na.rm = TRUE) - min(SensorGroup3, na.rm = TRUE))
    
    # split Sensor6 by IdGroup (numeric output guaranteed)
    # split selected sensors by IdGroup
    Sensor4D  = ifelse(IdGroup == "D", Sensor4,  NA_real_),
    Sensor8D  = ifelse(IdGroup == "D", Sensor8,  NA_real_),
    Sensor11D = ifelse(IdGroup == "D", Sensor11, NA_real_),
    Sensor16D = ifelse(IdGroup == "D", Sensor16, NA_real_),
    Sensor22D = ifelse(IdGroup == "D", Sensor22, NA_real_),
    Sensor24D = ifelse(IdGroup == "D", Sensor24, NA_real_),
    Sensor28D = ifelse(IdGroup == "D", Sensor28, NA_real_),
    
    Sensor4G  = ifelse(IdGroup == "G", Sensor4,  NA_real_),
    Sensor8G  = ifelse(IdGroup == "G", Sensor8,  NA_real_),
    Sensor11G = ifelse(IdGroup == "G", Sensor11, NA_real_),
    Sensor16G = ifelse(IdGroup == "G", Sensor16, NA_real_),
    Sensor22G = ifelse(IdGroup == "G", Sensor22, NA_real_),
    Sensor24G = ifelse(IdGroup == "G", Sensor24, NA_real_),
    Sensor28G = ifelse(IdGroup == "G", Sensor28, NA_real_)
    
  ) %>% # end of enriching mutation
  
  # reordering dataset after enriching
  reorder_cols()


# ·· MODEL DATASET ························································

# trim down to only the columns a model needs
model_dataset <- enriched_dataset %>%
  
  select(
    -any_of(c("ID", "Date")),       # example drops
    # -matches("^RawSensor\\d+$"),  # drop Sensor1, Sensor2, ... Sensor30
    # -matches("^Normalised"),      # drop NormalisedSG1, NormalisedSG2, NormalisedSG3
  )


# ·· DEBUGGING DATASET ····················································

# add a flag column, for exploring
debug_dataset <- enriched_dataset %>%
  mutate(
    # example: flag
    Flag = NA_character_   # empty string column — fill in manually later
  )





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
    
    # spacer
    column(6),
    
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
               sidebarPanel(width = 2,
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
               mainPanel(width = 10,
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
                                         or categorical values, depending on the selected mode."),
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
                            
                            selectizeInput("vc_cols", "Search in columns:",
                                           choices  = NULL,
                                           multiple = TRUE),
                            
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
                            ),   # ← conditionalPanel closed here
                            hr(),
                            actionButton("gg_run", "Plot", icon = icon("play"), width = "100%"),
                            helpText("Select variables then click Plot. Large selections may be slow.")
               ),
               mainPanel(width = 9,
                         plotOutput("gg_plot", height = "80vh")
               )
             )
    ),  # end of tab panel
    
    
    # ── UI BOXPLOT ────────────────────────────────────────────────────────
    
    tabPanel("Boxplot",
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
                            checkboxInput("box_violin", "Show as violin", value = FALSE),
               ),
               mainPanel(width = 9,
                         plotlyOutput("box_plot", height = "85vh")
               )
             )
    ),  # end of tab panel
    
    
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
    
    
  
    # ── UI PLOT A EXAMPLE ─────────────────────────────────────────────────
    
    tabPanel("Plot A",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Hint: <br><br>add my own controls here."),
                            hr(),
                            selectInput("plotA_var", "Variable:", choices = NULL)
               ),
               mainPanel(width = 9,
                         plotOutput("plotA_output", height = "85vh")
               )
             )
    ), # end of tab panel
    
    
    # ── COMING SOON ───────────────────────────────────────────────────────
    
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





# =============================================================================
# server.R
# =============================================================================

server <- function(input, output, session) {
  
  # isolated R environment for the console (inheritance)
  #   so that I can read global objects
  #   assign while staying inside rconsole_env
  #   but my console code won't overwrite globalenv()
  
  rconsole_env <- new.env(parent = globalenv())
  
  
  # ── SERVER SECURITY REACTIVE ───────────────────────────────────────────
  
  # ·· LOCK STATE UPDATE ··············································
  
  # single reactive value drives everything security-related
  is_unlocked <- reactiveVal(FALSE)
  
  # toggle lock / unlock on button click
  observeEvent(input$privacy_unlock, {
    if (is_unlocked()) {
      # if already unlocked → lock again
      is_unlocked(FALSE)
      updateTextInput(session, "privacy_pass", value = "")
    } else {
      if (!is.null(input$privacy_pass) && input$privacy_pass == UNLOCK_PASSPHRASE) {
        is_unlocked(TRUE)
        updateTextInput(session, "privacy_pass", value = "")
      } else {
        showNotification("Incorrect passphrase.", type = "error", duration = 3)
      }
    }
  })
  
  # status badge next to the lock button (bigger and nice looking, this is intended, not a bug)
  output$privacy_status_ui <- renderUI({
    if (is_unlocked()) {
      tagList(
        actionButton("privacy_lock_btn", label = NULL, icon = icon("unlock"),
                     style = "color:#198754; background:none; border:none; font-size:18px; padding:0;",
                     title = "Click to lock"),
        span("Full access", style = "color:#198754; font-size:12px;")
      )
    } else {
      span(icon("lock"), " Private Mode",
           style = "color:#6c757d; font-size:12px;")
    }
  })
  
  
  # ·· DATASET SELECTOR UI ············································
  
  output$dataset_selector_ui <- renderUI({
    choices <- c("Raw Dataset", "Enriched Dataset", "Model Dataset")
    
    # (injects "Debug Dataset" when unlocked)
    if (is_unlocked()) choices <- c(choices, "Debug Dataset")
    
    # preserve current selection
    current <- isolate(input$dataset_choice)
    if (is.null(current) || !current %in% choices) current <- "Enriched Dataset"
    
    selectInput("dataset_choice", "Dataset Stage:",
                choices = choices, selected = current)
  })
  
  # info modal explaining each stage
  observeEvent(input$dataset_info, {
    showModal(modalDialog(
      title = "Dataset Stages",
      tags$dl(
        tags$dt("Raw Dataset"),
        tags$dd("Original data as loaded from disk. No changes."),
        tags$dt("Enriched Dataset"),
        tags$dd("Raw + derived columns (type fixes, date parts, computed features, etc.)."),
        tags$dt("Model Dataset"),
        tags$dd("Enriched with identifiers and leaky columns removed. Ready for modelling."),
        tags$dt("Debug Dataset", style = "color:#856404;"),
        tags$dd("Enriched + diagnostic flags and intermediate values. Only visible when unlocked."),
        tags$dt("Note: ", style = "color:#856404;"),
        tags$dd("ID column is intentionally hidden across all datasets. Only visible when unlocked."),
      ),
      easyClose = TRUE,
      footer    = modalButton("Close")
    ))
  })
  
  
  # ── GLOBAL DATASET STAGE ───────────────────────────────────────────────
  
  # Note: everything downstream will read from display_data()
  
  # layer 1: selected_data()  —> picks the intended dataset stage to examine
  selected_data <- reactive({
    req(input$dataset_choice)
    switch(input$dataset_choice,
           "Raw Dataset"      = raw_dataset,
           "Enriched Dataset" = enriched_dataset,
           "Model Dataset"    = model_dataset,
           "Debug Dataset"    = debug_dataset,   # only reachable when unlocked
           enriched_dataset)                      # enriched dataset always as fallback
  })
  
  # layer 2: display_data()   —> additionally strips PRIVATE_COLS when locked
  display_data <- reactive({
    df   <- selected_data()
    drop <- intersect(PRIVATE_COLS, names(df))
    if (!is_unlocked() && length(drop) > 0) {
      df <- df[, !names(df) %in% drop, drop = FALSE]
    }
    df
  })
  
  
  # ── SERVER DATA TABLE ──────────────────────────────────────────────────
  
  output$data_table <- DT::renderDataTable({
    df <- head(display_data(), 1000)
    DT::datatable(df,
                  caption = paste0(input$dataset_choice,
                                   " — ", nrow(df), " rows × ", ncol(df), " cols"))
  })
  
  
  # ── SERVER SUMMARY ─────────────────────────────────────────────────────
  
  output$summary_output <- renderPrint({
    df <- display_data()
    switch(input$summary_style,
           "base"    = summary(df),
           "glimpse" = cat(capture.output(tibble::glimpse(df)), sep = "\n"),
           "dfsummary" = summarytools::dfSummary(df))
  })
  
  
  # ── SERVER WORD CLOUD ──────────────────────────────────────────────────
  
  observe({
    df       <- display_data()
    req(df)
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "wc_var", choices = cat_vars, selected = cat_vars[1])
  })
  
  output$wc_plot <- renderPlot({
    
    df  <- display_data()
    
    # add options to check variable names based on user need
    if (isTRUE(input$wc_varnames_mode)) {
      val <- names(df)
    } else {
      req(input$wc_var)
      val <- as.character(df[[input$wc_var]])
    }
    
    val <- val[!is.na(val) & nchar(trimws(val)) > 0]
    
    # apply case folding on raw values before any splitting
    if (!input$wc_case) val <- tolower(val)
    
    validate(need(length(val) > 0, "No non-missing values to display."))
    
    # optional splitting logic
    tokens <- switch(input$wc_split_mode,
                     "none" = val,
                     "chars" = {
                       t <- unlist(strsplit(val, ""))
                       t[!grepl("\\s", t)]
                     },
                     "alphanum" = {
                       t <- unlist(lapply(val, function(tok) {
                         m <- gregexpr("[A-Za-z]+|[0-9]+", tok, perl = TRUE)
                         regmatches(tok, m)[[1]]
                       }))
                       t[nchar(t) > 0]
                     }
    )
    
    freq_table <- sort(table(tokens), decreasing = TRUE)
    freq_table <- freq_table[freq_table >= input$wc_min_freq]
    validate(need(length(freq_table) > 0,
                  paste0("No tokens appear at least ", input$wc_min_freq, " times.")))
    freq_table <- head(freq_table, input$wc_max_words)
    
    words <- names(freq_table)
    freqs <- as.integer(freq_table)
    n     <- length(words)
    
    # colours
    pal    <- RColorBrewer::brewer.pal(max(3, min(8, n)), input$wc_palette)
    colors <- colorRampPalette(pal)(n)[rank(-freqs, ties.method = "first")]
    
    # adaptive normalisation with outlier-aware contrast boost
    freq_ranks <- rank(-freqs, ties.method = "first")   # 1 = most frequent
    
    # detect if top token(s) are genuine outliers vs the rest
    # outlier = top freq is 3x the median freq
    freq_ratio <- max(freqs) / max(median(freqs), 1)
    
    if (freq_ratio >= 3) {
      # outlier mode: use sqrt-compressed raw freq → preserves contrast without
      # letting the top token completely dwarf everyone else
      freqs_norm <- as.integer(20 + (sqrt(freqs) - sqrt(min(freqs))) /
                                 max(sqrt(max(freqs)) - sqrt(min(freqs)), 1) * 80)
    } else {
      # normal mode: pure rank-based (flat distribution, no outliers)
      freqs_norm <- as.integer(100 - (freq_ranks - 1) / max(freq_ranks - 1, 1) * 80)
    }
    
    # adaptive scale — use outlier ratio to decide canvas scale
    # high ratio = we WANT a large max_scale so G/D are visually dominant
    n_eff      <- min(n, 30)
    base_scale <- max(3, min(9, 40 / sqrt(n_eff)))
    # boost max_scale proportionally to outlier strength, cap at 12
    max_scale  <- min(12, base_scale * (1 + log10(max(freq_ratio, 1))))
    min_scale  <- max(0.3, max_scale * 0.15)   # tighter min so rare tokens stay small
    max_scale <- max_scale * input$wc_scale
    min_scale <- min_scale * input$wc_scale
    
    par(mar = c(0, 0, 0, 0), bg = "white")
    
    title(
      main = plot_title(
        "Word Cloud",
        if (isTRUE(input$wc_varnames_mode)) {
          "Variable Names"
        } else {
          paste("Values of", input$wc_var)
        }
      ),
      cex.main = 1.2,
      font.main = 2
    )
    
    wordcloud::wordcloud(
      words        = words,
      freq         = freqs_norm,
      max.words    = n,
      min.freq     = 1,
      random.order = FALSE,
      rot.per      = 0.15,
      colors       = colors,
      scale        = c(max_scale, min_scale),
      use.r.layout = FALSE
    )
  })
  
  
  # ── SERVER UPSET ───────────────────────────────────────────────────────
  
  observe({
    df <- display_data()
    req(df)
    na_cols <- names(df)[sapply(df, function(x) any(is.na(x)))]
    if (length(na_cols) == 0) na_cols <- names(df)
    updateSelectizeInput(session, "upset_cols", choices = names(df), selected = names(df))
  })
  
  output$upset_plot <- renderPlotly({
    req(input$upset_cols)
    df <- display_data()
    validate(need(length(input$upset_cols) >= 1, "Please select at least 1 column."))
    
    df_sub <- df[, input$upset_cols, drop = FALSE]
    
    # ·· build anomaly token lists ··
    
    # preset + custom text tokens
    text_tokens <- c(input$upset_det_text)
    if (nchar(trimws(input$upset_custom_text)) > 0) {
      extra <- strsplit(input$upset_custom_text, ",")[[1]]
      text_tokens <- c(text_tokens, trimws(extra))
    }
    
    # preset + custom numeric sentinel tokens (as strings for unified matching)
    num_tokens <- c(input$upset_det_sentinel)
    if (nchar(trimws(input$upset_custom_num)) > 0) {
      extra <- strsplit(input$upset_custom_num, ",")[[1]]
      num_tokens <- c(num_tokens, trimws(extra))
    }
    # also as numeric for numeric columns
    num_sentinels <- suppressWarnings(as.numeric(num_tokens))
    num_sentinels <- num_sentinels[!is.na(num_sentinels)]
    
    # ·· flag each cell ··
    is_anomaly <- function(x, col_name) {
      
      n      <- length(x)
      flag   <- rep(FALSE, n)
      x_str  <- as.character(x)   # string view for all columns
      
      # empty / whitespace
      if (isTRUE(input$upset_det_empty)) {
        flag <- flag | (!is.na(x) & trimws(x_str) == "")
      }
      
      # true NA
      if (isTRUE(input$upset_det_na)) {
        flag <- flag | is.na(x)
      }
      
      # text pseudo-NAs + custom text (unified, respects case toggle)
      if (length(text_tokens) > 0) {
        if (isTRUE(input$upset_case)) {
          flag <- flag | (trimws(x_str) %in% text_tokens)
        } else {
          flag <- flag | (tolower(trimws(x_str)) %in% tolower(text_tokens))
        }
      }
      
      # sentinel numbers — string path (applies to ALL column types)
      if (length(num_tokens) > 0) {
        flag <- flag | (trimws(x_str) %in% num_tokens)
      }
      
      # sentinel numbers — numeric path (numeric columns only, catches 9999.0 etc.)
      if (length(num_sentinels) > 0 && is.numeric(x)) {
        flag <- flag | (x %in% num_sentinels)
      }
      
      # negative numbers (numeric columns only)
      if (isTRUE(input$upset_det_negative) && is.numeric(x)) {
        flag <- flag | (!is.na(x) & x < 0)
      }
      
      flag
    }
    
    # build binary anomaly matrix
    na_mat <- as.data.frame(
      mapply(is_anomaly, df_sub, names(df_sub), SIMPLIFY = FALSE)
    )
    na_mat <- as.data.frame(lapply(na_mat, as.integer))
    
    # pattern string per row
    patterns <- apply(na_mat, 1, function(row) {
      cols_flagged <- input$upset_cols[row == 1]
      if (length(cols_flagged) == 0) return("(Complementary Completeness)")
      paste(cols_flagged, collapse = " & ")
    })
    
    pattern_counts <- sort(table(patterns), decreasing = TRUE)
    pattern_counts <- head(pattern_counts, input$upset_top)
    
    plot_df <- data.frame(
      Pattern = names(pattern_counts),
      Count   = as.integer(pattern_counts)
    )
    
    # sort
    plot_df <- if (input$upset_sort == "desc") {
      plot_df[order(-plot_df$Count), ]
    } else {
      plot_df[order(plot_df$Count), ]
    }
    
    plot_df$Pattern <- factor(plot_df$Pattern, levels = rev(plot_df$Pattern))
    plot_df$Colour  <- ifelse(plot_df$Pattern == "(Complementary Completeness)", "complete", "missing")
    
    p <- ggplot(plot_df, aes(x = Count, y = Pattern, fill = Colour,
                             text = paste0(Pattern, "\n", Count, " rows"))) +
      geom_col() +
      scale_fill_manual(values = c("complete" = "steelblue", "missing" = "tomato")) +
      labs(title = plot_title("UpSet", "Anomaly intersection patterns")) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
  })
  
  
  # ── SERVER LOLLIPOP ────────────────────────────────────────────────────
  
  observe({
    df   <- display_data()
    req(df)
    all_cols <- names(df)
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    updateSelectizeInput(session, "vc_cols",      choices = all_cols, selected = all_cols)
    updateSelectInput(   session, "vc_group_var", choices = cat_vars, selected = cat_vars[1])
  })
  
  # populate group levels when group variable changes
  observeEvent(input$vc_group_var, {
    req(input$vc_group_var)
    df   <- display_data()
    lvls <- sort(unique(as.character(df[[input$vc_group_var]])))
    lvls <- lvls[!is.na(lvls)]
    # append (NA) as selectable level only if the grouping variable actually has NAs
    if (any(is.na(df[[input$vc_group_var]]))) lvls <- c(lvls, "(NA)")
    updateSelectizeInput(session, "vc_group_levels", choices = lvls, selected = lvls)
  })
  
  output$vc_plot <- renderPlotly({
    req(input$vc_value, input$vc_cols)
    df  <- display_data()
    val <- input$vc_value
    
    # helper: count occurrences in a single df
    count_val <- function(data, cols, val, case_sensitive) {
      results <- lapply(cols, function(col) {
        x     <- data[[col]]
        total <- length(x)
        n_match <- if (toupper(val) == "NA") {
          sum(is.na(x))
        } else if (case_sensitive) {
          sum(as.character(x) == val, na.rm = TRUE)
        } else {
          sum(tolower(as.character(x)) == tolower(val), na.rm = TRUE)
        }
        data.frame(Column = col, Count = n_match,
                   Pct = round(n_match / total * 100, 1), Total = total)
      })
      do.call(rbind, results)
    }
    
    # grouped or ungrouped
    if (input$vc_group_on && !is.null(input$vc_group_var)) {
      
      grp  <- input$vc_group_var
      lvls <- if (!is.null(input$vc_group_levels) && length(input$vc_group_levels) > 0)
        input$vc_group_levels
      else
        sort(unique(as.character(df[[grp]])))
      
      # compute counts per level — treat "(NA)" as a real level by subsetting on is.na()
      plot_df <- do.call(rbind, lapply(lvls, function(lv) {
        sub <- if (lv == "(NA)") {
          df[is.na(df[[grp]]), ]
        } else {
          df[!is.na(df[[grp]]) & as.character(df[[grp]]) == lv, ]
        }
        res       <- count_val(sub, input$vc_cols, val, input$vc_case)
        res$Group <- lv
        res
      }))
      
    } else {
      plot_df        <- count_val(df, input$vc_cols, val, input$vc_case)
      plot_df$Group  <- "All"
    }
    
    # apply min count filter
    # filter keeps a column if ANY group for that column passes the threshold
    cols_pass <- unique(plot_df$Column[plot_df$Count >= input$vc_min_count])
    plot_df   <- plot_df[plot_df$Column %in% cols_pass, ]
    validate(need(nrow(plot_df) > 0, "No columns meet the minimum count threshold."))
    
    # sort
    sort_col <- if (input$vc_metric == "count") "Count" else "Pct"
    # sort by the max value across groups per column, for stable ordering
    col_order <- tapply(plot_df[[sort_col]], plot_df$Column, max)
    if (input$vc_sort == "desc") {
      col_order <- names(sort(col_order, decreasing = TRUE))
    } else if (input$vc_sort == "asc") {
      col_order <- names(sort(col_order, decreasing = FALSE))
    } else {
      col_order <- input$vc_cols[input$vc_cols %in% names(col_order)]
    }
    plot_df$Column <- factor(plot_df$Column, levels = rev(col_order))
    
    # labels & y value
    y_val   <- if (input$vc_metric == "count") plot_df$Count else plot_df$Pct
    y_label <- if (input$vc_metric == "count") "Count" else "Percentage (%)"
    plot_df$y_val <- y_val
    plot_df$label <- if (input$vc_metric == "count") {
      paste0(plot_df$Column, " [", plot_df$Group, "]: ", plot_df$Count, " / ", plot_df$Total)
    } else {
      paste0(plot_df$Column, " [", plot_df$Group, "]: ", plot_df$Pct, "%")
    }
    
    # plot
    if (input$vc_group_on && !is.null(input$vc_group_var)) {
      
      p <- ggplot(plot_df, aes(x = y_val, y = Column, colour = Group, text = label)) +
        geom_segment(aes(x = 0, xend = y_val, y = Column, yend = Column,
                         colour = Group),
                     linewidth = 0.6,
                     position  = position_dodge(width = 0.6)) +
        geom_point(size = 3, position = position_dodge(width = 0.6)) +
        labs(title = plot_title(
          "Lollipop",
          paste0('Occurrences of "', val, '" grouped by ', input$vc_group_var)
        )) +
        theme_minimal(base_size = 11)
      
    } else {
      
      p <- ggplot(plot_df, aes(x = y_val, y = Column, text = label)) +
        geom_segment(aes(x = 0, xend = y_val, y = Column, yend = Column),
                     colour = "steelblue", linewidth = 0.8) +
        geom_point(colour = "steelblue", size = 3) +
        labs(title = plot_title("Lollipop", paste0('Occurrences of "', val, '"'))) +
        theme_minimal(base_size = 11)
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # ── SERVER RISING VALUE ────────────────────────────────────────────────
  
  # user sidebar action collection layer (reactive auto-detect)
  observe({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    default_sel  <- num_vars[num_vars %in% 
                               c("Y", "Sensor4", "Sensor6", "Sensor8", "Sensor11", 
                                 "Sensor16", "Sensor22", "Sensor24", "Sensor28")]
    updateSelectizeInput(session, "rising_var", choices = num_vars, selected = default_sel)
  })
  
  # render layer — rising value lines, ggplotly
  output$rising_output <- renderPlotly({
    req(input$rising_var)
    df       <- display_data()
    num_vars <- input$rising_var
    colours  <- theme_colours_for(num_vars)
    
    # build long-format df
    plot_df <- do.call(rbind, lapply(num_vars, function(v) {
      y   <- sort(na.omit(df[[v]]))
      pct <- seq_along(y) / length(y) * 100
      data.frame(Percentile = pct, Value = y, Variable = v)
    }))
    
    p <- ggplot(plot_df, aes(x = Percentile, y = Value,
                             colour = Variable,
                             group  = Variable,
                             text   = paste0(Variable,
                                             "<br>Percentile: ", round(Percentile, 1),
                                             "<br>Value: ",      round(Value, 3)))) +
      geom_line(linewidth = input$rising_lwd / 3, linetype = input$rising_lty) +
      scale_colour_manual(values = colours) +
      labs(title = plot_title("Rising Value", "Sorted Distribution by Percentile")) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # ── SERVER GGPAIRS ─────────────────────────────────────────────────────
  
  observe({
    df <- display_data()
    req(df)
    all_vars     <- names(df)
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars     <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    default_gg_vars <- c("Sensor4", "Sensor8", "Sensor11", "Sensor16", "Sensor22", "Sensor24", "Sensor28")
    updateSelectizeInput(session, "gg_vars", choices  = all_vars, selected = intersect(default_gg_vars, all_vars))
    updateSelectInput(session, "gg_group_var", choices = cat_vars, selected = cat_vars[1])
  })
  
  # populate level choices when group variable changes
  observeEvent(input$gg_group_var, {
    req(input$gg_group_var)
    df   <- display_data()
    lvls <- sort(unique(as.character(df[[input$gg_group_var]])))
    lvls <- lvls[!is.na(lvls)]
    updateSelectizeInput(session, "gg_group_levels",
                         choices  = lvls,
                         selected = lvls)
  })
  
  output$gg_plot <- renderPlot({
    input$gg_run
    isolate({
      req(input$gg_vars)
      df <- display_data()
      validate(need(length(input$gg_vars) >= 2, "Please select at least 2 variables."))
      
      if (input$gg_group_on && !is.null(input$gg_group_var)) {
        
        grp <- input$gg_group_var
        
        # build df with selected vars + group col, drop NAs in group
        plot_df <- df[, c(input$gg_vars, grp), drop = FALSE]
        plot_df <- plot_df[!is.na(plot_df[[grp]]), ]
        
        # filter to selected levels only
        if (!is.null(input$gg_group_levels) && length(input$gg_group_levels) > 0) {
          plot_df <- plot_df[as.character(plot_df[[grp]]) %in% input$gg_group_levels, ]
          plot_df[[grp]] <- droplevels(as.factor(plot_df[[grp]]))  # drop unused levels from legend
        }
        
        # columns to plot = only the selected vars (not the group col)
        col_idx <- seq_along(input$gg_vars)
        
        GGally::ggpairs(
          plot_df,
          columns  = col_idx,
          mapping  = aes(colour = .data[[grp]], alpha = 0.6),
          upper    = list(
            continuous = GGally::wrap("cor", size = 2.5),
            combo      = GGally::wrap("box_no_facet", alpha = 0.5),
            discrete   = GGally::wrap("facetbar", alpha = 0.5)
          ),
          lower    = list(
            continuous = GGally::wrap("points", alpha = 0.3, size = 0.8),
            combo      = GGally::wrap("facethist", bins = 20, alpha = 0.5),
            discrete   = GGally::wrap("facetbar", alpha = 0.5)
          ),
          diag     = list(
            continuous = GGally::wrap("densityDiag", alpha = 0.5),
            discrete   = GGally::wrap("barDiag", alpha = 0.5)
          ),
          legend   = 1  # show legend
        ) +
          ggplot2::theme_minimal(base_size = 9) +
          ggplot2::theme(
            strip.text  = element_text(size = 7),
            legend.position = "bottom"
          ) +
          ggplot2::labs(title = plot_title("GGPairs", paste("Grouped by", grp)))
        
      } else {
        
        plot_df <- df[, input$gg_vars, drop = FALSE]
        
        GGally::ggpairs(
          plot_df,
          upper = list(continuous = GGally::wrap("cor", size = 3)),
          lower = list(continuous = GGally::wrap("points", alpha = 0.4, size = 0.8)),
          diag  = list(continuous = GGally::wrap("densityDiag"))
        ) +
          ggplot2::theme_minimal(base_size = 9) +
          ggplot2::theme(strip.text = element_text(size = 7))
      }
    })
  })
  
  
  # ── SERVER BOXPLOT ─────────────────────────────────────────────────────
  
  observe({
    df       <- display_data()
    req(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    updateSelectizeInput(session, "box_vars",      choices = num_vars, selected = intersect(paste0("Sensor", 1:30), num_vars))
    updateSelectInput(   session, "box_group_var", choices = cat_vars, selected = cat_vars[1])
  })
  
  # add an observer to populate levels
  observeEvent(input$box_group_var, {
    req(input$box_group_var)
    df   <- display_data()
    lvls <- sort(unique(as.character(df[[input$box_group_var]])))
    lvls <- lvls[!is.na(lvls)]
    updateSelectizeInput(session, "box_group_levels", choices = lvls, selected = lvls)
  })
  
  output$box_plot <- renderPlotly({
    req(input$box_vars)
    df <- display_data()
    validate(need(length(input$box_vars) >= 1, "Please select at least 1 variable."))
    
    plot_df <- tidyr::pivot_longer(df, cols = all_of(input$box_vars),
                                   names_to = "Variable", values_to = "Value")
    plot_df$Variable <- factor(plot_df$Variable, levels = input$box_vars)
    
    # build geom layer separately to avoid inline if/else issue
    geom_layer <- if (input$box_violin) {
      geom_violin(alpha = 0.7, trim = FALSE)
    } else {
      geom_boxplot(alpha = 0.7, outlier.size = 0.8)
    }
    
    if (input$box_group_on && !is.null(input$box_group_var)) {
      grp <- input$box_group_var
      plot_df[[grp]] <- as.factor(plot_df[[grp]])
      
      if (!is.null(input$box_group_levels) && length(input$box_group_levels) > 0) {
        plot_df <- plot_df[as.character(plot_df[[grp]]) %in% input$box_group_levels, ]
        plot_df[[grp]] <- droplevels(as.factor(plot_df[[grp]]))
      }
      
      validate(need(nrow(plot_df) > 0, ""))
      
      p <- ggplot(plot_df, aes(x = Variable, y = Value, fill = Variable)) +
        geom_layer +
        facet_wrap(as.formula(paste("~", grp)), scales = "free_y") +
        labs(
          title = paste(
            if (input$box_violin) "Violin" else "Boxplot",
            "grouped by", grp
          ),
          x = NULL, y = "Value") +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
      
    } else {
      p <- ggplot(plot_df, aes(x = Variable, y = Value, fill = Variable)) +
        geom_layer +
        labs(
          title = if (input$box_violin) "Violin — variable comparison" else "Boxplot — variable comparison",
          x = NULL, y = "Value") +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none")
    }
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  
  # ── SERVER Q-Q PLOT ────────────────────────────────────────────────────
  
  # update variable choices for QQ Plot
  observe({
    df <- display_data()
    req(df)
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "qq_var", choices = numeric_vars)
  })
  
  # dynamic UI for distribution parameters
  output$qq_params <- renderUI({
    req(input$qq_dist)
    
    switch(input$qq_dist,
           "norm" = tagList(
             numericInput("qq_mean", "Mean", value = 0),
             numericInput("qq_sd", "SD", value = 1, min = 0.01)
           ),
           "exp" = tagList(
             numericInput("qq_rate", "Rate (λ)", value = 1, min = 0.01)
           ),
           "lnorm" = tagList(
             numericInput("qq_meanlog", "Meanlog", value = 0),
             numericInput("qq_sdlog", "SDlog", value = 1, min = 0.01)
           ),
           "gamma" = tagList(
             numericInput("qq_shape", "Shape (α)", value = 1, min = 0.01),
             numericInput("qq_rate_g", "Rate (β)", value = 1, min = 0.01)
           ),
           "weibull" = tagList(
             numericInput("qq_shape_w", "Shape (k)", value = 1, min = 0.01),
             numericInput("qq_scale_w", "Scale (λ)", value = 1, min = 0.01)
           )
    )
  })
  
  # render the QQ Plot
  output$qq_plot <- renderPlotly({
    req(input$qq_var, input$qq_dist)
    df  <- display_data()
    val <- df[[input$qq_var]]
    val <- sort(val[!is.na(val)])  # ascending order/quantile
    
    # define distribution arguments based on user input
    dist_args <- switch(input$qq_dist,
                        "norm"    = list(mean = input$qq_mean,     sd    = input$qq_sd),
                        "exp"     = list(rate = input$qq_rate),
                        "lnorm"   = list(meanlog = input$qq_meanlog, sdlog = input$qq_sdlog),
                        "gamma"   = list(shape = input$qq_shape,   rate  = input$qq_rate_g),
                        "weibull" = list(shape = input$qq_shape_w, scale = input$qq_scale_w)
    )
    
    # validate that args are not null
    if (any(sapply(dist_args, is.null))) return(NULL)
    
    # generate theoretical quantiles
    n     <- length(val)
    probs <- (1:n - 0.5) / n
    
    # map string distribution names to their quantile functions (qnorm, qexp, etc)
    q_func                <- match.fun(paste0("q", input$qq_dist))
    theoretical_quantiles <- do.call(q_func, c(list(p = probs), dist_args))
    
    qq_df <- data.frame(Theoretical = theoretical_quantiles, Sample = val)
    
    dot_colour <- "#A6CEE3"
    
    p <- ggplot(qq_df, aes(x = Theoretical, y = Sample)) +
      geom_point(shape = 21, fill  = dot_colour, colour = "black", size = 2, stroke = 0.25, alpha = 0.7) +
      labs(
        title = plot_title(
          "Q-Q Plot",
          paste0(input$qq_var, " ~ ", input$qq_dist, "(μ=", input$qq_mean, ", σ=", input$qq_sd, ")")
        )) +
      theme_minimal(base_size = 11)
    
    if (input$qq_line) {
      # reference line passing through 1st and 3rd quartiles (similar to qqline logic)
      y_q   <- quantile(val, c(0.25, 0.75))
      x_q   <- do.call(q_func, c(list(p = c(0.25, 0.75)), dist_args))
      slope <- diff(y_q) / diff(x_q)
      int   <- y_q[1] - slope * x_q[1]
      p     <- p + geom_abline(intercept = int, slope = slope,
                               colour = THEME_DESIGNATED[["Y"]], linewidth = 0.8)
    }
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  # ── SERVER PLOT A EXAMPLE ──────────────────────────────────────────────
  
  # user sidebar action collection layer (reactive auto-detect)
  observe({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "plotA_var", choices = num_vars, selected = num_vars[1])
  })
  
  # render layer (computation / visualization)
  output$plotA_output <- renderPlot({
    req(input$plotA_var)
    df <- display_data()
    # random plot example
    hist(df[[input$plotA_var]],
         main = paste("Histogram —", input$plotA_var),
         xlab = input$plotA_var, col = "steelblue", border = "white")
  })
  
  
  # ── SERVER R CONSOLE ───────────────────────────────────────────────────
  
  # the entire UI is replaced with a lock screen when not authenticated
  
  # ·· CONDITIONAL UI (LOCK GATE) ·····································
  
  output$rconsole_body_ui <- renderUI({
    
    if (!is_unlocked()) {
      # ~~ locked state: show a friendly gate ~~
      div(style = "text-align:center; margin-top:120px; color:#6c757d;",
          icon("lock", style = "font-size:48px;"),
          h4("R Console is locked for developer only."),
          p("Enter the passphrase above to unlock.")
      )
      
    } else {
      # ~~ unlocked state: render full console ~~
      sidebarLayout(
        sidebarPanel(width = 3,
                     sidebar_note("Note: <br><br>Run R expressions against the current dataset. 
                                  <br><br>The dataset is available as df."),
                     hr(),
                     actionButton("rconsole_run",   "Run",   icon = icon("play"),  width = "100%"),
                     br(), br(),
                     actionButton("rconsole_clear", "Clear", icon = icon("trash"), width = "100%")
        ),
        mainPanel(width = 9,
                  # code editor (ACE)
                  shinyAce::aceEditor(
                    "rconsole_input",
                    mode     = "r",
                    theme    = "tomorrow",
                    height   = "400px",
                    value    = "# type R code here\nhead(df)",
                    fontSize = 14
                  ),
                  hr(),
                  verbatimTextOutput("rconsole_output")
        )
      )
    }
  })
  
  
  # ·· CONTROLLED CODE EXECUTION ······································
  
  output$rconsole_output <- renderPrint({
    input$rconsole_run
    # isolate here means code only runs on button click
    isolate({
      code <- input$rconsole_input
      if (is.null(code) || trimws(code) == "") {
        cat("# Type R code above and click Run\n")
        return(invisible(NULL))
      }
      
      # normalise line endings and common unicode punctuation
      code <- gsub("\r\n|\r", "\n", code)
      code <- gsub("\u2018|\u2019", "'",  code)
      code <- gsub("\u201c|\u201d", '"',  code)
      code <- gsub("\u2013|\u2014", "-",  code)
      code <- iconv(code, from = "UTF-8", to = "ASCII", sub = "")
      
      # inject current dataset into the console environment
      assign("df", display_data(), envir = rconsole_env)
      
      # parse once, eval expression-by-expression so every value prints
      exprs <- tryCatch(parse(text = code),
                        error = function(e) { cat("Parse error:", conditionMessage(e), "\n"); NULL })
      if (is.null(exprs)) return(invisible(NULL))
      
      for (expr in exprs) {
        tryCatch({
          res <- capture.output(
            eval(expr, envir = rconsole_env)
          )
          if (length(res) > 0) cat(paste(res, collapse = "\n"), "\n")
        },
        error = function(e) cat("Error:", conditionMessage(e), "\n"),
        warning = function(w) cat("Warning:", conditionMessage(w), "\n"))
      }
    })
  })
  
  # resets editor content, clean UX
  observeEvent(input$rconsole_clear, {
    shinyAce::updateAceEditor(session, "rconsole_input", value = "")
  })
  
  
} # end server





# =============================================================================
# Run shiny app
# =============================================================================

shinyApp(ui, server)