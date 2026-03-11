# =================================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(shinyAce)  # R console ace editor
library(tidyverse)
library(plotly)
library(seriation)

# library(dplyr)
# library(DT)

# library(ggplot2)
# library(vcd)
# library(dbscan)
# library(paletteer) # global colour theme
# library(colorspace)
# library(visdat)
# library(shinyjs)
# library(tidytext)
# library(wordcloud2)
# library(pls)


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
  "NormalisedSG1", "NormalisedSG2", "NormalisedSG3",
  # na flag specific for Sensor6
  "NaFlag_S6"
)

# ~~ reordering helper function ~~

# reorder a df by global master order
reorder_cols <- function(df) {
  keep <- MASTER_COL_ORDER[MASTER_COL_ORDER %in% names(df)]  # only cols that exist, skipping missing cols
  df[, keep]
}


# ·· VARIABLE PRESET ······················································

# all conceptualised presets for this dataset
VAR_PRESETS <- list(
  "Sensor 1-10"  = paste0("Sensor", 1:10),
  "Sensor 11-20" = paste0("Sensor", 11:20),
  "Sensor 21-30" = paste0("Sensor", 21:30),
  "All Sensors"  = paste0("Sensor", 1:30),
  "Gapped Sensors"  = paste0("Sensor", c(4,6,8,11,16,22,24,28)),
  "Gapped Sensors vs Y"  = c("Y", paste0("Sensor", c(4,6,8,11,16,22,24,28))),
  "Gapped Sensors (excl. 6)"  = paste0("Sensor", c(4,8,11,16,22,24,28)),
  "Sensor 1-10 (excl. 4,8)"  = paste0("Sensor", (1:10)[!(1:10 %in% c(4, 8))])
)

# which to display as preset choices (using this meta)
VAR_PRESET_META <- tibble::tribble(
  ~plot,        ~s1_10, ~s11_20, ~s21_30, ~sall, ~gapped, ~gapped_y, ~gapped_excl6, ~s1_10_excl48,
  "rising",      1,      1,       1,       1,     1,       1,         1,             0,
  "ggpairs",     1,      1,       1,       0,     1,       1,         1,             1,
  "heatmap",     1,      1,       1,       1,     1,       1,         1,             1
)

# default select box content per plot
DEFAULT_PRESET <- list(
  rising  = "Gapped Sensors vs Y",
  ggpairs = "Sensor 1-10 (excl. 4,8)",
  heatmap = "Gapped Sensors vs Y"
  )

# lookup helper: returns named VAR_PRESETS list valid for a given plot
presets_for <- function(plot_name) {
  row  <- VAR_PRESET_META[VAR_PRESET_META$plot == plot_name, -1]  # drop ~plot col
  keep <- as.logical(row)
  VAR_PRESETS[keep]
}

# logic helper: sets ONLY the selected values from preset, caller controls choices separately
apply_preset_selection <- function(session, plot_name, input_id, available_vars) {
  preset_name <- DEFAULT_PRESET[[plot_name]]
  default_sel <- intersect(VAR_PRESETS[[preset_name]], available_vars)
  updateSelectizeInput(session, input_id, selected = default_sel)
}


# ·· GROUPBY DEFAULT ······················································

# groupby default and its levels (NULL = all levels)
#   group_on, means this plot should have a groupby option
GROUPBY_META <- tibble::tribble(
  ~plot,      ~group_on, ~group_var,  ~group_levels,
  "rising",    0,         NA,          NULL,
  "ggpairs",   1,         "NaFlag_S6", NULL,
  "heatmap",   0,         NA,          NULL,  
)

# lookup helper: for groupby default
groupby_for <- function(plot_name) {
  GROUPBY_META[GROUPBY_META$plot == plot_name, ]
}

# logic helper: applies groupby defaults to checkbox, group var selector, and level selector
apply_groupby_defaults <- function(session, plot_name, cat_vars, input_group_var) {
  grp_meta <- groupby_for(plot_name)
  grp_var  <- if (grp_meta$group_var %in% cat_vars) grp_meta$group_var else cat_vars[1]
  updateSelectInput(session, input_group_var, choices = cat_vars, selected = grp_var)
}

# logic helper: resolves level preselection from groupby meta
resolve_group_levels <- function(plot_name, all_lvls) {
  grp_meta <- groupby_for(plot_name)
  if (!is.null(grp_meta$group_levels[[1]])) {
    intersect(grp_meta$group_levels[[1]], all_lvls)  # specific levels from meta
  } else {
    all_lvls                                          # NULL = all levels
  }
}

# ·· GLOBAL COLOUR THEME ··················································

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


# ·· GENERAL HELPER ·······················································

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


# ·· EDA DATASET ··························································

eda_dataset <- ds_typed %>%
  
  mutate(
    
    # derive IdGroup (G and D)
    IdGroup = as.factor(substr(as.character(ID), 1, 1)),
    
    # too few years prefer factor, purely categorical regimes
    Year   = factor(format(Date, "%Y")),
    
    # get weekdays potentially from Monday to Sunday
    Weekday = factor(format(Date, "%A")),
    
    # NA flag specifically for Sensor6
    NaFlag_S6 = as.factor(ifelse(is.na(Sensor6), "Yes", "No"))
    
  ) %>% # end of enriching mutation
  
  # reordering dataset after enriching
  reorder_cols()


# ·· ENRICHED DATASET ·····················································

enriched_dataset <- eda_dataset %>%
  
  # derive IdGroup (G and D)
  mutate(
    
    # # derive IdGroup (G and D)
    # IdGroup = as.factor(substr(as.character(ID), 1, 1)),
    
    # # too few years prefer factor, purely categorical regimes
    # Year   = factor(format(Date, "%Y")),
    
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
    
    # # get weekdays potentially from Monday to Sunday
    # Weekday = factor(format(Date, "%A")),
    
    # ISO week number 1-53 as factor
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
    Sensor28G = ifelse(IdGroup == "G", Sensor28, NA_real_),
    
    # # NA flag specifically for Sensor6
    # NaFlag_S6 = as.factor(ifelse(is.na(Sensor6), "Yes", "No"))
    
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
debug_dataset <- eda_dataset %>%
  mutate(
    # synthetic pair for testing extreme negative correlation (~-1)
    # DebugPos: standardised Y; DebugNeg: its near-perfect mirror
    DebugPos = as.numeric(scale(Y)),
    DebugNeg = -DebugPos + rnorm(n(), mean = 0, sd = 0.02)# example: flag
  )





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
                         verbatimTextOutput("summary_output")
               )
             )
    ), # end of tab panel
    
    
    # ── UI RISING VALUE ───────────────────────────────────────────────────
    
    tabPanel("Rising Value",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Note: <br><br>
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
    
    
    # ── UI GGPAIRS ────────────────────────────────────────────────────────
    
    tabPanel("GGPairs",
             sidebarLayout(
               sidebarPanel(width = 3,
                            sidebar_note("Note: <br><br>
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
                                        selected = "pearson"),
                            hr(),
                            selectInput("hm_order", "Variable ordering:",
                                        choices  = c("Original"               = "FALSE",
                                                     "AOE (Eigenvector)"      = "TRUE",
                                                     "HC (Hierarchical)"      = "HC",
                                                     "OLO (Optimal Leaf)"     = "OLO"),
                                        selected = "FALSE"),
                            hr(),
                            checkboxInput("hm_abs", "Absolute correlation (abs)", value = FALSE),
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
    choices <- c("Raw Dataset", "EDA Dataset", "Enriched Dataset", "Model Dataset")
    
    # (injects "Debug Dataset" when unlocked)
    if (is_unlocked()) choices <- c(choices, "Debug Dataset")
    
    # preserve current selection
    current <- isolate(input$dataset_choice)
    if (is.null(current) || !current %in% choices) current <- "EDA Dataset"
    
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
        tags$dt("EDA Dataset"),
        tags$dd("Raw + derived EDA helper columns for Exploratory Data Analysis 
                (e.g., grouping, pattern inspection, or data integrity checks) 
                without any preprocessing for modeling. 
                (fixed minor schema and naming inconsistencies so that 
                the dataset is correctly interpreted when loaded)"),
        tags$dt("Enriched Dataset"),
        tags$dd("Dataset with additional derived or contextual columns added 
                to enrich the information available for further modelling consideration."),
        tags$dt("Model Dataset"),
        tags$dd("Reduced dataset containing only the variables required for modelling, 
                with unnecessary or potentially leaky columns removed."),
        tags$dt("Debug Dataset", style = "color:#856404;"),
        tags$dd("Explore upon enriched dataset with diagnostic flags and intermediate values 
                for debugging or testing. Only visible when unlocked."),
        tags$dt("Note 1: ", style = "color:#856404;"),
        tags$dd("The Enriched/Model dataset stages are shown here as placeholders within 
                the overall framework. They are not meaningfully implemented at this stage."),
        tags$dt("Note 2: ", style = "color:#856404;"),
        tags$dd("The ID column is intentionally hidden across all dataset views 
                to demonstrate how the framework can protect sensitive attributes. 
                It becomes visible only when unlocked."),
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
           "EDA Dataset"      = eda_dataset,
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
  
  
  # ── SERVER RISING VALUE ────────────────────────────────────────────────
  
  # 1st block: variable selector and preset dropdown initialisation
  observe({
    df <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "rv_vars", choices = num_vars)
    apply_preset_selection(session, "rising", "rv_vars", num_vars)
    # dropdown only shows presets valid for this tab
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("rising"))
    choices <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "rv_preset", choices = choices)
  })
  
  # 2nd block: preset observer — apply selected preset to variable selector
  observeEvent(input$rv_preset, {
    # do nothing if "None" is selected
    req(input$rv_preset != "none")
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    # look up preset vars by name, keep only those that exist in current dataset
    sel <- intersect(VAR_PRESETS[[input$rv_preset]], num_vars)
    # guard: skip update if no matching vars found
    if (length(sel) > 0)
      updateSelectizeInput(session, "rv_vars", selected = sel)
  })
  
  # 3rd block: plot output
  output$rv_output <- renderPlotly({
    req(input$rv_vars)
    df       <- display_data()
    num_vars <- input$rv_vars
    colours  <- theme_colours_for(num_vars)
    # build long-format df
    plot_df <- do.call(rbind, lapply(num_vars, function(v) {
      y <- df[[v]]
      if (input$rv_omit_na) y <- na.omit(y)
      
      y <- switch(input$rv_transform,
                  "center"      = y - mean(y, na.rm = TRUE),
                  "standardise" = as.numeric(scale(y, center = TRUE, scale = TRUE)),
                  "normalise"   = (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)),
                  y
      )
      
      y   <- sort(y, na.last = TRUE)
      pct <- seq_along(y) / length(y) * 100
      data.frame(Percentile = pct, Value = y, Variable = v)
    }))
    
    p <- ggplot(plot_df, aes(x = Percentile, y = Value,
                             colour = Variable,
                             group  = Variable,
                             text   = paste0(Variable,
                                             "<br>Percentile: ", round(Percentile, 1),
                                             "<br>Value: ",      round(Value, 3)))) +
      geom_line(linewidth = input$rv_lwd / 3, linetype = input$rv_lty) +
      scale_colour_manual(values = colours) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "right")
    
    default_title <- paste0(
      "Rising Value Graph for Continuity",
      if (input$rv_omit_na) " | NAs Omitted" else "",
      if (input$rv_transform != "none") paste0(" | ", tools::toTitleCase(input$rv_transform)) else ""
    )
    
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(
        text = if (nzchar(input$rv_title)) paste0("<b>", input$rv_title, "</b>") else 
          paste0("<b>", default_title, "</b>"),
        font = list(size = 28, color = "black"), x = 0.5, y = 0.95),
        margin = list(t = 90),
        legend = list(y = 0.8, yanchor = "middle", font  = list(size = 16),
                      title = list(font = list(size = 16, color = "black"), text = "<b>Variable</b>")),
        xaxis  = list(title    = list(text = "<b>Percentile</b>", font = list(size = 20)), 
                      tickfont = list(size = 18)),
        yaxis  = list(title    = list(text = "<b>Value</b>", font = list(size = 20)),
                      tickfont = list(size = 18))
        )
  })
  
  
  # ── SERVER GGPAIRS ─────────────────────────────────────────────────────
  
  # 1st block: variable selector (initialise selectors when data changes)
  observe({
    # detect variable types
    df       <- display_data()
    all_vars <- names(df)
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    num_vars <- names(df)[sapply(df, is.numeric)]
    # updates the variable selection widget in the UI
    updateSelectizeInput(session, "gg_vars", choices = all_vars)
    # selection driven by preset (from custom helper function)
    apply_preset_selection(session, "ggpairs", "gg_vars", all_vars)
    # applly group-by defaults (from custom helper function)
    apply_groupby_defaults(session, "ggpairs", cat_vars, "gg_group_var")
    # build the preset dropdown, construct dropdown choices, then update preset dropdown
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("ggpairs"))
    choices      <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "gg_preset", choices = choices)
  })
  
  # 2nd block: apply preset selection, triggered when user chooses a preset
  observeEvent(input$gg_preset, {
    req(input$gg_preset != "none")
    df       <- display_data()
    all_vars <- names(df)
    sel      <- intersect(VAR_PRESETS[[input$gg_preset]], all_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "gg_vars", selected = sel)
  })
  
  # 3rd block: groupby level selector
  observeEvent(input$gg_group_var, {
    req(input$gg_group_var)
    df   <- display_data()
    lvls <- sort(unique(as.character(df[[input$gg_group_var]])))
    lvls <- lvls[!is.na(lvls)]
    updateSelectizeInput(session, "gg_group_levels",
                         choices  = lvls,
                         selected = resolve_group_levels("ggpairs", lvls))
  })
  
  # 4th block: output plot
  output$gg_output <- renderPlot({
    input$gg_run
    isolate({
      req(input$gg_vars)
      df <- display_data()
      validate(need(length(input$gg_vars) >= 2, "Please select at least 2 variables."))
      
      default_title <- paste0(
        "GGPairs",
        if (input$gg_group_on && !is.null(input$gg_group_var)) paste0(" | Grouped by ", input$gg_group_var) else ""
      )
      plot_title <- if (nzchar(input$gg_title)) input$gg_title else default_title
      
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
        
        # columns to plot = only the selected vars
        col_idx <- seq_along(input$gg_vars)
        
        GGally::ggpairs(
          plot_df,
          progress = FALSE,
          columns  = col_idx,
          mapping  = aes(colour = .data[[grp]], alpha = 0.6),
          # displaying logic above the diagonal
          upper    = list(
            continuous = GGally::wrap("cor", size = 4),                      # corr
            combo      = GGally::wrap("box_no_facet", alpha = 0.5),          # box plot
            discrete   = GGally::wrap("facetbar", alpha = 0.5)               # bar chart
          ),
          # logic below the diagonal
          lower    = list(
            continuous = GGally::wrap("points", alpha = 0.3, size = 0.8),    # scatter plot
            combo      = GGally::wrap("facethist", bins = 20, alpha = 0.5),  # histogram
            discrete   = GGally::wrap("facetbar", alpha = 0.5)               # bar chart
          ),
          # the diagonal (each variable against itself)
          diag     = list(
            continuous = GGally::wrap("densityDiag", alpha = 0.5),           # density curve
            discrete   = GGally::wrap("barDiag", alpha = 0.5)                # bar chart
          ),
          # takes the legend from panel 1 and displays it for the whole plot
          legend   = 1
        ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title  = element_text(size = 20, face = "bold", hjust = 0.5),
            strip.text  = element_text(size = 14, face = "bold"),
            axis.text   = element_text(size = 14),
            legend.title  = element_text(face = "bold"),
            legend.text   = element_text(size = 12),
            plot.margin = margin(t = 20)
          ) +
          ggplot2::labs(title = plot_title)
        
      } else {
        
        plot_df <- df[, input$gg_vars, drop = FALSE]
        
        GGally::ggpairs(
          plot_df,
          progress = FALSE,
          upper = list(continuous = GGally::wrap("cor", size = 4)),
          lower = list(continuous = GGally::wrap("points", alpha = 0.4, size = 0.8)),
          diag  = list(continuous = GGally::wrap("densityDiag"))
        ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title  = element_text(size = 20, face = "bold", hjust = 0.5),
            strip.text  = element_text(size = 14, face = "bold"),
            axis.text   = element_text(size = 14),
            plot.margin = margin(t = 20)
          ) +
          ggplot2::labs(title = plot_title)
      }
    })
  })
  
  
  # ── SERVER HEATMAP ─────────────────────────────────────────────────────
  
  # 1st block: variable selector and preset dropdown initialisation
  observe({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "hm_vars", choices = num_vars)
    apply_preset_selection(session, "heatmap", "hm_vars", num_vars)
    # dropdown only shows presets valid for this tab
    valid_groups <- Filter(function(v) any(v %in% num_vars), presets_for("heatmap"))
    choices      <- c("None" = "none", setNames(names(valid_groups), names(valid_groups)))
    updateSelectInput(session, "hm_preset", choices = choices)
  })
  
  # 2nd block: preset observer, apply selected preset to variable selector
  observeEvent(input$hm_preset, {
    req(input$hm_preset != "none")
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    sel      <- intersect(VAR_PRESETS[[input$hm_preset]], num_vars)
    if (length(sel) > 0)
      updateSelectizeInput(session, "hm_vars", selected = sel)
  })
  
  # 3rd block: corrgram plot output
  output$hm_output <- renderPlot({
    df       <- display_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    selected <- if (!is.null(input$hm_vars) && length(input$hm_vars) >= 2)
      input$hm_vars else num_vars
    selected <- intersect(selected, num_vars)
    validate(need(length(selected) >= 2, "Select at least 2 numeric variables."))
    
    df_num <- df[, selected, drop = FALSE]
    
    # compute correlation matrix first, then apply abs manually if checked
    cor_mat <- cor(df_num, use = "pairwise.complete.obs", method = input$hm_cor)
    if (isTRUE(input$hm_abs)) cor_mat <- abs(cor_mat)
    
    # build order label
    order_label <- switch(input$hm_order,
                          "FALSE" = "Original Order",
                          "TRUE"  = "AOE (Eigenvector)",
                          "HC"    = "HC (Hierarchical)",
                          "OLO"   = "OLO (Optimal Leaf)"
    )
    
    default_title <- paste0(
      "Corrgram | ", tools::toTitleCase(input$hm_cor),
      " | ", order_label,
      if (isTRUE(input$hm_abs)) " | Absolute" else ""
    )
    
    # outer top margin, pushes title up, 3 lines of space
    par(oma = c(0, 0, 3, 0))
    
    corrgram::corrgram(
      cor_mat,
      order      = if (input$hm_order == "FALSE") FALSE else input$hm_order,
      abs        = FALSE, # already handled above
      cor.method = input$hm_cor,
      cex.var    = 1.2
    )
    
    # overlay larger title (base graphics workaround — corrgram uses base plot)
    plot_title <- if (nzchar(input$hm_title)) input$hm_title else default_title
    title(main = plot_title, cex.main = 2, font.main = 2, outer = TRUE, line = 1)
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
                     sidebar_note("Note: 
                                  <br><br>R console occupied when running shiny app, 
                                  thus build the console in app as a debugging helper. 
                                  <br><br>Run R expressions against the current dataset. 
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