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
library(vcd)
library(dbscan)
library(paletteer) # global colour theme
library(colorspace)
library(visdat)


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
    
    # NA flag specifically for Sensor6
    NaFlag_S6 = ifelse(is.na(Sensor6), "Yes", "No")
    
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
    # Flag = NA_character_   # empty string column — fill in manually later
    NaFlag_S6 = ifelse(is.na(Sensor6), "Yes", "No")
  )


