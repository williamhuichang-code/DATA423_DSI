# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(bs4Dash)
library(dplyr)
library(summarytools)
library(plotly)
library(visdat)
library(naniar)
library(gridExtra)
library(dbscan)       # LOF
library(e1071)        # SVM
library(randomForest) # RF
library(isotree)      # Isolation Forest
library(ggrepel)      # label repelling in plots

# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

##### change global configs when needed

# file of interest
FILE_OF_INTEREST <- "a2_14_accepted.csv"

# explicit data folder
DATA_WD <- "."


# ── FILES IN THE WORK DIRECTORY ──────────────────────────────────────────────

csv_files <- list.files(DATA_WD, pattern = "\\.csv$", full.names = FALSE)
file_choices <- if (length(csv_files) == 0) c("(none)") else csv_files
default_selected <- if (FILE_OF_INTEREST %in% csv_files) FILE_OF_INTEREST else "(none)"


# ── LOAD MODULE ──────────────────────────────────────────────────────────────
list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)
print(getwd())
print(csv_files)




