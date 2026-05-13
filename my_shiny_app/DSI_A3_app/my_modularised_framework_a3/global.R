# =================================================================================
# global.R
# =================================================================================

# ── LIBRARY ──────────────────────────────────────────────────────────────────

library(shiny)
library(bs4Dash)
library(dplyr)
library(waiter)
library(plotly)
library(ggrepel)

# ── GLOBAL CONFIG ────────────────────────────────────────────────────────────

# file of interest
FILE_OF_INTEREST <- "Ass2Data.csv"

# explicit data folder
DATA_WD <- "."


# ── FILE LOADING LOGIC ───────────────────────────────────────────────────────

# all csv files as a list
csv_files <- list.files(DATA_WD, pattern = "\\.csv$", full.names = FALSE)

# add a (none) option to choices
file_choices_with_none    <- c("(none)", csv_files)

# prioritise on interested file and fallback at (none)
default_selected <- if (FILE_OF_INTEREST %in% csv_files) FILE_OF_INTEREST else "(none)"


# ── MODULE LOADING LOGIC ─────────────────────────────────────────────────────

# look inside "modules" folder and its subs, load all files with .R according to their full paths
list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE) |>
  lapply(source)





