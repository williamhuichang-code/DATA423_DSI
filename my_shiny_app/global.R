# =============================================================================
# global.R
# Loaded once when the app starts. Shared across all sessions.
# =============================================================================

library(shiny)
library(ggplot2)
library(GGally)
library(corrgram)
library(vcd)
library(visdat)
library(DT)
library(car)
library(dplyr)

# -----------------------------------------------------------------------------
# LOAD DATA
# To swap datasets: just change the filename here (and update type fixes below)
# -----------------------------------------------------------------------------
dat <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)

# -----------------------------------------------------------------------------
# FIX DATA TYPES
# Dates, ordered factors, etc. must be set manually.
# -----------------------------------------------------------------------------
dat$Date <- as.Date(dat$Date)

# Ordered factors (adjust levels to match your data)
dat$Priority <- factor(dat$Priority, levels = c("Low", "Medium", "High"), ordered = TRUE)
dat$Price    <- factor(dat$Price,    levels = c("Cheap", "Medium", "Expensive"), ordered = TRUE)
dat$Speed    <- factor(dat$Speed,    levels = c("Slow", "Medium", "Fast"), ordered = TRUE)
dat$Duration <- factor(dat$Duration, levels = c("Short", "Long", "Very Long"), ordered = TRUE)

# -----------------------------------------------------------------------------
# CONVENIENCE VECTORS  (used throughout ui.R / server.R)
# These auto-detect column roles so the app works with a different dataset too.
# -----------------------------------------------------------------------------
num_cols    <- names(dat)[sapply(dat, is.numeric)]
factor_cols <- names(dat)[sapply(dat, is.factor)]
date_cols   <- names(dat)[sapply(dat, inherits, "Date")]
all_cols    <- names(dat)

# Sensor columns specifically (for grouped ggpairs)
sensor_cols <- grep("^sensor", num_cols, value = TRUE)
meta_num    <- setdiff(num_cols, sensor_cols)   # Y, and any other non-sensor numerics
