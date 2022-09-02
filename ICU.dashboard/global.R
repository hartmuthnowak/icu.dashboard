# =============
# ICU.dashboard
# --- Global --
# =============

library(RJDBC)
library(tidyverse)
library(lubridate)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(waiter)

# Options
options(scipen = 999)

# Import helper functions
source("functions_global.R") # Global functions for all modules of ICU.dashboard
source("functions_raw.R") # Raw data export functions
source("functions_aggregated.R") # Aggregated data export functions (extension of raw data export)
source("functions_dashboard.R") # Dashboard functions (plot generation)
