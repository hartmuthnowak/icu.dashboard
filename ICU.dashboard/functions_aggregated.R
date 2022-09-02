# ==============================
# Helper Functions ICU.dashboard
# --- Aggregated data export ---
# -Extension of raw data export-
# ==============================

library(tidyverse)
library(lubridate)

#' @title Get aggregated data from raw data
#' @description Aggregates raw data (input) in a defined intervals and returns a dataframe (output). Intervals will be counted from 0 at ICU admission.
#' @param raw Input raw data to be aggregated (from fn_raw_insertTimesplit)
#' @param demographicdata demographic data (from fn_raw_getDemographicData)
#' @param interval time interval data will be aggregated for (same as raw data input, c("stundenweise", "tageweise", "wochenweise"))
#' @return Dataframe of aggregated data
fn_aggregated_getData <- function(raw, demographic, interval) {
  # Variables for time interval calculation
  time_factor <- case_when(
    interval == "stundenweise" ~ 1,
    interval == "tageweise"    ~ 24,
    interval == "wochenweise"  ~ 168
  )
  
  interval_unit <- case_when(
    interval == "stundenweise" ~ "hour",
    interval == "tageweise"    ~ "day",
    interval == "wochenweise"  ~ "week"
  )
  
  # Aggregate data
  aggregated <- raw %>%
    # include admission date as timepoint 0
    left_join(demographic %>% select(CASEID, ADMISSIONDATE), by = c("CASEID" = "CASEID")) %>%
    # assign aggregation time intervals (can only be >= 0)
    mutate(TIME_INTERVAL = as.period(floor_date(ADMISSIONDATE, unit = interval_unit, week_start = 1) %--% floor_date(ADMINDATE, unit = interval_unit, week_start = 1)) / hours(time_factor)) %>%
    filter(TIME_INTERVAL >= 0) %>%
    # transform to long format - dismiss TEXTVALUES (no statistical calculations possible)
    select(-ADMISSIONDATE, -BEGIN, -END, -contains("TEXTVALUE")) %>%
    pivot_longer(!c(CASEID:ADMINDATE, TIME_INTERVAL), names_to = "ITEM", values_to = "VALUE") %>%
    # aggregate data - NA will be removed
    group_by(CASEID, KIS_CASEID, TREATMENTID, TREATMENTNAME, TREATMENTUNIT, TIME_INTERVAL, ITEM) %>%
    summarize(MEAN = mean(VALUE, na.rm = TRUE),
              SD = sd(VALUE, na.rm = TRUE),
              Q25 = quantile(VALUE, 0.25, na.rm = TRUE),
              MEDIAN = median(VALUE, na.rm = TRUE),
              Q75 = quantile(VALUE, 0.75, na.rm = TRUE),
              MIN = min(VALUE, na.rm = TRUE),
              MAX = max(VALUE, na.rm = TRUE),
              .groups = "keep") %>%
    ungroup() %>%
    # modify column names for easier interpretation of data export
    rename_with(~ paste0("TIME_INTERVAL_SINCE_ADMISSION[", toupper(interval_unit), "]"), contains("TIME_INTERVAL"))
  
  aggregated
}
