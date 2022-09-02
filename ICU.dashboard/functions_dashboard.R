# ==============================
# Helper Functions ICU.dashboard
# --------- Dashboard ----------
# ==============================

library(tidyverse)
library(lubridate)

#' @title Get cases plot
#' @description Retrieves cases plot for dashboard presentation via database connection
#' @param db_con Database connection object
#' @param admission_date Date range of admission date for presentation. Format: yyyy-mm-dd in vector c(start, end)
#' @param interval Presentation interval. c("day" = 1, "month" = 2, "year" = 3)
#' @return plot object
getCasesPlot <- function(db_con, admission_date, interval) {
  sql <- paste0("
  SELECT
    D.CASEID,
    S.ADMISSIONDATE
  FROM
    RP_DEMOGRAPHICS D
  INNER JOIN VP_STAYS S
    ON D.CASEID = S.CASEID
  WHERE
    S.ADMISSIONDATE >= TO_DATE('", admission_date[1], "', 'yyyy-mm-dd')
    AND
    S.DISCHARGEDATE <= TO_DATE('", admission_date[2], "', 'yyyy-mm-dd')
  ")

  result <- dbGetQuery(db_con, sql)
  plot_data <- result %>%
    mutate(across(c("ADMISSIONDATE"), ymd_hms)) %>%
    # generate grouping variables for plotting
    mutate(month_year = format(ADMISSIONDATE, "%m/%Y"),
           year = year(ADMISSIONDATE)) %>%
    arrange(ADMISSIONDATE)
  
  # Plot output
  if(interval == 3) {
    # presentation per year
    ggplot(plot_data) +
      geom_bar(mapping = aes(x = year(ADMISSIONDATE)), fill = "lightgray", color = "black") +
      geom_text(stat = "count", mapping = aes(x = year(ADMISSIONDATE), label = ..count.. ), vjust = 1.5) +
      theme_bw() +
      labs(x = "Jahr", y = "Anzahl") +
      scale_y_continuous(breaks = seq(0, nrow(plot_data), as.numeric(Sys.getenv("CONFIG_DASHBOARD_CASES_PLOT_YEAR_Y_AXIS_MAJOR_BREAKS"))), 
                         minor_breaks = seq(0, nrow(plot_data), as.numeric(Sys.getenv("CONFIG_DASHBOARD_CASES_PLOT_YEAR_Y_AXIS_MINOR_BREAKS")))) +
      scale_x_continuous(breaks = min(plot_data$year):max(plot_data$year)) 
  } else if(interval == 2) {
    # presentation per month
    ggplot(plot_data) +
      geom_bar(mapping = aes(x = month_year), fill = "lightgray", color = "black") +
      geom_text(stat = "count", mapping = aes(x = month_year, label = ..count.. ), vjust = 1.5) +
      theme_bw() +
      labs(x = "Monat", y = "Anzahl") +
      scale_y_continuous(breaks = seq(0, nrow(plot_data), as.numeric(Sys.getenv("CONFIG_DASHBOARD_CASES_PLOT_MONTH_Y_AXIS_MAJOR_BREAKS"))), 
                         minor_breaks = seq(0, nrow(plot_data), as.numeric(Sys.getenv("CONFIG_DASHBOARD_CASES_PLOT_MONTH_Y_AXIS_MINOR_BREAKS")))) +
      scale_x_discrete(breaks = unique(plot_data$month_year)) 
  } else {
    # presentation per day
    ggplot(plot_data) +
      geom_bar(mapping = aes(x = date(ADMISSIONDATE)), fill = "lightgray", color = "black") +
      geom_text(stat = "count", mapping = aes(x = date(ADMISSIONDATE), label = ..count.. ), vjust = 1.5) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Tag", y = "Anzahl") +
      scale_y_continuous(breaks = seq(0, nrow(plot_data), as.numeric(Sys.getenv("CONFIG_DASHBOARD_CASES_PLOT_DAY_Y_AXIS_MAJOR_BREAKS"))),
                         minor_breaks = seq(0, nrow(plot_data), as.numeric(Sys.getenv("CONFIG_DASHBOARD_CASES_PLOT_DAY_Y_AXIS_MINOR_BREAKS")))) +
      scale_x_date(breaks = unique(date(plot_data$ADMISSIONDATE)), labels = format(unique(date(plot_data$ADMISSIONDATE)), "%d.%m.%Y"))
  }
}

#' @title Get mortality plot
#' @description Retrieves mortality plot for dashboard presentation via database connection
#' @param db_con Database connection object
#' @param admission_date Date range of admission date for presentation. Format: yyyy-mm-dd in vector c(start, end)
#' @param interval Presentation interval. c("day" = 1, "month" = 2, "year" = 3)
#' @return plot object
getDeathPlot <- function(db_con, admission_date, interval) {
  sql <- paste0("
  SELECT
    D.CASEID,
    D.ADMISSIONDATE,
    S.TARGETWARD
  FROM
    RP_DEMOGRAPHICS D
  INNER JOIN VP_STAYS S
    ON D.CASEID = S.CASEID
  WHERE
    S.ADMISSIONDATE >= TO_DATE('", admission_date[1], "', 'yyyy-mm-dd')
    AND
    S.DISCHARGEDATE <= TO_DATE('", admission_date[2], "', 'yyyy-mm-dd')
  ")
  
  result <- dbGetQuery(db_con, sql)
  plot_data <- result %>%
    mutate(across(c("ADMISSIONDATE"), ymd_hms)) %>%
    # generate grouping variables for plotting
    mutate(day = date(ADMISSIONDATE),
           month_year = format(ADMISSIONDATE, "%m/%Y"),
           year = year(ADMISSIONDATE)) %>%
    # identify ICU mortality
    mutate(death = (TARGETWARD == Sys.getenv("CONFIG_DASHBOARD_DEATH_IDENTIFYING_TARGETWARD"))) %>%
    arrange(ADMISSIONDATE)
  
  # Plot output
  if(interval == 3) {
    # presentation per year
    plot_data %>%
      group_by(year, death) %>%
      summarise(n = n(), .groups = "keep") %>%
      group_by(year) %>%
      mutate(proportion = n/sum(n)) %>%
      ggplot() +
      geom_col(aes(x = year, y = n, fill = death), color = "black") +
      geom_text(aes(x = year, y = ifelse(death == FALSE, n / proportion, 400), label = paste(n, "\n(", round(proportion * 100, 1), "%)", sep = "")), vjust = 1.5, size = 3) +
      theme_bw() +
      labs(x = "Jahr", y = "Patientenzahl [n (%)]") +
      scale_y_continuous(breaks = seq(0, nrow(result), as.numeric(Sys.getenv("CONFIG_DASHBOARD_DEATH_PLOT_YEAR_Y_AXIS_MAJOR_BREAKS"))), 
                         minor_breaks = seq(0, nrow(result), as.numeric(Sys.getenv("CONFIG_DASHBOARD_DEATH_PLOT_YEAR_Y_AXIS_MINOR_BREAKS")))) +
      scale_fill_discrete(name = "Sterblichkeit", labels = c("nicht verstorben", "verstorben")) +
      scale_x_continuous(breaks = min(plot_data$year):max(plot_data$year))
  } else if(interval == 2) {
    # presentation per month
    plot_data %>%
      group_by(month_year, death) %>%
      summarise(n = n(), .groups = "keep") %>%
      group_by(month_year) %>%
      mutate(proportion = n/sum(n)) %>%
      ggplot() +
      geom_col(aes(x = month_year, y = n, fill = death), color = "black") +
      geom_text(aes(x = month_year, y = ifelse(death == FALSE, n / proportion, 40), label = paste(n, "\n(", round(proportion * 100, 1), "%)", sep = "")), vjust = 1.5, size = 3) +
      theme_bw() +
      labs(x = "Monat", y = "Patientenzahl [n (%)]") +
      scale_y_continuous(breaks = seq(0, nrow(result), as.numeric(Sys.getenv("CONFIG_DASHBOARD_DEATH_PLOT_MONTH_Y_AXIS_MAJOR_BREAKS"))), 
                         minor_breaks = seq(0, nrow(result), as.numeric(Sys.getenv("CONFIG_DASHBOARD_DEATH_PLOT_MONTH_Y_AXIS_MINOR_BREAKS")))) +
      scale_fill_discrete(name = "Sterblichkeit", labels = c("nicht verstorben", "verstorben")) +
      scale_x_discrete(breaks = unique(plot_data$month_year))
  } else {
    # presentation per day
    plot_data %>%
      group_by(day, death) %>%
      summarise(n = n(), .groups = "keep") %>%
      group_by(day) %>%
      mutate(proportion = n/sum(n)) %>%
      ggplot() +
      geom_col(aes(x = day, y = n, fill = death), color = "black") +
      geom_text(aes(x = day, y = ifelse(death == FALSE, n / proportion, 4), label = paste(n, "\n(", round(proportion * 100, 1), "%)", sep = "")), vjust = 1.5, size = 3) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Tag", y = "Patientenzahl [n (%)]") +
      scale_y_continuous(breaks = seq(0, nrow(result), as.numeric(Sys.getenv("CONFIG_DASHBOARD_DEATH_PLOT_DAY_Y_AXIS_MAJOR_BREAKS"))), 
                         minor_breaks = seq(0, nrow(result), as.numeric(Sys.getenv("CONFIG_DASHBOARD_DEATH_PLOT_DAY_Y_AXIS_MINOR_BREAKS")))) +
      scale_fill_discrete(name = "Sterblichkeit", labels = c("nicht verstorben", "verstorben")) +
      scale_x_date(breaks = unique(plot_data$day), labels = format(unique(plot_data$day), "%d.%m.%Y"))
  }
}
