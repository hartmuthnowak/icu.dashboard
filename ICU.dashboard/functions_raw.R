# ==============================
# Helper Functions ICU.dashboard
# ------ Raw data export -------
# ==============================

library(tidyverse)
library(lubridate)

#' @title Get all cases from DB
#' @description Retrieves all cases from DB.
#' @param db_con Database connection object
#' @return Dataframe of all cases
fn_raw_getAllCases <<- function(db_con) {
  dbGetQuery(db_con, paste0("
  SELECT
    D.CASEID,
    D.BIRTHDATE,
    G.NAME AS GENDER,
    S.ADMISSIONDATE,
    S.DISCHARGEDATE,
    S.TARGETWARD
  FROM
    RP_DEMOGRAPHICS D
  INNER JOIN VD_GENDERS G ON
    D.GENDER = G.ID
  INNER JOIN VP_STAYS S ON
    S.CASEID = D.CASEID
  "))
}

#' @title Get all treatments from DB
#' @description Retrieves all treatments from DB.
#' @param db_con Database connection object
#' @return Dataframe of all treatments
fn_raw_getAllTreatments <<- function(db_con) {
  dbGetQuery(db_con, paste0("
  SELECT
    T.TREATMENTID,
    T.TREATMENTNAME,
    T.SUBSTANCEUNIT
  FROM
    VD_TREATMENTS T
  ORDER BY
    T.TREATMENTNAME ASC
  "))
}

#' @title Get all boxes from DB
#' @description Retrieves all boxes from DB.
#' @param db_con Database connection object
#' @return Dataframe of all boxes
fn_raw_getAllBoxes <<- function(db_con) {
  dbGetQuery(db_con, paste0("
  SELECT
    ODT.ID,
    ODT.NAME
  FROM
    VS_ORDERDETAILTYPE ODT
  ORDER BY
    ODT.ID ASC
  "))
}

#' @title Get case and value count from DB
#' @description Retrieves the case and value count from DB according to set parameters.
#' @param db_con Database connection object
#' @param gender Vector of gender of patients (coding: VD_GENDERS.ID)
#' @param age_start Minimum age of patients (in years)
#' @param age_end Maximum age of patients (in years)
#' @param admission_date Vector of admission_dates. Format: yyyy-mm-dd in vector c(start, end)
#' @param case_ids Vector of case ids to be exported
#' @param case_ids_type Type of case ids (if set) c("his" = 1, "icm" = 2)
#' @param selected_treatments Vector of selected treatments (coding: VD_TREATMENTS.TREATMENTID)
#' @return Dataframe
fn_raw_getCaseAndValueCount <<- function(db_con, gender, age_start, age_end, admission_date, case_ids, case_ids_type, selected_treatments) {
  sql <- paste0("
  SELECT
    D.CASEID,
    D.BIRTHDATE,
    S.ADMISSIONDATE,
    O.TREATMENTID,
    O.TREATMENTNAME,
    O.NUMADMINS
  FROM
    RP_DEMOGRAPHICS D
  INNER JOIN VP_STAYS S
    ON D.CASEID = S.CASEID
  INNER JOIN RP_ORDERS O
    ON D.CASEID = O.CASEID
  WHERE
    D.GENDER IN ('", paste0(gender, collapse = "', '"), "')
    AND
    S.ADMISSIONDATE >= TO_DATE('", admission_date[1], "', 'yyyy-mm-dd')
    AND
    S.DISCHARGEDATE <= TO_DATE('", admission_date[2], "', 'yyyy-mm-dd')
    AND
    O.TREATMENTID IN ('", paste0(selected_treatments, collapse = "', '"), "')
  ",
  ifelse(!is.null(case_ids),
         ifelse(case_ids_type == 1,
                # HIS case ids
                paste0("
                  AND
                  D.HIS_CASEID IN ('", paste0(case_ids, collapse = "', '"), "')
                "),
                # ICM case ids
                paste0("
                  AND
                  D.CASEID IN ('", paste0(case_ids, collapse = "', '"), "')
                ")
         ),
         "")
  )
  result <- dbGetQuery(db_con, sql)
  result %>%
    mutate(across(c("BIRTHDATE", "ADMISSIONDATE"), ymd_hms)) %>%
    mutate(AGE = floor(as.duration(BIRTHDATE %--% ADMISSIONDATE) / dyears(1))) %>%
    filter(AGE >= age_start & AGE <= age_end)
}

#' @title Get raw data from DB
#' @description Retrieves the raw data from DB according to set parameters.
#' @param db_con Database connection object
#' @param gender Vector of gender of patients (coding: VD_GENDERS.ID)
#' @param age_start Minimum age of patients (in years)
#' @param age_end Maximum age of patients (in years)
#' @param admission_date Vector of admission_dates. Format: yyyy-mm-dd in vector c(start, end)
#' @param case_ids Vector of case ids to be exported
#' @param case_ids_type Type of case ids (if set) c("his" = 1, "icm" = 2)
#' @param selected_treatments Vector of selected treatments (coding: VD_TREATMENTS.TREATMENTID)
#' @param selected_boxes optional: if set, export boxes in vector (ids)
#' @param timesplit optional: if set, insert timesplit in continuous treatments c("stundenweise", "tageweise", "wochenweise")
#' @return Dataframe of raw data
fn_raw_getData <<- function(db_con, gender, age_start, age_end, admission_date, case_ids, case_ids_type, selected_treatments, selected_boxes = NULL, timesplit = NULL) {
  sql <- paste0("
  SELECT
    D.CASEID,
    O.ORDERID,
    SPEC.SPECID,
    D.HIS_CASEID KIS_CASEID,
    D.BIRTHDATE,
    S.ADMISSIONDATE,
    S.DISCHARGEDATE,
    O.TREATMENTID,
    O.TREATMENTNAME,
    T.SUBSTANCEUNIT TREATMENTUNIT,
    SPEC.BEGIN,
    SPEC.END,
    A.ADMINDATE,
    A.TEXTVALUE,
    A.NUMVALUE",
  ifelse(!is.null(selected_boxes),
  ",
  ODT.NAME AS BOXNAME,
  OSD.TEXTVALUE AS BOXTEXTVALUE,
  OSD.NUMVALUE AS BOXNUMVALUE",
  ""),
  "              
  FROM
    RP_DEMOGRAPHICS D
  INNER JOIN VP_STAYS S
    ON D.CASEID = S.CASEID
  INNER JOIN RP_ORDERS O
    ON D.CASEID = O.CASEID
  INNER JOIN VD_TREATMENTS T
    ON O.TREATMENTID = T.TREATMENTID
  INNER JOIN RP_ORDERSPECS SPEC
    ON SPEC.CASEID = O.CASEID
    AND
    SPEC.ORDERID = O.ORDERID
  INNER JOIN RP_ORDERSPECADMINS A
    ON A.CASEID = SPEC.CASEID
    AND
    A.ORDERID = SPEC.ORDERID
    AND
    A.SPECID = SPEC.SPECID",
  ifelse(!is.null(selected_boxes), 
  "
  INNER JOIN RP_ORDERSPECDETAILS OSD
    ON OSD.CASEID = SPEC.CASEID
    AND
    OSD.ORDERID = SPEC.ORDERID
    AND
    OSD.SPECID = SPEC.SPECID
  INNER JOIN VS_ORDERDETAILTYPE ODT
    ON ODT.ID = OSD.CONTENTTYPE
  ",
  ""),
  "
  WHERE
    D.GENDER IN ('", paste0(gender, collapse = "', '"), "')
    AND
    S.ADMISSIONDATE >= TO_DATE('", admission_date[1], "', 'yyyy-mm-dd')
    AND
    S.DISCHARGEDATE <= TO_DATE('", admission_date[2], "', 'yyyy-mm-dd')
    AND
    O.TREATMENTID IN ('", paste0(selected_treatments, collapse = "', '"), "')",
  ifelse(!is.null(selected_boxes),
         paste0("
           AND
           OSD.CONTENTTYPE IN ('", paste0(selected_boxes, collapse = "', '"), "')
         "),
         ""
  ),
  ifelse(!is.null(case_ids),
         ifelse(case_ids_type == 1,
                # HIS case ids
                paste0("
                  AND
                  D.HIS_CASEID IN ('", paste0(case_ids, collapse = "', '"), "')
                "),
                # ICM case ids
                paste0("
                  AND
                  D.CASEID IN ('", paste0(case_ids, collapse = "', '"), "')
                ")
         ),
         "")
  )
  writeLines(sql)
  result <- dbGetQuery(db_con, sql)
  
  output <- result %>%
    mutate(across(c("BIRTHDATE", "ADMISSIONDATE", "BEGIN", "END", "ADMINDATE"), ymd_hms)) %>%
    mutate(AGE = floor(as.duration(BIRTHDATE %--% ADMISSIONDATE) / dyears(1))) %>%
    filter(AGE >= age_start & AGE <= age_end)
  
  if(!is.null(selected_boxes)) {
    output %>%
      pivot_wider(names_from = "BOXNAME", values_from = c("BOXTEXTVALUE", "BOXNUMVALUE")) %>%
      select(-ORDERID, -SPECID, -BIRTHDATE, -ADMISSIONDATE, -DISCHARGEDATE, -AGE)
  } else {
    output %>%
      select(-ORDERID, -SPECID, -BIRTHDATE, -ADMISSIONDATE, -DISCHARGEDATE, -AGE)
  }
}

#' @title Inserts a timesplit in continuous treatment
#' @description Inserts a timesplit in continuous treatment every hour, every day (at 0h) or every week (at 0h between sunday and monday). Works only for the same treatment in all entries.
#' @param dataframe dataframe of raw data from fn_raw_getData() for one treatment
#' @param bin bin of timesplit: c("stundenweise", "tageweise", "wochenweise")
#' @return Dataframe
fn_raw_insertTimesplit <<- function(dataframe, bin = Sys.getenv("CONFIG_AGGREGATED_TIMESPAN_PRESELECTED")) {
  bin_unit <- case_when(
    bin == "stundenweise" ~ "hour",
    bin == "tageweise"    ~ "day",
    bin == "wochenweise"  ~ "week"
  )
  
  # determine whether timesplit is present in rows (with all rows remaining in dataframe)
  pre_output <- dataframe %>%
    arrange(BEGIN) %>%
    mutate(BIN_SIZE = fn_raw_insertTimesplit_endsOnOtherBin(BEGIN, END, bin_unit)) %>%
    mutate(ROW_ID = 1:n())
  
  # create dataframe of new rows with timesplits (which will be changed)
  new_rows <- pre_output %>%
    filter(BIN_SIZE > 0) %>%
    group_by(ROW_ID) %>%
    fn_raw_createTimesplitRows(., bin_unit)
  
  # create dataframe of old rows without timesplits (which will not be changed)
  old_rows <- pre_output %>%
    filter(BIN_SIZE == 0)
  
  # create output dataframe
  output <- pre_output %>%
    filter(BIN_SIZE > 0) %>%
    select(-BEGIN, -END, -ADMINDATE) %>%
    inner_join(new_rows %>% select(ROW_ID, BEGIN, END, ADMINDATE), by = c("ROW_ID" = "ROW_ID")) %>%
    select(CASEID:TREATMENTUNIT, BEGIN, END, ADMINDATE, TEXTVALUE:ROW_ID) %>%
    bind_rows(old_rows) %>%
    select(-BIN_SIZE, -ROW_ID) %>%
    arrange(CASEID, TREATMENTID, ADMINDATE)
  
  output
}

#' @title Creates a new timesplit dataframe from one dataframe row
#' @description Creates a new dataframe from one dataframe row with number of rows according to skipped bins
#' @param .data Dataframe row from fn_raw_insertTimesplit (rawdata)
#' @param bin bin of timesplit = c("hour", "day", "week")
fn_raw_createTimesplitRows <<- function(.data, bin) {
  factor <- case_when(
    bin == "hour" ~ 1,
    bin == "day"  ~ 24,
    bin == "week" ~ 168
  )
  
  output <- .data %>%
    do(., tibble(
            BEGIN = c(.$BEGIN, floor_date(.$BEGIN, unit = bin, week_start = 1) + 1:.$BIN_SIZE * hours(factor)),
            END = c(floor_date(.$BEGIN, unit = bin, week_start = 1) + 1:.$BIN_SIZE * hours(factor) - seconds(1), .$END),
            ADMINDATE = BEGIN
          )   
    )
  
  output
}

#' @title Checks if continuous treatment ends on other bin
#' @description Checks if continuous treatment ends on other bin (for hour-wise, day-wise and week-wise split)
#' @param start start datetime of treatment
#' @param end end datetime of treatment
#' @param bin bin of timesplit = c("hour", "day", "week")
#' @return Number of bins skipped
fn_raw_insertTimesplit_endsOnOtherBin <<- function(start, end, bin) {
  factor <- case_when(
    bin == "hour" ~ 1,
    bin == "day"  ~ 24,
    bin == "week" ~ 168
  )
  
  as.period(floor_date(start, unit = bin, week_start = 1) %--% floor_date(end, unit = bin, week_start = 1)) / hours(factor)
}

#' @title Get demographic data from DB
#' @description Retrieves the demographic data from DB according to set parameters.
#' @param db_con Database connection object
#' @param gender Vector of gender of patients (coding: VD_GENDERS.ID)
#' @param age_start Minimum age of patients (in years)
#' @param age_end Maximum age of patients (in years)
#' @param admission_date Vector of admission_dates. Format: yyyy-mm-dd in vector c(start, end)
#' @param case_ids Vector of case ids to be exported
#' @param case_ids_type Type of case ids (if set) c("his" = 1, "icm" = 2)
#' @param selected_treatments Vector of selected treatments (coding: VD_TREATMENTS.TREATMENTID)
#' @return Dataframe of demographic data
fn_raw_getDemographicData <<- function(db_con, gender, age_start, age_end, admission_date, case_ids, case_ids_type, selected_treatments) {
  sql <- paste0("
  SELECT
    D.CASEID,
    D.HIS_CASEID KIS_CASEID,
    D.HIS_PATIENTID KIS_PATIENTID,
    D.COSTCENTER,
    G.NAME SEX,
    D.BIRTHDATE,
    D.HEIGHT,
    D.WEIGHT,
    S.ADMISSIONDATE,
    S.DISCHARGEDATE,
    S.TARGETWARD,
    S.STAYLENGTH
  FROM
    RP_DEMOGRAPHICS D
  INNER JOIN VD_GENDERS G
    ON D.GENDER = G.ID
  INNER JOIN VP_STAYS S
    ON D.CASEID = S.CASEID
  INNER JOIN RP_ORDERS O
    ON D.CASEID = O.CASEID
  WHERE
    D.GENDER IN ('", paste0(gender, collapse = "', '"), "')
    AND
    S.ADMISSIONDATE >= TO_DATE('", admission_date[1], "', 'yyyy-mm-dd')
    AND
    S.DISCHARGEDATE <= TO_DATE('", admission_date[2], "', 'yyyy-mm-dd')
    AND
    O.TREATMENTID IN ('", str_c(selected_treatments, collapse = "', '"), "')
  ",
  ifelse(!is.null(case_ids),
         ifelse(case_ids_type == 1,
                # HIS case ids
                paste0("
                  AND
                  D.HIS_CASEID IN ('", paste0(case_ids, collapse = "', '"), "')
                "),
                # ICM case ids
                paste0("
                  AND
                  D.CASEID IN ('", paste0(case_ids, collapse = "', '"), "')
                ")
         ),
         "")
  )
  result <- dbGetQuery(db_con, sql)
  result %>%
    mutate(across(c("BIRTHDATE", "ADMISSIONDATE"), ymd_hms)) %>%
    mutate(AGE = floor(as.duration(BIRTHDATE %--% ADMISSIONDATE) / dyears(1))) %>%
    select(CASEID:BIRTHDATE, AGE, HEIGHT:STAYLENGTH) %>%
    filter(AGE >= age_start & AGE <= age_end) %>%
    distinct()
}