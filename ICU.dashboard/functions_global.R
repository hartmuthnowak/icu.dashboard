# ==============================
# Helper Functions ICU.dashboard
# ---------- Global ------------
# ==============================

# Import libraries
library(stringr)
library(RJDBC)

#' @title Connect to database
#' @description Connect to sql database with credentials and return an connection object.
#' @param server IP address of server
#' @param port Port number of server
#' @param database Name of database
#' @param username Username
#' @param password Password 
#' @return Connection Object
fn_global_dbConnect <<- function(server, port, database, username, password) {
  drv <- JDBC(Sys.getenv("SQL_DRIVER_NAME"), classPath = paste(getwd(), Sys.getenv("SQL_DRIVER_FILE"), sep = "/"))
  dbConnect(drv, paste0(Sys.getenv("SQL_DRIVER_CONN_PRESTRING"), server, ":", port, ":", database), username, password)
}

#' @global Get vector from string in .Renviron with separator 
#' @description Returns vector from string separated by separator (e.g. comma).
#' @param var_name Variable name in environment
#' @param sep Separator string (Standard: ,)
#' @return Vector
fn_global_vectorFromEnv <<- function(var_name, sep = ",") {
  str_split(Sys.getenv(var_name), sep)[[1]]
}

#' @global Get vector from string with separator 
#' @description Returns vector from string separated by separator (e.g. comma).
#' @param string String
#' @param sep Separator string (Standard: ,)
#' @return Vector
fn_global_vectorFromString <<- function(string, sep = ",") {
  if(string != "") {
    str_split(string, sep)[[1]] 
  } else {
    NULL
  }
}

#' @global Get position of a search term in env vector
#' @description Returns position of a search term in .Renvironment vector.
#' @param var_name Variable name in environment
#' @param search_term Search term of which the position will be returned
#' @param sep Separator string (Standard: ,)
#' @return Number
fn_global_findPosInEnvVector <<- function(var_name, search_term, sep = ",") {
  which(Sys.getenv(search_term) == fn_global_vectorFromEnv(var_name, sep))
}

#' @global Get named list from .Renviron variable
#' @description Returns a named list from .Renviron variable.
#' @param names Environment variable name as string for names
#' @param values Environment variable name as string for values (optional, if not set names will be count from 1..length of values)
#' @param values_numeric Custom names are numeric (FALSE = standard)
#' @return Named list
fn_global_getNamedList <<- function(names, values = NULL, values_numeric = FALSE) {
  if(is.null(values)) {
    list <- as.list(seq(1, length(fn_global_vectorFromEnv(names)), 1))
  } else {
    if(values_numeric) {
      list <- as.list(as.numeric(fn_global_vectorFromEnv(values)))
    } else {
      list <- as.list(fn_global_vectorFromEnv(values)) 
    }
  }
  
  names(list) <- fn_global_vectorFromEnv(names)
  
  list
}
