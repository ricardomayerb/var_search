library(xts)
# library(zeallot)
library(tidyr)
library(dplyr)
library(timetk)
library(readxl)
library(lubridate)
library(purrr)
library(forecast)
library(ggplot2)
library(tibbletime)
library(purrr)
library(stringr)

make_yoy_xts <- function(df_xts) {
  new_xts <- diff.xts(df_xts, lag = 4, na.pad = FALSE)/lag.xts(
    df_xts, na.pad = FALSE, k = 4)
}

make_yoy_ts <- function(df_ts) {
  new_ts <- diff.ts(df_ts, lag = 4)/lag.ts(df_xts, k = 4)
}


drop_this_vars <- function(df, vars_to_drop) {
  new_df <- df[!(names(df) %in% vars_to_drop)]
}

drop_this_vars_this_country <- function(df, id_col, country, vars_to_drop) {
  if (id_col == country) {
    new_df <- drop_this_vars(df, vars_to_drop)
  }  else {
    new_df <- df
  }
}

to_ts_q <- function(df_xts){
  
  yq_start <- first(index(df_xts))
  yq_end <- last(index(df_xts))
  
  start_year <- year(yq_start)
  start_quarter <- quarter(yq_start)
  this_start = c(start_year, start_quarter)
  
  end_year <- year(yq_end)
  end_quarter <- quarter(yq_end)
  this_end = c(end_year, end_quarter)
  
  this_ts <- tk_ts(df_xts, start = this_start,
                   frequency = 4, silent = TRUE)
  return(this_ts)
}


chop_start_xts <- function(df_xts, start_date){
  subset_string <- paste0(as.Date(start_date), "/")
  new_xts <- df_xts[subset_string]
  return(new_xts)
}

chop_start_end_xts <- function(df_xts, start_date, end_date){
  
  subset_string <- paste0(as.Date(as.yearqtr(start_date)),
                          "/", as.Date(as.yearqtr(end_date) ))
  
  new_xts <- df_xts[subset_string]
  
  return(new_xts)
}


