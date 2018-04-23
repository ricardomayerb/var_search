library(MTS)
library(haven)
library(vars)
library(xts)
library(timetk)
library(readxl)
library(lubridate)
library(purrr)
library(forecast)
library(tibbletime)
library(haven)
library(tidyverse)


var_cv <- function(var_data, var_object, n_cv = 8, train_test_marks = NULL) {
  
}

loop_size_vbls_lag <- function(vec_size = c(3,4,5), vec_lags = c(1,2,3,4,5),
                               var_data, is_cv = FALSE) {
  
  # i, outer most loop: var size (number of edogenous variables), e.g. 3, then 4, then 5 variables
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags
  
  
  
}