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

loop_size_vbls_lag <- function(vec_size = c(3,4,5), vec_lags = c(1,2,3,4),
                               var_data, target_v, pre_selected_v = "",
                               is_cv = FALSE) {
  
  len_size <-  length(vec_size)
  len_lag <- length(vec_lags)
  
  # i, outer most loop: var size (number of edogenous variables), e.g. 3, then 4, then 5 variables
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags
  
  # I considered including a loop between i and j, loopig through several
  # choices of fixed or preselected variables but I think that makes the code less intuitive and 
  # is not a frequently used feature, so I discarded it. 
  
  for (i in seq.int(1, len_size)) {
    this_size <- vec_size[i]
    
    already_chosen <- c(target_v, pre_selected_v)
    already_chosen <- already_chosen[already_chosen != ""]
    len_already_chosen <- length(already_chosen)
    len_other_vbls <- this_size - len_already_chosen
    
    if (this_size == 3) {
      sets_of_other_variables <- list(c("tot"), c("imp"), c("exp"))
    }
    
    if (this_size == 4) {
      sets_of_other_variables <- list(c("tot", "ip"), c("imp", "m1"))
    }
    
    len_sets_of_vars <- length(sets_of_other_variables)
    
    for (j in seq.int(1, len_sets_of_vars)) {
      
      vec_of_other_vbls <- sets_of_other_variables[[j]]
      vbls_for_var <- c(already_chosen, vec_of_other_vbls)
      
      for (k in seq.int(1, len_lag)) {
        this_lag <- vec_lags[k]
        print(paste("i:", i))
        print(paste("j:", j))
        print(paste("k:", k))
        
        print(paste("vec size:", this_size))
        print("free vars:")
        print(vec_of_other_vbls)
        print("endo vbls:")
        print(vbls_for_var)
        print(paste("lag = ", this_lag))
        
        sub_data = var_data[, vbls_for_var]
        # print(paste("nrow(sub_data) =", nrow(sub_data)))
        # print(paste("ncol(sub_data) =", ncol(sub_data)))
        # print("colnames(sub_data) : ")
        # print(colnames(sub_data))
        
        this_var_obj <- vars::VAR(y = sub_data, p = this_lag, type = "const")
        
      }
      
    }
    
  }
  
  
}



get_sets_of_variables <- function(all_variables, n_vbls, target_vbl = "rgdp",
                                  fixed_vbls = "") {
  
}