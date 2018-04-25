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


var_cv <- function(var_data, this_p, this_type = "const", n_cv = 8, h_max = 6, 
                   train_test_marks = NULL,
                   training_length = 20, timetk_idx = TRUE,
                   external_idx = NULL) {
  
  if (is.null(train_test_marks)) {
    train_test_dates <- make_test_dates_list(ts_data = var_data, 
                        type = "tscv", n = n_cv, h_max = h_max, 
                        training_length = training_length, 
                        timetk_idx = timetk_idx, 
                        external_idx = external_idx)
    
    train_test_dates <- train_test_dates[["list_of_year_quarter"]]
  }
  
  n <- nrow(var_data)
  
  cv_errors <- list_along(1:n_cv)
  cv_test_data <- list_along(1:n_cv)
  cv_fcs <- list_along(1:n_cv)
  cv_vbl_names <- list_along(1:n_cv)
  cv_lag <- list_along(1:n_cv)
  
  for (i in seq_along(1:n_cv)) {
    
    this_tra_s <- train_test_dates[[i]]$tra_s
    this_tra_e <- train_test_dates[[i]]$tra_e
    
    this_tes_s <- train_test_dates[[i]]$tes_s
    this_tes_e <- train_test_dates[[i]]$tes_e
    
    training_y <- window(var_data, 
                         start = this_tra_s,
                         end = this_tra_e)
    
    # print("nrow(training_y)")
    # print(nrow(training_y))
    
    training_rgdp <- training_y[ , "rgdp"]
    
    test_y <- window(var_data, 
                     start = this_tes_s,
                     end = this_tes_e)

    test_rgdp <- test_y[ , "rgdp"]
    

    this_var <- VAR(y = training_y, p = this_p, type = this_type) 

    this_fc <- forecast(this_var, h = h_max)
    
    this_rgdp_fc_mean <- this_fc[["forecast"]][["rgdp"]][["mean"]]

    fc_error <- test_rgdp - this_rgdp_fc_mean
    
    vbl_names <- colnames(training_y)
    
    lag <- this_p
    
    cv_vbl_names[[i]] <- vbl_names
    cv_lag[[i]] <- lag
    cv_errors[[i]] <- fc_error
    cv_test_data[[i]] <- test_rgdp
    cv_fcs[[i]] <- this_rgdp_fc_mean
    
  }
  
  cv_test_data_mat <- reduce(cv_test_data, rbind)
  cv_fcs_mat <- reduce(cv_fcs, rbind)

  # eliminate pesky "out" of it
  dimnames(cv_test_data_mat) <- NULL
  dimnames(cv_fcs_mat) <- NULL

  mean_cv_rmse <- fcs_accu(cv_fcs_mat, cv_test_data_mat)
  
  return(list(cv_errors = cv_errors,
              cv_test_data = cv_test_data,
              cv_fcs = cv_fcs,
              mean_cv_rmse = mean_cv_rmse,
              cv_vbl_names = cv_vbl_names,
              cv_lag = cv_lag))
}

try_sizes_vbls_lags <- function(var_data, target_v, vec_size = c(3,4,5), 
                                vec_lags = c(1,2,3,4), pre_selected_v = "",
                               is_cv = FALSE, h_max = 5, n_cv = 8,
                               training_length = 16) {
  
  # print("in try_sizes_vbls_lags, has_timetk_idx(var_data)")
  # print(has_timetk_idx(var_data))
  
  len_size <-  length(vec_size)
  len_lag <- length(vec_lags)
  
  
  
  # i, outer most loop: var size (number of edogenous variables), e.g. 3, then 4, then 5 variables
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags
  
  # I considered including a loop between i and j, loopig through several
  # choices of fixed or preselected variables but I think that makes the code less intuitive and 
  # is not a frequently used feature, so I discarded it. 
  
  results_all_models <- list_along(seq.int(1, len_size))
  fcs_var_all_sizes <- list_along(seq.int(1, len_size))
  
  var_fixed_size_fixed_vset_all_lags <- list_along(seq.int(1, len_lag))
  fcs_fixed_size_fixed_vset_all_lags <- list_along(seq.int(1, len_lag))
  
  model_number <- 0
  
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
    
    var_fixed_size_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
    
    for (j in seq.int(1, len_sets_of_vars)) {
      
      vec_of_other_vbls <- sets_of_other_variables[[j]]
      vbls_for_var <- c(already_chosen, vec_of_other_vbls)
      
      for (k in seq.int(1, len_lag)) {
        
        model_number <- model_number + 1
        
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

        sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
        
        # this_var_obj <- vars::VAR(y = sub_data, p = this_lag, type = "const")
        
        this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                          external_idx = sub_data_tk_index, this_p = this_lag,
                          this_type = "const", h_max = h_max,
                          n_cv = n_cv, training_length = training_length)
        
        var_fixed_size_fixed_vset_all_lags[[k]] <- this_cv
        
      }
      
      est_var_this_vset <- var_fixed_size_fixed_vset_all_lags
      var_fixed_size_all_vset_all_lags[[j]] <- est_var_this_vset
      
    }
    
    est_var_this_size <- var_fixed_size_all_vset_all_lags
    results_all_models[[i]] <- est_var_this_size 
    
  }
  
  results_all_models <- flatten(flatten(results_all_models))
  column_names <- names(results_all_models[[1]])
  
  # transitory names to allow conversion to tibble (columns must be names)
  names(results_all_models) <- seq_along(results_all_models)
  
  # transpose tibble, ensure result is still a tibble
  results_all_models <- as_tibble(t(as_tibble(results_all_models)))
  names(results_all_models) <- column_names 
  
  results_all_models <- results_all_models %>% 
    mutate(cv_vbl_names = map(cv_vbl_names, 1),
           cv_lag = map(cv_lag, 1),
           mean_cv_rmse = unlist(mean_cv_rmse)
    ) %>% 
    arrange(mean_cv_rmse) %>% 
    mutate(ranking = 1:n()) %>% 
    filter(ranking <= 100)
  
  return(results_all_models)
}



get_sets_of_variables <- function(all_variables, n_vbls, target_vbl = "rgdp",
                                  fixed_vbls = "") {
  
}