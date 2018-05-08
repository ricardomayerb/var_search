library(MTS)
library(haven)
library(vars)
library(xts)
library(timetk)
library(readxl)
library(lubridate)
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

try_sizes_vbls_lags <- function(var_data, yoy_data, level_data, target_v, vec_size = c(3,4,5), 
                                vec_lags = c(1,2,3,4), pre_selected_v = "",
                               is_cv = FALSE, h_max = 5, n_cv = 8,
                               training_length = 16, maxlag_ccm = 8,
                               bt_factor = 1.4, return_cv = TRUE) {
  
  # print("in try_sizes_vbls_lags, has_timetk_idx(var_data)")
  # print(has_timetk_idx(var_data))
  
  len_size <-  length(vec_size)
  len_lag <- length(vec_lags)
  
  all_names <- colnames(var_data)
  
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
    
    
    sets_of_other_variables <- get_sets_of_variables(
      df = var_data, this_size = this_size, all_variables = all_names, 
      already_chosen = already_chosen, bt_factor = bt_factor,
      maxlag_ccm = maxlag_ccm)
    
    # print(class("sets_of_other_variables"))
    # print(class(sets_of_other_variables))
    # 
    # print("sets_of_other_variables")
    # print(sets_of_other_variables)
    
  
      
# 
#     if (this_size == 3) {
#       sets_of_other_variables <- list(c("tot"), c("imp"), c("exp"))
#     }
#     
#     if (this_size == 4) {
#       sets_of_other_variables <- list(c("tot", "ip"), c("imp", "m1"))
#     }
    
    # len_sets_of_vars <- length(sets_of_other_variables)
    len_sets_of_vars <- ncol(sets_of_other_variables)
    
    var_fixed_size_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
    
    for (j in seq.int(1, len_sets_of_vars)) {
      
      # vec_of_other_vbls <- sets_of_other_variables[[j]]
      vec_of_other_vbls <- sets_of_other_variables[,j]
      vbls_for_var <- c(already_chosen, vec_of_other_vbls)
      
      for (k in seq.int(1, len_lag)) {
        
        model_number <- model_number + 1
        this_lag <- vec_lags[k]

        sub_data = var_data[, vbls_for_var]

        sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)

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
    mutate(accu_yoy = map(cv_fcs, from_diff_to_yoy_accu,
                          yoy_ts = yoy_data, diff_ts = var_data, 
                          level_ts = level_data, 
                          training_length = train_span, n_cv = number_of_cv,
                          h_max = fc_horizon),
           accu_lev = map(cv_fcs, from_diff_to_lev_accu,
                          yoy_ts = yoy_data, diff_ts = var_data, 
                          level_ts = level_data, 
                          training_length = train_span, n_cv = number_of_cv,
                          h_max = fc_horizon)
           ) %>% 
    arrange(mean_cv_rmse) %>% 
    mutate(diff_ranking = 1:n()) %>% 
    arrange(unlist(accu_yoy)) %>% 
    mutate(yoy_ranking = 1:n()) %>% 
    arrange(unlist(accu_lev)) %>% 
    mutate(level_ranking = 1:n()) %>% 
    rename(accu_diff_yoy = mean_cv_rmse) %>% 
    filter((diff_ranking <= 50) | (yoy_ranking <= 50) |
             (level_ranking <= 50))
  
  print(paste("Tried", len_lag, "different choices of lags per each combination"))
  print(paste("Number of models analyzed:", model_number))
  print(paste("CV repetitions:", number_of_cv))
  print(paste("Total estimations and fcs:", number_of_cv*model_number))
  
  cv_objects <- results_all_models %>% dplyr::select(cv_vbl_names, cv_lag, cv_errors, cv_test_data,
                                   cv_fcs) %>% 
    rename(variables = cv_vbl_names, lags = cv_lag)
  
  accu_rankings_models <- results_all_models %>% 
    dplyr::select(cv_vbl_names, cv_lag, accu_diff_yoy, accu_yoy, accu_lev,
           diff_ranking, yoy_ranking, level_ranking) %>% 
    rename(variables = cv_vbl_names, lags = cv_lag)
  
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects))
  } else {
    return(list(accu_rankings_models = accu_rankings_models))
    
  }
  
  
}



get_sets_of_variables <- function(df, this_size, all_variables, 
                                  already_chosen, bt_factor,
                                  maxlag_ccm = 12) {

  len_already_chosen <- length(already_chosen)
  len_other_vbls <- this_size - len_already_chosen
  
  tiao_box_treshold <- 2 / sqrt(nrow(df))
  tresh <- bt_factor * tiao_box_treshold
  
  p_and_ccm_mat <- ccm(df, output = FALSE, lags = maxlag_ccm)
  
  ccm_mat <- p_and_ccm_mat$ccm
  ccm_mat_rgdp <- ccm_mat[1:ncol(df) ,]
  geq_cor <- abs(ccm_mat_rgdp) >= tresh
  geq_cor_row_sums <- rowSums(geq_cor) 
  geq_cor_variables <- geq_cor_row_sums >= 1
    
  passing_variables <- all_variables[geq_cor_variables]
  
  passing_not_alr_chosen <- passing_variables[!passing_variables %in% already_chosen]
  
  n_passing_vbls <- length(passing_not_alr_chosen)
  
  print(paste("We have", n_passing_vbls, "variables, to fill", len_other_vbls,
              "slots in the VAR.Total possible combinations :",
              choose(n_passing_vbls, len_other_vbls)))
  
  combinations <- combn(passing_not_alr_chosen, len_other_vbls)

}

calc_ee <- function(vec_of_dQ, levQ, cut_1 = 3, cut_2 = 7) {
  ee2_start <- cut_1 + 1
  vec_Q_dQ_ee1 <- c(levQ, vec_of_dQ[1:cut_1])
  Q_ee_1 <- mean(cumsum(vec_Q_dQ_ee1))
  vec_Q_dQ_ee2 <- c(Q_ee_1, vec_of_dQ[ee2_start:cut_2])
  Q_ee_2 <- mean(cumsum(vec_Q_dQ_ee2))
  
  return(
    list(ee1 = Q_ee_1, ee2 = Q_ee_2)
  )
}


calc_bp <- function(vec_of_dQ, levQ, cut_1 = 1, cut_2 = 5) {
  bp2_start <- cut_1 + 1
  vec_Q_dQ_bp1 <- c(levQ, vec_of_dQ[1:cut_1])
  Q_bp_1 <- mean(cumsum(vec_Q_dQ_bp1))
  vec_Q_dQ_bp2 <- c(Q_bp_1, vec_of_dQ[bp2_start:cut_2])
  Q_bp_2 <- mean(cumsum(vec_Q_dQ_bp2))
  
  return(
    list(bp1 = Q_bp_1, bp2 = Q_bp_2)
  )
}


var_fc_from_best <- function(rank_tibble, VAR_data, levQ, custom_h = 12) {
  
  end_time_vardata <- VAR_data %>% time %>% last
  start_time_fc <- end_time_vardata + 0.25
  start_year_fc <- floor(start_time_fc)
  start_quarter_fc <- as.integer(4*(start_time_fc - start_year_fc + 0.25))
  
  print("start_quarter_fc")
  print(start_quarter_fc)
  
  
  VARs_from_best_inputs <- rank_tibble %>%
    mutate(
      vfit = map2(
        variables, lags,
        function(x, y) vars::VAR(y = VAR_data[, x], p = y)
      ),
      fc = map(vfit, forecast, h = custom_h),
      fc_rgdp_mean = map(fc, c("forecast", "rgdp", "mean")),
      fc_rgdp_mean = map(fc_rgdp_mean, ts, 
                         start = c(start_year_fc, start_quarter_fc),
                         frequency = 4),
      ee1 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee1"),
      ee2 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee2"),
      bp1 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp1"),
      bp2 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp2")
    )
  
  # fc_with_ave <- add_average_fcs(VARs_from_best_inputs)
  # 
  # fc_with_ave <- fc_with_ave %>%
  #   mutate(
  #     ee1 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee1"),
  #     ee2 = map(map(fc_rgdp_mean, calc_ee, levQ = levQ), "ee2"),
  #     bp1 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp1"),
  #     bp2 = map(map(fc_rgdp_mean, calc_bp, levQ = levQ), "bp2")
  #   )
  
  
  fc_with_ave <- 1
  return(VARs_from_best_inputs)
  # return(list(
  #   var_fc_indiv = VARs_from_best_inputs,
  #   fcs_ave = fc_with_ave
  # ))
  
  
  
}


add_average_fcs <- function(var_fc_tbl, n_ave = c(1, 3, 5)) {
  just_fcs <- var_fc_tbl %>% dplyr::select(fc_rgdp_mean, tibble_id)
  new_just_fcs <- just_fcs
  j_names <- names(just_fcs)
  
  fc_h <- length(var_fc_tbl$fc_rgdp_mean[[1]])
  
  ts_start <- (var_fc_tbl %>%
                 mutate(st = map(fc_rgdp_mean, start)) %>%
                 dplyr::select(st))[[1, 1]]
  
  var_all_with_rankings <- var_fc_tbl %>%
    arrange(RMSE) %>%
    mutate(rmse_ranking = 1:n()) %>%
    arrange(MAE) %>%
    mutate(mae_ranking = 1:n()) %>%
    arrange(Theil) %>%
    mutate(theil_ranking = 1:n())
  
  
  do_list_ave <- function(sorted_tbl) {
    rgdp_fc_as_matrix <- sorted_tbl %>%
      dplyr::select(fc_7_rgdp_mean) %>%
      unlist() %>%
      matrix(ncol = fc_h, byrow = TRUE)
    
    fc_colmeans <- colMeans(rgdp_fc_as_matrix)
    
    return(fc_colmeans)
  }
  
  for (i in 1:length(n_ave)) {
    rmse_id <- paste("ave_rmse", n_ave[i], sep = "_")
    mae_id <- paste("ave_mae", n_ave[i], sep = "_")
    theil_id <- paste("ave_theil", n_ave[i], sep = "_")
    ave_of_ave_id <- paste("ave_r_m_t", n_ave[i], sep = "_")
    
    this_ave_rmse <- var_all_with_rankings %>%
      filter(rmse_ranking <= n_ave[i]) %>%
      do_list_ave()
    
    this_ave_mae <- var_all_with_rankings %>%
      filter(mae_ranking <= n_ave[i]) %>%
      do_list_ave()
    
    this_ave_theil <- var_all_with_rankings %>%
      filter(rmse_ranking <= n_ave[i]) %>%
      do_list_ave()
    
    this_ave_of_aves <- colMeans(rbind(
      this_ave_rmse, this_ave_mae,
      this_ave_theil
    ))
    
    this_ave_rmse_ts <- tk_ts(this_ave_rmse, start = ts_start, frequency = 4)
    this_ave_mae_ts <- tk_ts(this_ave_mae, start = ts_start, frequency = 4)
    this_ave_theil_ts <- tk_ts(this_ave_theil, start = ts_start, frequency = 4)
    this_ave_of_aves_ts <- tk_ts(this_ave_of_aves,
                                 start = ts_start,
                                 frequency = 4
    )
    
    this_ave_rmse_tbl <- tibble(list(this_ave_rmse_ts), rmse_id)
    names(this_ave_rmse_tbl) <- j_names
    
    this_ave_mae_tbl <- tibble(list(this_ave_mae_ts), mae_id)
    names(this_ave_mae_tbl) <- j_names
    
    this_ave_theil_tbl <- tibble(list(this_ave_theil_ts), theil_id)
    names(this_ave_theil_tbl) <- j_names
    
    this_ave_of_aves_tbl <- tibble(list(this_ave_of_aves_ts), ave_of_ave_id)
    names(this_ave_of_aves_tbl) <- j_names
    
    new_ave_fcs <- this_ave_of_aves_tbl %>%
      rbind(this_ave_rmse_tbl) %>%
      rbind(this_ave_mae_tbl) %>%
      rbind(this_ave_theil_tbl)
    
    new_just_fcs <- new_just_fcs %>% rbind(new_ave_fcs)
  }
  
  return(new_just_fcs)
}

