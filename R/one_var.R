library(MTS)
library(xts)
library(tidyverse)
library(haven)
library(lubridate)
library(timetk)
library(vars)
library(forecast)


search_over_ap_tset_lags_size_comb <-
  function(ap_list, dates_list, lags_vec, target, VAR_data,
             this_id, bt = 1.4, sizes_vec, n_best,
             return_all_res = TRUE) {
    tictoc::tic("all_ex")

    all_res <- list()


    for (i in 1:length(ap_list)) {
      this_id <- paste0("ex", 1)

      tictoc::tic(this_id)

      this_a_priori <- ap_list[[i]]

      res <- search_for_VARs(
        VAR_data = VAR_data, target_variables = target,
        a_priori_variables = this_a_priori,
        vec_lags = lags_vec,
        in_the_top_n = n_best, bt_factor = bt,
        vec_n_endog = sizes_vec,
        id_label = this_id, sets_dates = dates_list
      )

      all_res[[i]] <- res

      tictoc::toc()
    }

    top_n_all <- reduce(map(all_res, "top_n_mods"), rbind)

    ferr_all <- reduce(map(all_res, "ferr_rank_n"), rbind)

    inputs_n_all <- reduce(map(all_res, "inputs_n"), rbind)

    print(paste(
      "Total number of VARs fitted and tested:",
      length(dates_list) * nrow(ferr_all)
    ))


    final_best_models <- reduce_top_n(top_n_all, inputs_n_all,
      best_n = n_best, podium_n = 1
    )

    if (return_all_res) {
      return(list(final_best_models, all_res))
    } else {
      return(list(final_best_models))
    }
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

var_fc_from_best <- function(inputs_best, VAR_data, levQ, custom_h = 12) {
  VARs_from_best_inputs <- inputs_best %>%
    mutate(
      vfit = map2(
        variables, lags,
        function(x, y) vars::VAR(y = VAR_data[, x], p = y)
      ),
      fc_7 = map(vfit, forecast, h = 7),
      fc_7_rgdp_mean = map(fc_7, c("forecast", "rgdp", "mean")),
      ee1 = map(map(fc_7_rgdp_mean, calc_ee, levQ = levQ), "ee1"),
      ee2 = map(map(fc_7_rgdp_mean, calc_ee, levQ = levQ), "ee2"),
      bp1 = map(map(fc_7_rgdp_mean, calc_bp, levQ = levQ), "bp1"),
      bp2 = map(map(fc_7_rgdp_mean, calc_bp, levQ = levQ), "bp2")
    )

  fc_with_ave <- add_average_fcs(VARs_from_best_inputs)

  fc_with_ave <- fc_with_ave %>%
    mutate(
      ee1 = map(map(fc_7_rgdp_mean, calc_ee, levQ = levQ), "ee1"),
      ee2 = map(map(fc_7_rgdp_mean, calc_ee, levQ = levQ), "ee2"),
      bp1 = map(map(fc_7_rgdp_mean, calc_bp, levQ = levQ), "bp1"),
      bp2 = map(map(fc_7_rgdp_mean, calc_bp, levQ = levQ), "bp2")
    )

  return(list(
    var_fc_indiv = VARs_from_best_inputs,
    fcs_ave = fc_with_ave
  ))
}


add_average_fcs <- function(var_fc_tbl, n_ave = c(1, 3, 5)) {
  just_fcs <- var_fc_tbl %>% dplyr::select(fc_7_rgdp_mean, tibble_id)
  new_just_fcs <- just_fcs
  j_names <- names(just_fcs)

  fc_h <- length(var_fc_tbl$fc_7_rgdp_mean[[1]])

  ts_start <- (var_fc_tbl %>%
    mutate(st = map(fc_7_rgdp_mean, start)) %>%
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




search_for_VARs <- function(VAR_data, target_variables = "rgdp",
                            a_priori_variables = NULL, vec_lags = c(2, 3),
                            in_the_top_n = 3, bt_factor = 1.5,
                            vec_n_endog = c(4, 5),
                            id_label = "foo", sets_dates = NULL) {
  all_results <- list()

  i <- 1

  label_a_priori_part <- paste(substr(a_priori_variables, 1, 4), collapse = "_")

  for (vs in vec_n_endog) {
    label_varsize_part <- paste0("vs", vs)

    this_res_label <- paste(label_a_priori_part, label_varsize_part,
      id_label,
      sep = "_"
    )

    print(this_res_label)

    result <- search_for_VARs_one_vec_size(
      data_in_var = data_in_diff, target_variables = target_variables,
      a_priori_variables = a_priori_variables, n_endog = vs,
      max_lags_to_try = vec_lags, results_id = this_res_label,
      n_best_in_fc_err = in_the_top_n, bt_factor = bt_factor,
      sets_dates = sets_dates
    )

    all_results[[i]] <- result

    i <- i + 1
  }

  top_n_mods <- reduce(map(all_results, "top_n"), rbind) %>%
    unite(unique_model_index, c(model_index, tibble_id), remove = FALSE) %>%
    dplyr::rename(non_unique_model_index = model_index) %>%
    dplyr::select(-c(rmse_ranking, mae_ranking, theil_ranking)) %>%
    arrange(RMSE)

  inputs_n <- reduce(map(all_results, "inputs_for_var_top_n"), rbind) %>%
    unite(unique_model_index, c(model_index, tibble_id), remove = FALSE) %>%
    dplyr::rename(non_unique_model_index = model_index)

  ferr_rank_n <- reduce(map(all_results, "all_models_err_rankings"), rbind) %>%
    unite(unique_model_index, c(model_index, tibble_id), remove = FALSE) %>%
    dplyr::rename(non_unique_model_index = model_index)


  return(list(
    top_n_mods = top_n_mods, inputs_n = inputs_n,
    ferr_rank_n = ferr_rank_n
  ))
}



search_for_VARs_one_vec_size <- function(data_in_var, target_variables = c("rgdp"),
                                         a_priori_variables = NULL, n_endog = 5,
                                         bt_factor = 1.2, max_lags_to_try = c(3, 5),
                                         fc_horizons = c(3, 7), results_id = NULL,
                                         n_best_in_fc_err = 5,
                                         sets_dates = NULL) {
  VAR_size <- n_endog

  if (!is.null(sets_dates)) {
    n_training_sets <- length(sets_dates)
  } else {
    n_training_sets <- 1
  }

  cmm_res <- get_ccm_variables(
    df = data_in_var,
    intended_var_size = VAR_size,
    target_variables = target_variables,
    a_priori_variables = a_priori_variables,
    maxlag = 12, box_tiao_factor = bt_factor
  )

  set_of_free_variables <- cmm_res$set_of_vars_by_lag[2]

  # print(paste("set_of_free_variables: ", set_of_free_variables))

  n_comb_free_vars <- ncol(set_of_free_variables[[1]])
  n_of_max_lags <- length(max_lags_to_try)

  print(paste("number of combn of free variables = ", n_comb_free_vars))
  print(paste("number of lag specifications = ", n_of_max_lags))
  print(paste("number of training sets = ", n_training_sets))
  print(paste(
    "number of VARs to estimate and evalaute = ",
    n_comb_free_vars * n_of_max_lags * n_training_sets
  ))

  tictoc::tic("tictoc")
  results_VAR_fc <- get_VARs_evaluation(
    set_of_free_variables = set_of_free_variables,
    target_variables = target_variables,
    a_priori_variables = a_priori_variables, VAR_data = data_in_var,
    varlag = max_lags_to_try,
    f_horizons = fc_horizons, dates_list = sets_dates
  )
  tictoc::toc()

  # results_VAR_fc

  # print(names(results_VAR_fc))

  best_models_tbl <- get_best_models_tbl(
    results_list = results_VAR_fc,
    n_best_per_error = n_best_in_fc_err,
    results_id = results_id
  )

  return(best_models_tbl)
}






one_var <- function(varnames, VAR_data, thislag = 3, f_horizons = c(3, 7),
                    start_training_set_index = NULL, last_training_index = NULL,
                    test_set_nobs = NULL, training_set_start_date = NULL,
                    training_set_end_date = NULL, test_set_start_date = NULL,
                    test_set_end_date = NULL, use_dates = FALSE,
                    return_forecast_object = TRUE) {
  ncolumns <- ncol(VAR_data)
  data_th <- nrow(VAR_data)

  VAR_data_start_date <- stats::start(VAR_data)
  VAR_data_end_date <- stats::end(VAR_data)

  partial_horizon <- f_horizons[1]
  total_horizon <- f_horizons[2]

  if (use_dates) {
    if (is.null(training_set_start_date)) {
      training_set_start_date <- VAR_data_start_date
    }
    if (is.null(test_set_end_date)) {
      test_set_end_date <- VAR_data_end_date
    }

    # print(paste("training start date: ", paste(training_set_start_date, collapse = "-")))
    # print(paste("training end date: ", paste(training_set_end_date, collapse = "-")))
    # print(paste("test start date: ", paste(test_set_start_date, collapse = "-")))
    # print(paste("test end date: ", paste(test_set_end_date, collapse = "-")))

    training_set <- window(VAR_data,
      start = training_set_start_date,
      end = training_set_end_date
    )

    test_set <- window(VAR_data,
      start = test_set_start_date,
      end = test_set_end_date
    )

    n_obs_test_set <- nrow(test_set)
    # end_of_training_Q <- last(data_in_diff[,1])
  } else {
    if (is.null(test_set_nobs)) {
      test_set_nobs <- total_horizon
    }
    if (is.null(start_training_set_index)) {
      start_training_set_index <- 1
    }

    if (is.null(last_training_index)) {
      # print("training set is the largest posible for the chosen horizon")
      training_end_index <- data_th - total_horizon
      # print("training_end_index = ")
      # print(training_end_index)
    } else {
      training_end_index <- last_training_index
    }

    training_set <- subset(VAR_data,
      start = start_training_set_index,
      end = training_end_index
    )

    test_set <- subset(VAR_data,
      start = training_end_index + 1,
      end = training_end_index + test_set_nobs
    )
    # end_of_training_Q <- last(data_in_diff[,1])

    n_obs_test_set <- nrow(test_set)
  }

  # print("VAR estimation, varnames:")
  # print(varnames)
  VAR_training_set <- vars::VAR(training_set[, varnames], p = thislag)

  fc_VAR_using_training_set <- forecast(VAR_training_set, h = n_obs_test_set)

  fc_Q_diff <- fc_VAR_using_training_set[[2]][[1]][[3]]

  # fc_Q_diff <- fc_VAR_using_training_set[["forecast"]][["rgdp"]][["mean"]]

  # Q_diff_first_partition <- fc_Q_diff[1:f_horizons[1]]
  #
  # vec_to_accumulate <- c(end_of_training_Q, Q_diff_first_partition )
  # Qs_of_first_partition <- cumsum( vec_to_accumulate )
  # fc_current_year_growth <- mean(Qs_of_first_partition)
  #
  # Q_diff_second_partition <- fc_Q_diff[4:7]
  #
  # vec_to_accumulate_secpart <- c(last(Qs_of_first_partition),
  #                                Q_diff_second_partition )
  #
  # almost_Qs_of_second_partition <- cumsum(vec_to_accumulate_secpart)
  #
  # Qs_of_second_partition <- almost_Qs_of_second_partition[-1]
  # fc_next_year_growth <- mean(Qs_of_second_partition)

  # fc_Q_diff <- fc_VAR_on_test_set $forecast$rgdp$mean

  accu_all <- accuracy(fc_VAR_using_training_set,
    test_set,
    D = 0, d = 1
  )

  test_indices <- seq(from = 2, to = nrow(accu_all), by = 2)

  accu_on_test_set <- accu_all[test_indices, ]

  rgdp_accu <- accu_on_test_set[1, c(2, 3, 8)]

  if (return_forecast_object) {
    forecast_object_to_return <- fc_VAR_using_training_set
  } else {
    forecast_object_to_return <- NULL
  }

  return(list(
    test_data = test_set,
    training_data = training_set,
    fc_Q = fc_Q_diff,
    accu_total_horizon = accu_on_test_set,
    accu_rgdp = rgdp_accu,
    forecast_obj = forecast_object_to_return
  ))


  # acc_test <- accuracy(fc_train, fc_test_data, D = 0, d = 1)
  # thisvar <- VAR(d_data[ , varnames], p = thislag)
  # ser <- serial.test(thisvar, lags.pt = 16, type = "PT.asymptotic")
  # indep_residuals <- ser$serial$p.value < 0.05
  # varroots <- roots(thisvar)
  # is_stable <- all(varroots < 1)
  #
}


get_ccm_variables <- function(df, maxlag = 12, box_tiao_factor = 1,
                              intended_var_size = 6,
                              target_variables = "rgdp",
                              a_priori_variables = NULL) {
  df_names <- colnames(df)
  
  if (is.null(a_priori_variables)) {
    all_rest_variables <- df_names[!(df_names %in% c(
      target_variables
    ))]
  } else {
    all_rest_variables <- df_names[!(df_names %in% c(
      target_variables,
      a_priori_variables
    ))]
    
  }


  df_target_and_rest <- df[, c(target_variables, all_rest_variables)]

  names_df_target_and_rest <- colnames(df_target_and_rest)

  # print("names_df_target_and_rest")
  # print(names_df_target_and_rest)

  result_ccm <- ccm(df_target_and_rest, output = FALSE, lags = maxlag)

  tiao_box_treshold <- 2 / sqrt(nrow(df))

  n_target <- length(target_variables)
  
  if (is.null(a_priori_variables)) {
    print("No a priori variables specified")
    n_a_priori <- 0
  } else {
    n_a_priori <- length(a_priori_variables)
  }
  

  n_rest_in_var <- intended_var_size - n_target - n_a_priori

  n_vars_to_choose_from <- length(all_rest_variables)
  n_combinations_all_variables <- choose(n_vars_to_choose_from, n_rest_in_var)

  # matrices are stacked in a vector column by column
  myccms <- result_ccm$ccm
  myccms_box_tiao <- abs(myccms) >= (box_tiao_factor * tiao_box_treshold)

  cor_vari_lags <- list()
  cor_vari_lags_with_rgdp <- list()
  cor_vari_lags_box_tiao <- list()
  cor_vari_lags_with_rgdp_box_tiao <- list()
  names_cor_lag_with_rgdp <- list()
  n_combs_list <- list()
  set_variables_some_lags <- list()

  for (i in 1:ncol(myccms)) {
    # print("i")
    # print(i)

    thismat_box_tiao <- myccms_box_tiao[, i]
    thismat_box_tiao <- matrix(thismat_box_tiao,
      ncol = ncol(df_target_and_rest),
      nrow = ncol(df_target_and_rest)
    )
    # print(thismat_box_tiao)
    # print("thismat_box_tiao")


    cor_vari_lags_box_tiao[[i]] <- thismat_box_tiao
    cor_vari_lags_with_rgdp_box_tiao[[i]] <- thismat_box_tiao[, 1]

    # print(thismat_box_tiao[,1])
    # print("thismat_box_tiao[,1]")

    thisnames <- names_df_target_and_rest[thismat_box_tiao[, 1]]

    # print("thisnames")
    # print(thisnames)

    names_cor_lag_with_rgdp[[i]] <- thisnames

    thismat <- myccms[, i]
    thismat <- matrix(thismat,
      ncol = ncol(df_target_and_rest),
      nrow = ncol(df_target_and_rest)
    )

    cor_vari_lags[[i]] <- thismat

    thismat_rgdp <- thismat[, 1]
    names(thismat_rgdp) <- names_df_target_and_rest
    cor_vari_lags_with_rgdp[[i]] <- thismat_rgdp

  }

  vec_names_cor_with_rgdp <- unique(unlist(names_cor_lag_with_rgdp))

  free_vars_maxlags <- vec_names_cor_with_rgdp[
    !(vec_names_cor_with_rgdp == target_variables)
  ]

  n_free_vars_maxlags <- length(free_vars_maxlags)
  n_combinations_maxlag <- choose(n_free_vars_maxlags, n_rest_in_var)

  sublags <- seq(from = 3, to = maxlag, by = 2)

  names_upto_thislags <- list()
  free_vars_upto_thislags <- list()

  for (j in 1:length(sublags)) {
    thislag <- sublags[j]
    thisset <- unique(unlist(names_cor_lag_with_rgdp[2:thislag]))
    names_upto_thislags[[j]] <- thisset

    free_vars_thislag <- thisset[
      !(thisset == target_variables)
    ]

    free_vars_upto_thislags[[j]] <- free_vars_thislag

    n_free_vars_thislag <- length(free_vars_thislag)

    if (n_free_vars_thislag < n_rest_in_var) {
      print(free_vars_thislag)
      warn <- paste(
        "Not enough variables for VAR. Lag =", thislag, ": only ", n_free_vars_thislag,
        "pass the test and we need a minimum of ",
        n_rest_in_var
      )
      print(warn)
      n_combs_thislag <- 0
      # (n_combs_thislag)
    } else {
      n_combs_thislag <- choose(n_free_vars_thislag, n_rest_in_var)
      n_combs_list[[j]] <- n_combs_thislag
      set_of_variables_this_lag <- combn(free_vars_thislag, n_rest_in_var)
      # print(set_of_variables_this_lag)
      set_variables_some_lags[[j]] <- set_of_variables_this_lag
    }
  }

  return(
    list(
      shoo = myccms,
      ccm_rgdp_list = cor_vari_lags_with_rgdp,
      ccm_rgdp_box_tiao_list = cor_vari_lags_with_rgdp_box_tiao,
      names_rgdp_maxlag = vec_names_cor_with_rgdp,
      free_names_rgdp_somelags = free_vars_upto_thislags,
      n_comb_all = n_combinations_all_variables,
      n_comb_maxlag = n_combinations_maxlag,
      n_combs_somelags = n_combs_list,
      set_of_vars_by_lag = set_variables_some_lags
    )
  )
}



var_over_several_periods <- function(list_of_dates, varnames, VAR_data,
                                     thislag) {
  all_result <- list()

  for (i in 1:length(list_of_dates)) {
    loop_dates <- list_of_dates[[i]]

    tra_start <- loop_dates[["tra_s"]]
    tra_end <- loop_dates[["tra_e"]]
    tes_start <- loop_dates[["tes_s"]]
    tes_end <- loop_dates[["tes_e"]]

    result <- one_var(
      varnames = varnames, VAR_data = VAR_data,
      thislag = thislag,
      training_set_start_date = tra_start,
      training_set_end_date = tra_end,
      test_set_start_date = tes_start,
      test_set_end_date = tes_end,
      use_dates = TRUE
    )

    all_result[[i]] <- result
  }

  all_accu <- reduce(map(all_result, "accu_rgdp"), rbind)
  all_accu_w <- reduce(map(all_result, "accu_rgdp"), cbind)

  all_accu <- as_tibble(all_accu)
  names(all_accu) <- c("RMSE", "MAE", "Theil")


  periods_average_accu <- colMeans(all_accu)


  all_accu <- all_accu %>%
    mutate(period = 1:n())

  all_accu_w <- as_tibble(all_accu_w)


  return(list(
    accu_per_period = all_accu,
    accu_per_period_row = all_accu_w,
    average_accu = periods_average_accu,
    all_results = all_result
  ))
}




get_VARs_evaluation <- function(set_of_free_variables, target_variables,
                                a_priori_variables, VAR_data, varlag = 4,
                                f_horizons = c(3, 7), dates_list,
                                return_forecast_object = TRUE) {
  len_lags_vector <- length(varlag)

  freevars_as_matrix <- set_of_free_variables[[1]]
  number_of_sets <- ncol(freevars_as_matrix)

  all_lags_and_names_results <- vector(
    mode = "list",
    length = len_lags_vector * number_of_sets
  )

  j_VARs_results <- vector(mode = "list", length = number_of_sets)

  ind <- 0

  for (j in 1:length(varlag)) {
    loop_lag <- varlag[j]

    for (i in 1:number_of_sets) {
      ind <- ind + 1

      free_vars <- freevars_as_matrix[, i]

      # print(paste("target: ", target_variables))
      # print(paste("a_priori: ", a_priori_variables))
      # print(paste("free_vars: ", free_vars))

      temp_names <- c(target_variables, a_priori_variables, free_vars)

      var_results <- var_over_several_periods(
        list_of_dates = dates_list, varnames = temp_names,
        VAR_data = VAR_data, thislag = loop_lag
      )


      j_VARs_results[[i]] <- var_results
      all_lags_and_names_results[[ind]] <- var_results
    }
  }

  return(all_lags_and_names_results)
}


rank_models <- function(results_list, best_n = 5, results_id = NULL) {
  average_accu_across_periods <- map(results_list, "average_accu")
  per_period_accu <- map(results_list, "accu_per_period")

  results_all_mods_all_peri <- map(results_list, c("all_results"))

  results_all_mods_peri_1 <- map(results_all_mods_all_peri, 1)
  fobj_all_mods_peri_1 <- map(results_all_mods_all_peri, 1, "forecast_obj")
  methods_all_mods_peri_1 <- map(fobj_all_mods_peri_1, c("forecast_obj", "method"))
  methods_rgdp_all_mods_peri_1 <- map(methods_all_mods_peri_1, "rgdp")

  fcast_all_mods_peri_1 <- map(fobj_all_mods_peri_1, c("forecast_obj", "forecast"))
  endo_variables_peri_1 <- map(fcast_all_mods_peri_1, names)

  per_period_accu_tbl <- per_period_accu[[1]]

  per_period_accu_tbl <- per_period_accu_tbl %>%
    gather(key = ferror, value = value, -period) %>%
    mutate(error_period = paste(ferror, period, sep = "_"))

  ferror_periods_vector <- per_period_accu_tbl %>%
    dplyr::select(value)

  ferror_periods_vector <- as_vector(ferror_periods_vector)

  names(ferror_periods_vector) <- per_period_accu_tbl$error_period


  form_vec_ferror <- function(period_acu) {
    per_period_accu_tbl <- period_acu
    # per_period_accu_tbl
    per_period_accu_tbl <- per_period_accu_tbl %>%
      gather(key = ferror, value = value, -period) %>%
      mutate(error_period = paste(ferror, period, sep = "_"))

    ferror_periods_vector <- per_period_accu_tbl %>%
      dplyr::select(value)
    ferror_periods_vector <- as_vector(ferror_periods_vector)
    names(ferror_periods_vector) <- per_period_accu_tbl$error_period
    ferror_periods_vector
  }

  errors_all_periods_models <- map(per_period_accu, form_vec_ferror)

  per_period_errors_tbl <- as_tibble(
    reduce(errors_all_periods_models, rbind)
  ) %>%
    mutate(model_index = 1:n())

  models_table <- as_tibble(reduce(average_accu_across_periods, rbind))

  models_table <- models_table %>%
    mutate(model_index = 1:n())

  models_table <- left_join(models_table,
    per_period_errors_tbl,
    by = "model_index"
  )

  models_table <- models_table %>%
    mutate(
      method = methods_rgdp_all_mods_peri_1,
      varnames = endo_variables_peri_1
    ) %>%
    arrange(RMSE) %>%
    mutate(rmse_ranking = 1:n()) %>%
    arrange(MAE) %>%
    mutate(mae_ranking = 1:n()) %>%
    arrange(Theil) %>%
    mutate(theil_ranking = 1:n()) %>%
    mutate(times_being_top_n = if_else(mae_ranking <= best_n, 1, 0) +
      if_else(rmse_ranking <= best_n, 1, 0) +
      if_else(theil_ranking <= best_n, 1, 0))

  if (!is.null(results_id)) {
    models_table$tibble_id <- results_id
  }


  return(models_table)
}



get_best_models_tbl <- function(results_list, n_best_per_error,
                                results_id = NULL) {
  models_err_rankings <- rank_models(
    results_list = results_list,
    best_n = n_best_per_error,
    results_id = results_id
  )

  if (!is.null(results_id)) {
    models_err_rankings$tibble_id <- results_id
  }

  models_top_n <- models_err_rankings %>%
    filter(times_being_top_n >= 1)

  if (!is.null(results_id)) {
    models_top_n$tibble_id <- results_id
  }

  res_top_n_by_any <- results_list[as_vector(models_top_n$model_index)]

  results_top_n_mods_all_peri <- map(res_top_n_by_any, c("all_results"))

  results_top_n_mods_peri_1 <- map(results_top_n_mods_all_peri, 1)

  fobj_top_n_mods_peri_1 <- map(results_top_n_mods_peri_1, "forecast_obj")

  methods_top_n_mods_peri_1 <- map(fobj_top_n_mods_peri_1, c("method"))

  methods_rgdp_top_n_mods_peri_1 <- map(methods_top_n_mods_peri_1, "rgdp")

  fcast_top_n_mods_peri_1 <- map(fobj_top_n_mods_peri_1, c("forecast"))

  endo_variables_peri_1 <- map(fcast_top_n_mods_peri_1, names)

  nlags_top_n_mods_peri_1 <- map(fobj_top_n_mods_peri_1, c("model", "p"))

  inputs_for_var_top_n <- models_top_n %>%
    dplyr::select(model_index, RMSE, MAE, Theil) %>%
    mutate(
      variables = endo_variables_peri_1,
      lags = nlags_top_n_mods_peri_1
    )

  if (!is.null(results_id)) {
    inputs_for_var_top_n$tibble_id <- results_id
  }

  return(list(
    top_n = models_top_n,
    inputs_for_var_top_n = inputs_for_var_top_n,
    all_models_err_rankings = models_err_rankings,
    results_top_n = res_top_n_by_any
  ))
}



reduce_top_n <- function(top_n_tbl, input_n, best_n = 3, podium_n = 1,
                         model_index_is_unique = FALSE) {
  best_reduced <- top_n_tbl %>%
    filter(!is.na(RMSE)) %>%
    filter(!is.na(MAE)) %>%
    filter(!is.na(Theil)) %>%
    arrange(RMSE) %>%
    mutate(rmse_ranking = 1:n()) %>%
    arrange(MAE) %>%
    mutate(mae_ranking = 1:n()) %>%
    arrange(Theil) %>%
    mutate(theil_ranking = 1:n()) %>%
    mutate(times_being_top_n = if_else(mae_ranking <= best_n, 1, 0) +
      if_else(rmse_ranking <= best_n, 1, 0) +
      if_else(theil_ranking <= best_n, 1, 0)) %>%
    filter(times_being_top_n >= podium_n) %>%
    filter(Theil < 1)

  if (model_index_is_unique) {
    inputs_reduced <- best_reduced %>%
      dplyr::select(model_index) %>%
      left_join(input_n, by = "model_index")
  } else {
    inputs_reduced <- best_reduced %>%
      dplyr::select(unique_model_index) %>%
      left_join(input_n, by = "unique_model_index")
  }
}


make_d_rgdp_plots <- function(VAR_data, var_and_fc) {
  indiv_var_fc <- var_and_fc[["var_fc_indiv"]]

  ave_fc <- var_and_fc[["fcs_ave"]]

  ave_fc_tbl <- map(ave_fc$fc_7_rgdp_mean, tk_tbl)
  ave_fc_tbl <- reduce(ave_fc_tbl, left_join, by = "index")
  names(ave_fc_tbl) <- c("index", ave_fc[["tibble_id"]])


  data_in_diff_tbl <- tk_tbl(data = VAR_data)
  names(data_in_diff_tbl) <- c("index", colnames(data_in_diff))

  data_and_fc <- full_join(data_in_diff_tbl, ave_fc_tbl, by = "index")


  basic_drgdp_plot <- ggplot(
    data = data_and_fc,
    aes(x = index, y = rgdp)
  ) +
    geom_line() + scale_x_yearqtr()


  plot_ave_of_ave <- basic_drgdp_plot +
    geom_line(aes(y = ave_r_m_t_1), col = "blue") +
    geom_line(aes(y = ave_r_m_t_3), col = "green") +
    geom_line(aes(y = ave_r_m_t_5), col = "red")


  plot_with_top1 <- basic_drgdp_plot +
    geom_line(aes(y = ave_rmse_1), col = "green") +
    geom_line(aes(y = ave_mae_1), col = "red") +
    geom_line(aes(y = ave_theil_1), col = "yellow") +
    geom_line(aes(y = ave_r_m_t_1), col = "blue")


  plot_with_top3 <- basic_drgdp_plot +
    geom_line(aes(y = ave_rmse_3), col = "green") +
    geom_line(aes(y = ave_mae_3), col = "red") +
    geom_line(aes(y = ave_theil_3), col = "yellow") +
    geom_line(aes(y = ave_r_m_t_3), col = "blue")


  plot_with_top5 <- basic_drgdp_plot +
    geom_line(aes(y = ave_rmse_5), col = "green") +
    geom_line(aes(y = ave_mae_5), col = "red") +
    geom_line(aes(y = ave_theil_5), col = "yellow") +
    geom_line(aes(y = ave_r_m_t_5), col = "blue")

  return(list(
    basic = basic_drgdp_plot,
    ave_of_ave = plot_ave_of_ave,
    top_1 = plot_with_top1,
    top_3 = plot_with_top3,
    top_5 = plot_with_top5
  ))
}