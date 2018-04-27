library(xts)
library(timetk)
library(readxl)
library(forecast)
library(tibbletime)
library(tidyverse)

un_yoy <- function(init_lev, vec_yoy) {
  
  n_init <- length(init_lev)
  n_yoy <- length(vec_yoy)
  y_vec <- vector(mode = "numeric", length = n_init + n_yoy)
  y_vec[1:n_init] <- init_lev
  
  for (i in seq_along(vec_yoy)) {

    this_y <- vec_yoy[i] + y_vec[ i ]
    
    y_vec[n_init + i] <- this_y
  }
  
  return(y_vec[(n_init+1) : (n_init+n_yoy)])
}

cv_obs_fc_back_from_diff <- function(yoy_ts, diff_ts, training_length,
                                     n_cv, h_max, cv_fcs_one_model,
                                     level_ts){
  
  cv_marks <-  make_test_dates_list(ts_data = diff_ts, n = n_cv,
                                    h_max = h_max, 
                                    training_length = training_length)
  
  cv_yq <- cv_marks[["list_of_year_quarter"]]
  training_end_yq <- map(cv_yq, "tra_e")
  test_start_yq <- map(cv_yq, "tes_s")
  test_end_yq <- map(cv_yq, "tes_e")

  cv_last_tra_obs_yoy <- list_along(training_end_yq)
  
  cv_test_set_obs_yoy <- list_along(training_end_yq)
  cv_fcs_yoy <- list_along(training_end_yq)
  cv_errors_yoy <- list_along(training_end_yq)
  
  cv_test_set_obs_level <- list_along(training_end_yq)
  cv_fcs_level <- list_along(training_end_yq)
  cv_errors_level <- list_along(training_end_yq)
  
  
  for (i in seq_along(test_end_yq)) {
    this_training_end_yq <- training_end_yq[[i]]
    this_test_end_yq <- test_end_yq[[i]]
    
    this_test_start_yq <- test_start_yq[[i]]
    
    this_training_end_y <- this_training_end_yq[1]
    this_training_end_q <- this_training_end_yq[2]
    
    this_year_before_train_end <- c(this_training_end_y-1, this_training_end_q)
    
    this_diff_fc <- cv_fcs_one_model[[i]]
    
    this_last_yoy_tra <- window(yoy_ts, start = this_training_end_yq,
                                end = this_training_end_yq)
    
    this_last_yoy_tra_rgdp <- this_last_yoy_tra[, "rgdp"]
    
    this_test_yoy <- window(yoy_ts, start = this_test_start_yq,
                            end = this_test_end_yq)
    this_test_yoy_rgdp <- this_test_yoy[, "rgdp"]
    
    this_last_year_of_train_level <- window(level_ts, 
                                      start = this_year_before_train_end,
                                      end = this_training_end_yq)
    
    this_last_year_of_train_level_rgdp <- this_last_year_of_train_level[, "rgdp"]
 
    this_diff_fc_rgdp <- this_diff_fc
    
    this_yoy_fc_rgdp <-  this_last_yoy_tra_rgdp[1] + cumsum(this_diff_fc_rgdp)

    this_yoy_error <- this_test_yoy_rgdp - this_yoy_fc_rgdp

    this_level_fc_rgdp <- un_yoy(init_lev = this_last_year_of_train_level_rgdp,
                                 vec_yoy = this_yoy_fc_rgdp)  
    
    this_test_level <- window(level_ts, start = this_test_start_yq,
                            end = this_test_end_yq)
    this_test_level_rgdp <- this_test_level[, "rgdp"]

    this_level_error <- this_test_level_rgdp - this_level_fc_rgdp 

    cv_last_tra_obs_yoy[[i]] <- this_last_yoy_tra_rgdp
    
    cv_test_set_obs_yoy[[i]] <- this_test_yoy_rgdp
    cv_errors_yoy[[i]] <- this_yoy_error
    cv_fcs_yoy[[i]] <- this_yoy_fc_rgdp

    cv_test_set_obs_level[[i]] <- this_test_level_rgdp
    cv_errors_level[[i]] <- this_level_error
    cv_fcs_level[[i]] <- this_level_fc_rgdp
    
  }
  
  return(list(test_obs_yoy = cv_test_set_obs_yoy,
              fcs_yoy = cv_fcs_yoy,
              fcs_errors_yoy = cv_errors_yoy,
              test_obs_level = cv_test_set_obs_level,
              fcs_level = cv_fcs_level,
              fcs_errors_level = cv_errors_level))
  
}


fcs_accu <- function(fc_mat, test_data_mat) {
  
  errors_mat <- test_data_mat - fc_mat
  rmse_vec <- sqrt(colMeans(errors_mat^2))
  mean_rmse <- mean(rmse_vec)
  return(mean_rmse)
}

from_diff_to_yoy_accu <- function(yoy_ts, diff_ts, level_ts, training_length,
                                  n_cv, h_max, cv_fcs_one_model) {
  
  undiff_stuff <- cv_obs_fc_back_from_diff(yoy_ts = yoy_ts, diff_ts = diff_ts,
                                           level_ts = level_ts,
                                           training_length = training_length,
                                           n_cv = n_cv, h_max = h_max,
                                           cv_fcs_one_model = cv_fcs_one_model)
  
  cv_test_sets_yoy <- undiff_stuff$test_obs_yoy
  cv_fc_yoy <- undiff_stuff$fcs_yoy
  
  cv_test_sets_yoy_mat <- reduce(cv_test_sets_yoy, rbind)
  cv_fcs_yoy_mat <- reduce(cv_fc_yoy, rbind)
  
  accu_yoy <- fcs_accu(fc_mat = cv_fcs_yoy_mat, test_data_mat = cv_test_sets_yoy_mat) 
  
  return(accu_yoy)
  
}

from_diff_to_lev_accu <- function(yoy_ts, diff_ts, level_ts, training_length,
                                  n_cv, h_max, cv_fcs_one_model) {
  
  undiff_stuff <- cv_obs_fc_back_from_diff(yoy_ts = yoy_ts, diff_ts = diff_ts,
                                           level_ts = level_ts,
                                           training_length = training_length,
                                           n_cv = n_cv, h_max = h_max,
                                           cv_fcs_one_model = cv_fcs_one_model)
  
  cv_test_sets_lev <- undiff_stuff$test_obs_level
  cv_fc_lev <- undiff_stuff$fcs_level
  
  cv_test_sets_lev_mat <- reduce(cv_test_sets_lev, rbind)
  cv_fcs_lev_mat <- reduce(cv_fc_lev, rbind)
  
  accu_lev <- fcs_accu(fc_mat = cv_fcs_lev_mat, test_data_mat = cv_test_sets_lev_mat) 
  
  return(accu_lev)
  
}


make_test_dates_list <- function(ts_data, type = "tscv", n = 8, h_max = 6,
                                 timetk_idx = TRUE, training_length = 20,
                                 external_idx = NULL) {
  
  data_length <- nrow(ts_data)
  
  if (timetk_idx) {
    date_time_index <- tk_index(ts_data, timetk_idx = timetk_idx)
  } else {
    date_time_index <- external_idx
  }

  list_of_positions <- list_along(seq(1:n))
  list_of_dates <- list_along(seq(1:n))
  list_of_year_quarter <- list_along(seq(1:n))
  
  if (type == "tscv") {
    
    for (i in seq.int(1:n)) {

      from_the_right <-  i - 1
      
      end_test_pos <- data_length - from_the_right 
      start_test_pos <- end_test_pos - h_max + 1
      end_training_pos <- start_test_pos - 1
      start_training_pos <- end_training_pos - training_length + 1
      

      end_test_date <- date_time_index[end_test_pos]
      start_test_date <- date_time_index[start_test_pos] 
      end_training_date <- date_time_index[end_training_pos]
      start_training_date <- date_time_index[start_training_pos]
      
      end_test_year <- year(end_test_date)
      start_test_year <- year(start_test_date) 
      end_training_year <- year(end_training_date) 
      start_training_year <- year(start_training_date)
      
      end_test_quarter <- quarter(end_test_date)
      start_test_quarter <- quarter(start_test_date) 
      end_training_quarter <- quarter(end_training_date) 
      start_training_quarter <- quarter(start_training_date)
      
      this_pos <- list(
        tra_s = start_training_pos, 
        tra_e = end_training_pos,
        tes_s = start_test_pos, 
        tes_e = end_test_pos)
      
      this_date <- list(
        tra_s = start_training_date, 
        tra_e = end_training_date,
        tes_s = start_test_date, 
        tes_e = end_test_date)
      
      this_yq <- list(
        tra_s = c(start_training_year, start_training_quarter),
        tra_e = c(end_training_year, end_training_quarter),
        tes_s = c(start_test_year, start_test_quarter),
        tes_e = c(end_test_year, end_test_quarter)
        )
      
      list_of_positions[[i]] <- this_pos
      list_of_dates[[i]] <- this_date
      list_of_year_quarter[[i]] <- this_yq
      
    }
    
    return(list(
      list_of_year_quarter = list_of_year_quarter,
      list_of_dates = list_of_dates,
      list_of_positions = list_of_positions)
    )
    
  }
  
  
}

get_gdp_start_end <- function(data) {
  
  na_omitted_rgdp  <- data %>% dplyr::select(date, rgdp) %>% 
    filter(!is.na(rgdp)) %>% 
    summarise(start_rgdp_date = min(date), end_rgdp_date = max(date))
  
  rgdp_start <-  na_omitted_rgdp[["start_rgdp_date"]]
  rgdp_end <-  na_omitted_rgdp[["end_rgdp_date"]]
  
  return(c(start = rgdp_start, end = rgdp_end))
  
}

read_gather_qm_data <- function(data_path = "./data/pre_r_data/", 
                                country = NULL) {
  
  file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
  file_names 
  
  file_paths <- paste0(data_path, file_names)
  file_paths
  
  country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")
  
  names(file_paths) <- country_names
  names(file_names) <- country_names
  
  if (!is.null(country)) {
    file_paths <- file_paths[country]
    file_names <- file_names[country]
    country_names <- country
  }
  

  all_files_q <- list_along(country_names)
  all_files_m <- list_along(country_names)
  all_files_m_q <- list_along(country_names)
  countries_merged_q_m <- list_along(country_names)
  
  
  
  for (i in seq_along(country_names)) {

    this_q <- read_excel(file_paths[i], sheet = "quarterly")
    this_q <- as_tbl_time(this_q, index = date)
    this_q <- dplyr::select(this_q, -c(year, hlookup))
    
    if(country_names[i] == "Uruguay") {
      this_q[, "rm"] <- - this_q[, "rm"]
    }
      
      
    all_files_q[[i]] <- this_q
    
    this_m <- read_excel(file_paths[i], sheet = "monthly")
    this_m <- as_tbl_time(this_m, index = date)
    all_files_m[[i]] <- this_m
    
    this_m_q <- this_m  %>%
      collapse_by(period = "quarterly") %>%
      group_by(date) %>% transmute_all(mean) %>%
      distinct(date, .keep_all = TRUE) %>% 
      ungroup() 
    
    all_files_m_q[[i]] <- this_m_q

    countries_merged_q_m[[i]] <- left_join(this_q, this_m_q, by = "date")
    
  }
  
  names(all_files_q) <- country_names
  names(all_files_m) <- country_names
  names(all_files_m_q) <- country_names
  names(countries_merged_q_m) <- country_names
  
  return(list(countries_q = all_files_q, 
              countries_m = all_files_m, 
              countries_q_former_m = all_files_m_q,
              countries_merged_q_m = countries_merged_q_m))
  
}


get_gdp_shaped_data <- function(data_path, country = NULL, 
                                only_complete_cases = FALSE,
                                list_variables_to_drop = NULL,
                                apply_log = FALSE
                                ) {
  
  suppressMessages(data_q_m_qm <- read_gather_qm_data(
    data_path = data_path, country = country)
  )
  
  
  data_qm <- data_q_m_qm[["countries_merged_q_m"]]
  
  country_names <- names(data_qm)
  
  rgdp_dates <- map(data_qm, get_gdp_start_end)
  
  data_qm_xts <- list_along(country_names)
  
  for (i in seq_along(country_names)) {
    
    this_qm_xts <- tk_xts(data_qm[[i]], date_var = date, silent = TRUE)
    # print(this_qm_xts)
    # print(glimpse(this_qm_xts))
    
    this_dates <- rgdp_dates[[i]]
    
    this_start <- this_dates[[1]]
    this_end <- this_dates[[2]]
    
    this_qm_xts <- chop_start_end_xts(this_qm_xts, this_start, this_end)
    
    data_qm_xts[[i]] <- this_qm_xts
    
  }
  
  names(data_qm_xts) <- country_names
  
  if (!is.null(list_variables_to_drop)) {
    data_qm_xts <- map2(data_qm_xts, list_variables_to_drop, drop_this_vars)
  }
  
  if (only_complete_cases) {
    # print("balanced data of all variables")
    data_qm_xts <- map(data_qm_xts, ~ .x[complete.cases(.x) , ])
  }

  if (apply_log) {
    data_qm_xts <- map(data_qm_xts, log)
  }
  
  return(data_qm_xts)
  
}


make_yoy_xts <- function(df_xts) {
  new_xts <- diff.xts(df_xts, lag = 4, na.pad = FALSE)/lag.xts(
    df_xts, na.pad = FALSE, k = 4)
}

make_yoy_ts <- function(df_ts) {
  new_ts <- diff.ts(df_ts, lag = 4)/lag.ts(df_xts, k = 4)
}

drop_this_vars <- function(df, vars_to_drop) {
  new_df <- df[,!(names(df) %in% vars_to_drop)]
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

chop_start_end_xts <- function(df_xts, start_date, end_date) {
  
  start_str <- start_date
  end_str <- end_date
  
  subset_string <- paste0(start_str, "/", end_str)
  
  new_xts <- df_xts[subset_string]
  
  return(new_xts)
}


