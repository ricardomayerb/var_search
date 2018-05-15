library(xts)
library(timetk)
library(readxl)
library(forecast)
library(tibbletime)
library(tidyverse)
library(gridExtra)
library(grid)

un_yoy <- function(init_lev, vec_yoy) {
  
  n_init <- length(init_lev)
  n_yoy <- length(vec_yoy)
  y_vec <- vector(mode = "numeric", length = n_init + n_yoy)
  y_vec[1:n_init] <- init_lev
  
  for (i in seq_along(vec_yoy)) {

    this_y <- vec_yoy[i] + y_vec[ i ]
    
    y_vec[n_init + i] <- this_y
  }
  
  un_yoy_vec <- y_vec[(n_init + 1):(n_init + n_yoy)]
  
  return(un_yoy_vec)
}

un_yoy_ts <- function(init_lev, vec_yoy) {
  
  n_init <- length(init_lev)
  n_yoy <- length(vec_yoy)
  y_vec <- vector(mode = "numeric", length = n_init + n_yoy)
  y_vec[1:n_init] <- init_lev
  
  for (i in seq_along(vec_yoy)) {
    
    this_y <- vec_yoy[i] + y_vec[ i ]
    
    y_vec[n_init + i] <- this_y
  }
  
  un_yoy_vec <- y_vec[(n_init + 1):(n_init + n_yoy)]
  
  this_year <- as.integer(floor(time(vec_yoy)))
  
  # print("time(vec_yoy) in un_yoy_ts")
  # print(time(vec_yoy))
  
  
  # print("this_year in un_yoy_ts")
  # print(this_year)
  
  this_quarter <- as.integer(4 * (time(vec_yoy) - this_year + 0.25))

  # print("this_quarter in un_yoy_ts")
  # print(this_quarter)
  
  this_start <- c(first(this_year), first(this_quarter))
  # print("this_start in un_yoy_ts")
  # print(this_start)
  
  un_yoy_ts <- ts(un_yoy_vec, start = this_start, frequency = 4)
  return(un_yoy_ts)
}


un_diff_ts <- function(last_undiffed, diffed_ts) {
  undiffed <- as.numeric(last_undiffed) + cumsum(diffed_ts)

  this_year <- as.integer(floor(time(diffed_ts)))
  this_quarter <- as.integer(4 * (time(diffed_ts) - this_year + 0.25))
  undiffed_ts <- ts(undiffed, start = c(first(this_year), first(this_quarter)),
                    end = c(last(this_year), last(this_quarter)), frequency = 4)
  
  return(undiffed_ts)
}

un_diff <- function(last_undiffed, diffed_ts) {
  undiffed <- as.numeric(last_undiffed) + cumsum(diffed_ts)
  return(undiffed)
  
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
    
    decimal_q <- (this_training_end_q/4) - 0.25
    decimal_yq <- this_training_end_y + decimal_q
    new_decimal_yq <- decimal_yq - 0.75
    
    this_last_year_training_y <- floor(new_decimal_yq)
    this_last_year_training_q <- 4*(new_decimal_yq - floor(new_decimal_yq) + 0.25)
 
    this_year_before_train_end <- c(this_last_year_training_y, 
                                    this_last_year_training_q)
    
    # print("c(this_training_end_y, this_training_end_q)")
    # print(c(this_training_end_y, this_training_end_q))
    # 
    # print("decimal_q")
    # print(decimal_q)
    # 
    # print("decimal_yq")
    # print(decimal_yq)
    # 
    # print("this_last_year_training_y")
    # print(this_last_year_training_y)
    # 
    # print("this_last_year_training_q")
    # print(this_last_year_training_q)
    # 
    # 
    # 
    # print("this_year_before_train_end")
    # print(this_year_before_train_end)

    this_diff_fc <- cv_fcs_one_model[[i]]
    
    this_last_yoy_tra <- window(yoy_ts, start = this_training_end_yq,
                                end = this_training_end_yq)
    
    this_last_yoy_tra_rgdp <- this_last_yoy_tra[, "rgdp"]
    
    this_test_yoy <- window(yoy_ts, start = this_test_start_yq,
                            end = this_test_end_yq)
    this_test_yoy_rgdp <- this_test_yoy[, "rgdp"]
    

    
    # print("this_training_end_yq")
    # print(this_training_end_yq)
    
        
    this_last_year_of_train_level <- window(level_ts, 
                                      start = this_year_before_train_end,
                                      end = this_training_end_yq)
    
    this_last_year_of_train_level_rgdp <- this_last_year_of_train_level[, "rgdp"]

    # print("this_last_year_of_train_level_rgdp")
    # print(this_last_year_of_train_level_rgdp) 

    this_diff_fc_rgdp <- this_diff_fc
    
    # this_yoy_fc_rgdp <-  this_last_yoy_tra_rgdp[1] + cumsum(this_diff_fc_rgdp)
    this_yoy_fc_rgdp <-  un_diff(last_undiffed = this_last_yoy_tra_rgdp[1], 
                                     diffed_ts = this_diff_fc_rgdp)
    alt_ts_this_yoy_fc_rgdp <-  un_diff_ts(last_undiffed = this_last_yoy_tra_rgdp[1], 
                                     diffed_ts = this_diff_fc_rgdp)
    
    this_yoy_error <- this_test_yoy_rgdp - this_yoy_fc_rgdp
    alt_ts_this_yoy_error <- this_test_yoy_rgdp - alt_ts_this_yoy_fc_rgdp
    
    this_level_fc_rgdp <- un_yoy(init_lev = this_last_year_of_train_level_rgdp,
                                 vec_yoy = this_yoy_fc_rgdp)  
    
    # alt_ts_this_level_fc_rgdp <- un_yoy_ts(init_lev = this_last_year_of_train_level_rgdp,
    #                              vec_yoy = this_yoy_fc_rgdp)  
    # 
    # print("this_level_fc_rgdp")
    # print(this_level_fc_rgdp)
    # 
    # print("alt_ts_this_level_fc_rgdp")
    # print(alt_ts_this_level_fc_rgdp)
    
    # print(alt_ts_this_level_fc_rgdp)
    # print(alt_ts_this_yoy_fc_rgdp)
    
    this_test_level <- window(level_ts, start = this_test_start_yq,
                            end = this_test_end_yq)
    this_test_level_rgdp <- this_test_level[, "rgdp"]
    
    # print("this_test_level_rgdp")
    # print(this_test_level_rgdp)
    # 
    # print("level_ts[, rgdp]")
    # print(level_ts[, "rgdp"])
    

    this_level_error <- this_test_level_rgdp - this_level_fc_rgdp 
    # alt_ts_this_level_error <- this_test_level_rgdp - alt_ts_this_level_fc_rgdp 
    
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


make_tables_and_plots_vbls_lags_sizes <- function(models_and_accu) {
  
  models_and_accu <- models_and_accu %>% 
    mutate(accu_lev = map(accu_lev, unlist),
           accu_yoy = map(accu_yoy, unlist))
  
  models_and_accu <- models_and_accu %>%  
    mutate(n_variables = map(variables, length))
  
  ma_diff_yoy <- models_and_accu %>% 
    filter(diff_ranking <= 50)
  
  ma_yoy <- models_and_accu %>% 
    filter(yoy_ranking <= 50)
  
  ma_level <- models_and_accu %>% 
    filter(level_ranking <= 50)
  
  vbls_diff <- reduce(ma_diff_yoy$variables, c) 
  count_vbls_diff <- vbls_diff %>% tibble(v = .) %>% 
    group_by(v) %>% summarise(n_diff = n())
  
  vbls_yoy <- reduce(ma_yoy$variables, c)
  count_vbls_yoy <- vbls_yoy %>% tibble(v = .) %>% 
    group_by(v) %>% summarise(n_yoy = n())
  
  vbls_level <- reduce(ma_level$variables, c)
  count_vbls_level <- vbls_level %>% tibble(v = .) %>% 
    group_by(v) %>% summarise(n_level = n())
  
  n_endo_diff <- reduce(ma_diff_yoy$n_variables, c)
  count_n_endo_diff <- n_endo_diff %>% tibble(size = .) %>% 
    group_by(size) %>% summarise(size_diff = n())
  
  n_endo_yoy <- reduce(ma_yoy$n_variables, c)
  count_n_endo_yoy <- n_endo_yoy %>% tibble(size = .) %>% 
    group_by(size) %>% summarise(size_yoy = n())
  
  n_endo_level <- reduce(ma_level$n_variables, c)
  count_n_endo_level <- n_endo_level %>% tibble(size = .) %>% 
    group_by(size) %>% summarise(size_level = n())
  
  lags_diff <- reduce(ma_diff_yoy$lags, c)
  count_lags_diff <- lags_diff %>% tibble(lag = .) %>% 
    group_by(lag) %>% summarise(lag_diff = n())
  
  lags_yoy <- reduce(ma_yoy$lags, c)
  count_lags_yoy <- lags_yoy %>% tibble(lag = .) %>% 
    group_by(lag) %>% summarise(lag_yoy = n())
  
  lags_level <- reduce(ma_level$lags, c)
  count_lags_level <- lags_level %>% tibble(lag = .) %>% 
    group_by(lag) %>% summarise(lag_level = n())
  
  
  vbls <- reduce(list(count_vbls_diff, count_vbls_yoy, count_vbls_level), 
                 full_join, by = "v")  %>% 
    gather(key = "group", value = "n", -v) %>%
    mutate(group = factor(group, levels = c( "n_level" , "n_yoy", "n_diff"))) %>% 
    arrange(group, desc(n)) %>% 
    mutate(v_order = row_number())
  
  g_vbls_facets <- ggplot(vbls, aes(x = v_order, y = n, fill = group)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    facet_wrap(~ group, scales = "free") + 
    ylab("Number of appearances in selected VARs") +
    xlab("variable") + 
    scale_x_continuous(
      breaks = vbls$v_order,
      labels = vbls$v,
      expand = c(0,0)
    ) +
    coord_flip()
  
  g_vbls_stacked <- ggplot(data = vbls, aes(x = fct_reorder2(v, group, n), 
                                            weight = n, fill = group)) + 
    geom_bar()  + coord_flip()
  
  
  
  endo <- reduce(list(count_n_endo_diff, count_n_endo_yoy, count_n_endo_level), 
                 full_join, by = "size") %>% 
    gather(key = "group", value = "freq", -size) %>%
    mutate(group = factor(group, levels = c( "size_level" , "size_yoy", "size_diff"))) %>% 
    arrange(group, desc(freq)) %>% 
    mutate(size_order = row_number())
  
  g_endo_facets <- ggplot(endo, aes(x = size_order, y = freq, fill = group)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    facet_wrap(~ group) + 
    ylab("Number of appearances in selected VARs") +
    xlab("size") + 
    scale_x_continuous(
      breaks = endo$size_order,
      labels = endo$size
    ) +
    coord_flip()
  
  g_endo_stacked <- ggplot(data = endo, aes(x = size, weight = freq, fill = group)) + 
    geom_bar(position = "dodge") 
  
  
  n_lags <- reduce(list(count_lags_diff, count_lags_yoy, count_lags_level), 
                   full_join, by = "lag") %>% 
    gather(key = "group", value = "freq", -lag) %>%
    mutate(group = factor(group, levels = c( "lag_level" , "lag_yoy", "lag_diff"))) %>% 
    arrange(group, desc(freq)) %>% 
    mutate(lag_order = row_number())
  
  g_lag_facets <- ggplot(n_lags, aes(x = lag_order, y = freq, fill = group)) +
    geom_bar(stat = "identity", show.legend = FALSE) + 
    facet_wrap(~ group, scales = "free_y") + 
    ylab("Number of appearances in selected VARs") +
    xlab("lags") + 
    scale_x_continuous(
      breaks = n_lags$lag_order,
      labels = n_lags$lag
    ) +
    coord_flip()
  
  g_lag_stacked <- ggplot(data = n_lags, aes(x = lag, weight = freq, fill = group)) + 
    geom_bar(position = "dodge") 
  
  
  
  
  variables_table <- reduce(list(count_vbls_diff, count_vbls_yoy, count_vbls_level), 
                            full_join, by = "v") %>% 
    rename(variable = v,
           fcs_diff_yoy = n_diff,
           fcs_yoy = n_yoy,
           fcs_level = n_level) %>% 
    mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
           fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
           fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
    ) %>% 
    arrange(desc(fcs_diff_yoy))
  
  lags_table <- reduce(list(count_lags_diff, count_lags_yoy, count_lags_level), 
                       full_join, by = "lag") %>% 
    rename(fcs_diff_yoy = lag_diff,
           fcs_yoy = lag_yoy,
           fcs_level = lag_level) %>% 
    mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
           fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
           fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
    ) %>% 
    arrange(desc(fcs_diff_yoy))
  
  sizes_table <- reduce(list(count_n_endo_diff, count_n_endo_yoy, count_n_endo_level), 
                        full_join, by = "size") %>% 
    rename(fcs_diff_yoy = size_diff,
           fcs_yoy = size_yoy,
           fcs_level = size_level) %>% 
    mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
           fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
           fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
    ) %>% 
    arrange(desc(fcs_diff_yoy))
  
  
  
  g_diff_vs_yoy_best_diffs <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 50),
                                     aes(x = accu_diff_yoy, y = unlist(accu_yoy) )) + geom_point()
  
  g_diff_vs_lev_best_diffs <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 50),
                                     aes(x = accu_diff_yoy, y = unlist(accu_lev) )) + geom_point()
  
  g_diff_vs_yoy_best_yoys <- ggplot(data =  models_and_accu %>% filter(yoy_ranking <= 50),
                                    aes(x = unlist(accu_yoy), y = accu_diff_yoy)) + geom_point()
  
  g_lev_vs_yoy_best_lev <- ggplot(data =  models_and_accu %>% filter(level_ranking <= 50),
                                  aes(x = unlist(accu_lev), y = accu_diff_yoy)) + geom_point()
  
  g_best_diff_accu <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 50),
                             aes(x = accu_diff_yoy)) + geom_histogram(bins = 7)
  
  g_best_yoy_accu <- ggplot(data =  models_and_accu %>% filter(yoy_ranking <= 50),
                            aes(x = unlist(accu_yoy) )) + geom_histogram(bins = 7)
  
  g_best_lev_accu <- ggplot(data =  models_and_accu %>% filter(level_ranking <= 50),
                            aes(x = unlist(accu_lev) )) + geom_histogram(bins = 7)
  
  return(list(
    variables_table = variables_table, lags_table = lags_table, sizes_table = sizes_table,
    vbls_plot_facet = g_vbls_facets, lags_plot_facets = g_lag_facets, sizes_plot_facets = g_endo_facets, 
    vbls_plot_stacked = g_vbls_stacked, lags_plot_stacked = g_lag_stacked, sizes_plot_stacked = g_endo_stacked,
    accu_best_diffs_and_their_yoy = g_diff_vs_yoy_best_diffs, 
    accu_best_diffs_and_their_level = g_diff_vs_lev_best_diffs
  )
  )
  
}


