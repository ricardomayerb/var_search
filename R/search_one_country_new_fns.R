source('./R/utils_vars.R')
source('./R/var_functions.R')

data_path <- "./data/pre_r_data/"

file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
file_paths <- paste0(data_path, file_names)
country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")

general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                                    "month", "conf_emp", "conf_ibre", "ip_ine", 
                                    "vta_auto", "exist"))
# to make the data work we have to delete "m2" for argentina, "imp_int", "imp_k" for Ecuador and 
# "imp_consumer", "imp_intermediate", "imp_capital" for Mexico
extra_vars_to_drop <- list(Argentina = c("m2", "", ""), Bolivia = c("igae", "", ""), Brasil = c("", "", ""), 
                           Chile = c("", "", ""), Colombia = c("", "", ""), Ecuador = c("imp_int", "imp_k", ""), 
                           Mexico = c("imp_consumer", "imp_intermediate", "imp_capital"), Paraguay = c("", "", ""), 
                           Peru = c("", "", ""), Uruguay = c("cred", "", ""))

variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)

data_qm_xts_log <- get_gdp_shaped_data(data_path = data_path, 
                                       list_variables_to_drop = variables_to_drop,
                                       only_complete_cases = TRUE,
                                       apply_log = TRUE)

data_qm_xts_log_yoy <- map(data_qm_xts_log, make_yoy_xts)
data_qm_mts_log_yoy <- map(data_qm_xts_log_yoy, to_ts_q)

data_qm_xts_log_yoy_diff <- map(data_qm_xts_log_yoy, diff.xts, na.pad = FALSE)
data_qm_mts_log_yoy_diff <- map(data_qm_xts_log_yoy_diff, to_ts_q)



# OK countries: bol, bra, chl, col, par, per, ury
# Singular CCM problems: arg, ecu, mex

this_country_name <- "Uruguay"  
this_country <- this_country_name
data_ts <- data_qm_mts_log_yoy[[this_country]]
data_in_diff <- data_qm_mts_log_yoy_diff[[this_country]]

variable_names <- colnames(data_ts)
ncolumns <- ncol(data_ts)

this_bt <- 1
vec_max_lags <- c(2, 3, 4, 5)
vec_n_varsize <- c(3, 4)
n_best <- 5

target_rgdp <- c("rgdp")
# list_a_priori_groups <- list("rpc")
vec_a_priori_variables <- c("rpc")

timetk::has_timetk_idx(data_in_diff)

dates_list <- make_test_dates_list(ts_data = data_in_diff, type = "tscv", n = 8,
                                   h_max = 2, training_length = 32,
                                   timetk_idx = TRUE) 

# preview the number of models to evaluate given current choices
# print(paste("Running time: approx.", round(total_vars_to_estimate/1000, 1), "minutes."))

# this_var_names <- c("rgdp", "rpc", "manuf")

this_tra_s <- dates_list[[1]]$tra_s
this_tra_e <- dates_list[[1]]$tra_e

this_tes_s <- dates_list[[1]]$tes_s
this_tes_e <- dates_list[[1]]$tes_e

mock_all_vars <- c("rgdp", "rpc", "tot", "imp", "exp", "ip", "m1")

train_span <- 20
number_of_cv <- 8
fc_horizon <- 6

var_res <- try_sizes_vbls_lags(vec_size = vec_n_varsize, 
                               vec_lags = vec_max_lags,
                               var_data = data_in_diff, 
                              target_v = target_rgdp,
                              pre_selected_v = vec_a_priori_variables, 
                              is_cv = TRUE,
                              training_length = train_span,
                              h_max = fc_horizon, n_cv = number_of_cv)

cv_marks <-  make_test_dates_list(ts_data = data_in_diff, n = number_of_cv,
                                  h_max = fc_horizon, training_length = train_span)

cv_dates <- cv_marks[["list_of_dates"]]
cv_pos <- cv_marks[["list_of_positions"]]
cv_yq <- cv_marks[["list_of_year_quarter"]]

training_end_pos <- map(cv_pos, "tra_e")
training_end_dates <- map(cv_dates, "tra_e")
training_end_yq <- map(cv_yq, "tra_e")

get_last_training_obs <- function(this_data_ts, list_yq) {
  
  cv_last_tra_obs <- list_along(list_yq)
  
  for (i in seq_along(list_yq)) {
    this_yq <- list_yq[[i]]
    
    this_obs <- window(this_data_ts, start = this_yq, end = this_yq)
    cv_last_tra_obs[[i]] <- this_obs[, "rgdp"]
  }
  return(cv_last_tra_obs)
} 

cv_rgdp_lev <- get_last_training_obs(data_ts, training_end_yq)


# 
# back_from_diff_in_cv <- function(lev_ts, diff_ts, n){
#   
# }


# foo <- var_res %>% 


