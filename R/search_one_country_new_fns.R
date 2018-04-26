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

data_qm_mts_log <- map(data_qm_xts_log, to_ts_q)

data_qm_xts_log_yoy <- map(data_qm_xts_log, make_yoy_xts)
data_qm_mts_log_yoy <- map(data_qm_xts_log_yoy, to_ts_q)

data_qm_xts_log_yoy_diff <- map(data_qm_xts_log_yoy, diff.xts, na.pad = FALSE)
data_qm_mts_log_yoy_diff <- map(data_qm_xts_log_yoy_diff, to_ts_q)

# OK countries: bol, bra, chl, col, par, per, ury
# Singular CCM problems: arg, ecu, mex

this_country_name <- "Uruguay"  
this_country <- this_country_name
original_data_ts <- data_qm_mts_log[[this_country]]
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

# timetk::has_timetk_idx(data_in_diff)
# 
# dates_list <- make_test_dates_list(ts_data = data_in_diff, type = "tscv", n = 8,
#                                    h_max = 2, training_length = 32,
#                                    timetk_idx = TRUE) 

# preview the number of models to evaluate given current choices
# print(paste("Running time: approx.", round(total_vars_to_estimate/1000, 1), "minutes."))

# this_var_names <- c("rgdp", "rpc", "manuf")

# this_tra_s <- dates_list[[1]]$tra_s
# this_tra_e <- dates_list[[1]]$tra_e
# 
# this_tes_s <- dates_list[[1]]$tes_s
# this_tes_e <- dates_list[[1]]$tes_e

mock_all_vars <- c("rgdp", "rpc", "tot", "imp", "exp", "ip", "m1")

train_span <- 30
number_of_cv <- 4
fc_horizon <- 3

var_res <- try_sizes_vbls_lags(vec_size = vec_n_varsize, 
                               vec_lags = vec_max_lags,
                               var_data = data_in_diff, 
                              target_v = target_rgdp,
                              pre_selected_v = vec_a_priori_variables, 
                              is_cv = TRUE,
                              training_length = train_span,
                              h_max = fc_horizon, n_cv = number_of_cv)

cv_fcs <- var_res$cv_fcs

# 
# x_prueba <- c(2,1,4,5,6,3,5)
# x_prueba_yoy <- diff(x_prueba, lag = 4)
# x_prueba
# x_prueba_yoy
# x_un_yoy <- un_yoy(x_prueba[1:4], x_prueba_yoy)
# x_un_yoy
# x_prueba

undiff_stuff <- cv_obs_fc_back_from_diff(yoy_ts = data_ts, 
                                         diff_ts = data_in_diff,
                                training_length = train_span,
                                n_cv = number_of_cv, h_max = fc_horizon,
                                cv_fcs_one_model = cv_fcs[[1]],
                                level_ts = original_data_ts)

# cv_errors_yoy <- undiff_stuff$fcs_errors_level
# cv_test_sets_yoy <- undiff_stuff$test_obs_level
# cv_fc_yoy <- undiff_stuff$fcs_level
# 
# foo <- from_diff_to_lev_accu(yoy_ts = data_ts, diff_ts = data_in_diff, 
#                              level_ts = original_data_ts, 
#                              training_length = train_span, n_cv = number_of_cv,
#                              h_max = fc_horizon, cv_fcs_one_model = cv_fcs[[1]] 
#                             )

to_compare <- var_res %>% 
  mutate(accu_yoy = map(cv_fcs, from_diff_to_yoy_accu,
                        yoy_ts = data_ts, diff_ts = data_in_diff, 
                        level_ts = original_data_ts, 
                        training_length = train_span, n_cv = number_of_cv,
                        h_max = fc_horizon),
         accu_lev = map(cv_fcs, from_diff_to_lev_accu,
                        yoy_ts = data_ts, diff_ts = data_in_diff, 
                        level_ts = original_data_ts, 
                        training_length = train_span, n_cv = number_of_cv,
                        h_max = fc_horizon)
         ) %>% 
  dplyr::select(-c(cv_errors, cv_test_data, cv_fcs, ranking)) %>% 
  arrange(mean_cv_rmse) %>% 
  mutate(diff_ranking = 1:n()) %>% 
  arrange(unlist(accu_yoy)) %>% 
  mutate(yoy_ranking = 1:n()) %>% 
  arrange(unlist(accu_lev)) %>% 
  mutate(level_ranking = 1:n()) 
  


# # how to use diffinv
# xvec <- c(2,3,4,1,5,7,7,3,2,2, 5, 1)
# xdiff <- diff(xvec)
# xvec
# xdiff
# diffinv(xdiff)
# # key step, it should be identical to xvec
# xvec[1] + diffinv(xdiff)
# 
# # with YoY quarterly data
# x_yoy <- diff(xvec, lag = 4)
# x_yoy
# diffinv(x_yoy, lag = 4)
# # key step, it should be identical to xvec
# xvec[1:4] + diffinv(x_yoy, lag = 4)
