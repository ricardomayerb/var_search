source('./R/var_functions.R')
source('./R/utils_vars.R')
source('./R/utils.R')

country_name <- "Peru"

#################################### Load Data #######################################

data_path <- "./data/pre_r_data/"

file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
file_paths <- paste0(data_path, file_names)
country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")

general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                                    "month", "conf_emp", "conf_ibre", "ip_ine", 
                                    "vta_auto", "exist"))
# to make the data work we have to delete "m2" for argentina, "imp_int", "imp_k" for Ecuador and 
# "imp_consumer", "imp_intermediate", "imp_capital" for Mexico
extra_vars_to_drop <- list(Argentina = c("m2", "ri", "", "", "", "", "", "", "", "", ""), 
                           Bolivia = c("igae", "", "", "", "", "", "", "", "", "", "", ""), 
                           Brasil = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                           Chile = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                           Colombia = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                           Ecuador = c("imp_int", "imp_k", "", "", "", "", "", "", "", "", "", ""), 
                           Mexico = c("imp_consumer", "imp_intermediate", "imp_capital", "", "", "", "", "", "", "", "", ""), 
                           Paraguay = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                           Peru = c("expec_demand", "", "", "", "", "", "", "", "", "", "", ""),
                           Uruguay = c("cred", "", "", "", "", "", "", "", "", "", "", ""))

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

level_data_ts <- data_qm_mts_log[[country_name]]
yoy_data_ts <- data_qm_mts_log_yoy[[country_name]]
diff_yoy_data_ts <- data_qm_mts_log_yoy_diff[[country_name]]

loglev_rgdp_ts <- level_data_ts[ , "rgdp"]


################################### Load RDS objects ##################################
short_name <- substr(country_name, start = 1, stop = 3)

models_accu_path <- paste0("./data/", short_name, "_by_step_12345.rds")
cv_obj_path <- paste0("./data/", short_name, "_by_step_12345_cv_objects.rds")
from_sarima_path <- paste0("./data/sarimax_objects_", country_name, ".rds")

models_and_accu <- readRDS(models_accu_path)
cv_objects <- readRDS(cv_obj_path)
from_sarima <- readRDS(from_sarima_path)

######################################################################################

models_and_accu <- models_and_accu %>% 
  filter(yoy_ranking <= 50) %>% 
  mutate(lags = unlist(lags)) %>% 
  mutate(arima_order = NA, arima_seasonal = NA) %>% 
  mutate(model_type = "VAR")

fc_cv_loglevel_2_ext_cv_level <- function(loglev_rgdp, 
                                          one_cv_loglev_test,
                                          one_cv_loglev_fc) {
  
  test_start_log <- stats::start(one_cv_loglev_test)
  
  quasi_pre_test_log <- window(loglev_rgdp, end = test_start_log)
  pre_test_log <- subset(quasi_pre_test_log, 
                         end = length(quasi_pre_test_log) - 1)
  
  extended_test_log <- ts(data = c(pre_test_log, one_cv_loglev_test),
                          start = stats::start(pre_test_log), 
                          frequency = 4)
  
  extended_fc_log <- ts(data = c(pre_test_log, one_cv_loglev_fc),
                        start = stats::start(pre_test_log), 
                        frequency = 4)
  
  extended_test_level <- exp(extended_test_log)
  extended_fc_level <- exp(extended_fc_log)
  
  return(list(test_data = extended_test_level, fc = extended_fc_level))
}



cv_objects <- cv_objects %>% 
  mutate(accu_yoy = unlist(accu_yoy)) %>% 
  arrange(accu_yoy) %>% 
  mutate(yoy_ranking = 1:n()) %>% 
  filter(yoy_ranking <= 50) %>% 
  mutate(proper_level_test_data = map(cv_test_data_lev, ~ map(., exp)),
         proper_level_fcs = map(cv_fcs_lev, ~ map(., exp)),
         extended_test_lev = map2(cv_test_data_lev, cv_fcs_lev,
                                  ~ map2(
                                    .x,
                                    .y,
                                    ~ fc_cv_loglevel_2_ext_cv_level(
                                      loglev_rgdp_ts,
                                      .x, 
                                      .y)[["test_data"]]
                                    )
                                  ),
         extended_cv_fcs_lev = map2(cv_test_data_lev, cv_fcs_lev,
                                  ~ map2(
                                    .x,
                                    .y,
                                    ~ fc_cv_loglevel_2_ext_cv_level(
                                      loglev_rgdp_ts,
                                      .x, 
                                      .y)[["fc"]]
                                      )
                                  ),
         cv_errors_proper_levels = map2(extended_test_lev, extended_cv_fcs_lev,
                                        ~ map2(.x, .y, ~ .x - .y)),
         extended_test_yoy_growth = map(extended_test_lev, 
                                 ~ map(., ~ diff(., lag = 4)/lag.xts(., k = 4)
                                       )),
         extended_cv_fcs_yoy_growth = map(extended_cv_fcs_lev, 
                                 ~ map(., ~ diff(., lag = 4)/lag.xts(., k = 4)
                                 )),
         extended_cv_error_yoy = map2(extended_test_yoy_growth, 
                                      extended_cv_fcs_yoy_growth,
                                      ~ map2(.x, .y, ~ .x - .y))
  )

cv_objects <- add_column_cv_yoy_errors(cv_objects)

with_rmses <- get_rmse_var_table_at_each_h_diff_yoy(data = cv_objects) %>% 
  mutate(model_type = "VAR")

rmse_yoy_sarimax <- from_sarima$compare_rmse_yoy %>% 
  mutate(model_type = "arima")


extended_x_data_ts <- from_sarima$extended_x_data_ts
extended_x_data_ts <- window(extended_x_data_ts, 
                             start = stats::start(level_data_ts))
future_x_data_ts <- subset(extended_x_data_ts, start = (1 + nrow(level_data_ts)))




v_lags_order_season <- from_sarima$var_lag_order_season 
rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
  left_join(v_lags_order_season, by = c("variable", "lag"))


rmse_yoy_sarimax$ave_rmse_h1h6 <- rowMeans(cbind(rmse_yoy_sarimax$yoy_rmse_1, 
                                                 rmse_yoy_sarimax$yoy_rmse_2, 
                                                 rmse_yoy_sarimax$yoy_rmse_3, 
                                                 rmse_yoy_sarimax$yoy_rmse_4, 
                                                 rmse_yoy_sarimax$yoy_rmse_5, 
                                                 rmse_yoy_sarimax$yoy_rmse_6)) 




####### AVERAGE RMSE OVER H = 1, 2, 3, 4, 5 AND 6
just_model_and_ave_rmse_1 <- models_and_accu %>% 
  dplyr::select(variables, lags, accu_yoy, arima_order, arima_seasonal) 


just_model_and_ave_rmse_2 <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, ave_rmse_h1h6, arima_order, arima_seasonal) %>% 
  rename(variables = variable, accu_yoy = ave_rmse_h1h6, lags = lag) 

just_model_and_ave_rmse_2_stata <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, ave_rmse_h1h6) %>% 
  rename(variables = variable, accu_yoy = ave_rmse_h1h6, lags = lag)

model_and_ave_rmse_r <- rbind(just_model_and_ave_rmse_1, 
                              just_model_and_ave_rmse_2) %>% 
  arrange(accu_yoy)

########## RMSE at each H
each_h_just_model_and_ave_rmse_var <- with_rmses %>% 
  mutate(lags = unlist(lags)) %>% 
  mutate(arima_order = NA, arima_seasonal = NA) %>% 
  select(-yoy_ranking)


h_max = 6

each_h_just_model_and_ave_rmse_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  mutate(estimated_obj = map2(variables, lags,  ~ vars::VAR(y = diff_yoy_data_ts[, .x], p = .y)),
         fc_obj = map(estimated_obj, forecast, h = h_max),
         rgdp_mean_fc = map(fc_obj, c("forecast", "rgdp", "mean")),
         var_roots = map(estimated_obj, vars::roots),
         all_stable = map(var_roots, ~ all(.x < 1))
  )

each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, yoy_rmse_1, yoy_rmse_2, yoy_rmse_3, yoy_rmse_4, 
                yoy_rmse_5, yoy_rmse_6, model_type, arima_order, arima_seasonal) %>% 
  rename(variables = variable, rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, 
         rmse_5 = yoy_rmse_5, rmse_6 = yoy_rmse_6, lags = lag)


each_h_just_model_and_ave_rmse_sarimax_0 <- as_tibble(each_h_just_model_and_ave_rmse_sarimax) %>% 
  filter(variables != "rgdp") %>% 
  filter(variables != "expec_demand") %>%
  filter(lags == 0) %>%   
  mutate(estimated_obj = pmap(list(variables, lags, arima_order, arima_seasonal), 
                                 ~ my_arimax(y_ts = level_data_ts[, "rgdp"], 
                                             y_order = ..3, 
                                             y_seasonal = ..4, 
                                             xreg_ts = level_data_ts[, ..1],
                                             vec_of_names = ..1) ),
         estimated_obj = map(estimated_obj, 1) ,
         fc_obj = map2(estimated_obj, variables, 
                   ~ forecast(.x, 
                              h = h_max, 
                              xreg = future_x_data_ts[1:h_max, .y])
                   )
         )
         
arimax_fc <- function(arima_obj, xvariables, lags, extended_x, h) {
  maxlag <- max(lags)
  this_xreg <- extended_x[, xvariables]
  xreg_wlags <- reduce(map(seq(0, maxlag), 
                           ~ lag.xts(this_xreg, k = .)) , ts.union)
  
  future_xreg_wlag <- subset(xreg_wlags, 
                             start = (1 + nrow(level_data_ts)),
                             end = (h + nrow(level_data_ts))
                             ) 
  
  fc <- forecast(arima_obj, h = h, xreg = future_xreg_wlag)
  
  return(fc)
}

each_h_just_model_and_ave_rmse_sarimax_12 <- as_tibble(each_h_just_model_and_ave_rmse_sarimax) %>% 
  filter(variables != "rgdp") %>% 
  filter(variables != "expec_demand") %>%
  filter(lags != 0) %>%   
  mutate(estimated_obj = pmap(list(variables, lags, arima_order, arima_seasonal), 
                                 ~ my_arimax(y_ts = level_data_ts[, "rgdp"], 
                                             y_order = ..3, 
                                             y_seasonal = ..4, 
                                             xreg_ts = level_data_ts[, ..1],
                                             xreg_lags = 0:..2,
                                             vec_of_names = ..1)[[1]] ),
         fc_obj = pmap( list(estimated_obj, variables, lags), 
                   ~ arimax_fc(arima_obj = ..1,  xvariables = ..2,
                   lags = ..3, extended_x = extended_x_data_ts, h = h_max)
                   )
  )
                   
                   



each_h_model_and_ave_rmse_sarimax_012 <- rbind(
  each_h_just_model_and_ave_rmse_sarimax_0,
  each_h_just_model_and_ave_rmse_sarimax_12) %>% 
  mutate(rgdp_mean_fc = map(fc_obj, c("mean")))



each_h_model_and_ave_rmse_r <- rbind(
  each_h_just_model_and_ave_rmse_var %>%  select(-c(var_roots, all_stable)),
  each_h_model_and_ave_rmse_sarimax_012)



models_rmse_every_h_arima <- as_tibble(each_h_model_and_ave_rmse_r) %>% 
  gather(key = "rmse_horizon", value = "rmse_value", starts_with("rmse")) %>% 
  filter(model_type == "arima")

models_rmse_every_h <- as_tibble(each_h_model_and_ave_rmse_r) %>% 
  gather(key = "rmse_horizon", value = "rmse_value", starts_with("rmse"))



h1_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_1") %>% 
  arrange(rmse_value)

h2_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_2") %>% 
  arrange(rmse_value)

h3_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_3") %>% 
  arrange(rmse_value)

h4_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_4") %>% 
  arrange(rmse_value)

h5_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_5") %>% 
  arrange(rmse_value)

h6_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_6") %>% 
  arrange(rmse_value)





# 
# moo <- cv_objects$cv_test_data_lev[[1]]
# 
# moo
# goo <- moo[[1]]
# goo
# 
# goostart <- stats::start(goo)
# goostart
# 
# doo <- window(level_data_ts[, "rgdp"], end = goostart)
# doo
# 
# roo <- subset(doo, end = length(doo)-1)
# roo
# 
# extended_test <- ts(data = c(roo, goo), start = stats::start(roo), frequency = 4)
# extended_test
# 
# woo <- cv_objects$cv_fcs_lev[[1]][[1]]
# woo
# 
# extended_fc <- ts(data = c(roo, woo), start = stats::start(roo), frequency = 4)
# extended_fc
# 
# foo <- fc_cv_loglevel_2_ext_cv_level(level_data_ts[, "rgdp"], goo, woo)
# foo_fc <- foo[["fc"]]         
# foo_test <- foo[["test_data"]]
# foo_fc
# foo_test
# 
# yoy_extended_test <- diff(extended_test, lag = 4)
# yoy_extended_fc <- diff(extended_fc, lag = 4)
# 
