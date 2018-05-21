# take models_rmse_every_h and 
# 
# 0. load data files into the script (level and diff?yoy for westimation and possible yoy for plots)
# 
# 1.  add a column with mutate and map2 or map, call it estimated_VAR, and use the command previously seen in an email to 
# estimate a VAR. Use ifelse to assing a NA is the model is not a VAR
# 
# 2.  add a column with mutate and pmap, call it estimated_arima. Use ifelse to assing a NA is the model is not an arima
# 
# 
# The next piece of code we should write is something that take models out of a table (both var or sarimax) 
# and do estimation-fc-and-cv, optionally taking as given a set of rmse. Think of the final table with both var 
# and sarima accu_yoy or the m_analysis excel sheet


# VARs_from_best_inputs <- rank_tibble %>%
#   mutate(
#     vfit = map2(
#       variables, lags,
#       function(x, y) vars::VAR(y = VAR_data[, x], p = y)
#     )
#     
# 

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

cv_objects <- cv_objects %>% 
  mutate(accu_yoy = unlist(accu_yoy)) %>% 
  arrange(accu_yoy) %>% 
  mutate(yoy_ranking = 1:n()) %>% 
  filter(yoy_ranking <= 50)

cv_objects <- add_column_cv_yoy_errors(cv_objects)

with_rmses <- get_rmse_var_table_at_each_h_diff_yoy(data = cv_objects) %>% 
  mutate(model_type = "VAR")

rmse_yoy_sarimax <- from_sarima$compare_rmse_yoy %>% 
  mutate(model_type = "arima")


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

each_h_just_model_and_ave_rmse_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
  mutate(estimated_VAR = map2(variables, lags,  ~ vars::VAR(y = diff_yoy_data_ts[, .x], p = .y)),
         fc_obj = map(estimated_VAR, forecast, h = 6),
         rgdp_mean_fc = map(fc_obj, c("forecast", "rgdp", "mean")),
         var_roots = map(estimated_VAR, vars::roots),
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
  mutate(estimated_arimax = pmap(list(variables, lags, arima_order, arima_seasonal), 
                                 ~ my_arimax(y_ts = level_data_ts[, "rgdp"], 
                                             y_order = ..3, 
                                             y_seasonal = ..4, 
                                             xreg_ts = level_data_ts[, ..1],
                                             vec_of_names = ..1) ))
         
each_h_just_model_and_ave_rmse_sarimax_12 <- as_tibble(each_h_just_model_and_ave_rmse_sarimax) %>% 
  filter(variables != "rgdp") %>% 
  filter(variables != "expec_demand") %>%
  filter(lags != 0) %>%   
  mutate(estimated_arimax = pmap(list(variables, lags, arima_order, arima_seasonal), 
                                 ~ my_arimax(y_ts = level_data_ts[, "rgdp"], 
                                             y_order = ..3, 
                                             y_seasonal = ..4, 
                                             xreg_ts = level_data_ts[, ..1],
                                             xreg_lags = 0:..2,
                                             vec_of_names = ..1)[[1]] )
  )  

each_h_model_and_ave_rmse_sarimax012 <- rbind(
  each_h_just_model_and_ave_rmse_sarimax_0,
  each_h_just_model_and_ave_rmse_sarimax_12) 

each_h_model_and_ave_rmse_sarimax <- each_h_model_and_ave_rmse_sarimax012 %>%  
  mutate(fc_obj = map(estimated_arimax[[1]], forecast, h = 6))

foo <- each_h_model_and_ave_rmse_sarimax012[1,]

foo_ea <- foo[["estimated_arimax"]][[1]]

foo_ea %>% forecast(h=6)

fc_obj = map(estimated_VAR, forecast, h = 6),
rgdp_mean_fc = map(fc_obj, c("forecast", "rgdp", "mean")),
var_roots = map(estimated_VAR, vars::roots),
all_stable = map(var_roots, ~ all(.x < 1))

# each_h_just_model_and_ave_rmse_sarimax <- as_tibble(each_h_just_model_and_ave_rmse_sarimax) %>% 
#   filter(variables != "rgdp") %>% 
#   filter(variables != "expec_demand") %>%
#   mutate(estimated_arimax = ifelse(lags == 0, 
#                                    pmap(list(variables, lags, arima_order, arima_seasonal), 
#                                  ~ my_arimax(y_ts = level_data_ts[, "rgdp"], 
#                                              y_order = ..3, 
#                                              y_seasonal = ..4, 
#                                              xreg_ts = level_data_ts[, ..1],
#                                              vec_of_names = ..1) ), 
#                                  pmap(list(variables, lags, arima_order, arima_seasonal), 
#                                       ~ my_arimax(y_ts = level_data_ts[, "rgdp"], 
#                                                   y_order = ..3, 
#                                                   y_seasonal = ..4, 
#                                                   xreg_ts = level_data_ts[, ..1],
#                                                   xreg_lags = 0:..2,
#                                                   vec_of_names = ..1) )
#   )  )

# all_arimax_0 <- my_arimax(y_ts = level_data_ts[, "rgdp"], xreg_ts = level_data_ts[ , ..1],  y_order = ..3, 
#                           y_seasonal = ..4, vec_of_names = ..1)
# 
# all_arimax_1 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
#                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
#                           xreg_lags = 0:1)
# 
# all_arimax_2 <- my_arimax(y_ts = rgdp_ts, xreg_ts = mdata_ext_ts,  y_order = rgdp_order, 
#                           y_seasonal = rgdp_seasonal, vec_of_names = monthly_names,
#                           xreg_lags = 0:2)












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
# 1.  add a column with mutate and map2 or map, call it estimated_VAR, and use the command previously seen in an email to 
# estimate a VAR. Use ifelse to assing a NA is the model is not a VAR




