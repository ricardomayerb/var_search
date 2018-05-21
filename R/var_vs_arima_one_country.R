library(tidyverse)
source('./R/utils_vars.R')

country_name <- "Uruguay"
short_name <- substr(country_name, start = 1, stop = 3)

models_accu_path <- paste0("./data/", short_name, "_by_step_12345.rds")
cv_obj_path <- paste0("./data/", short_name, "_by_step_12345_cv_objects.rds")
from_sarima_path <- paste0("./data/sarimax_objects_", country_name, ".rds")

models_and_accu <- readRDS(models_accu_path)
cv_objects <- readRDS(cv_obj_path)
from_sarima <- readRDS(from_sarima_path)

models_and_accu <- models_and_accu %>% 
  filter(yoy_ranking <= 100) %>% 
  mutate(lags = unlist(lags)) %>% 
  mutate(arima_order = NA, arima_seasonal = NA) %>% 
  mutate(model_type = "VAR")

cv_objects <- cv_objects %>% 
  mutate(accu_yoy = unlist(accu_yoy)) %>% 
  arrange(accu_yoy) %>% 
  mutate(yoy_ranking = 1:n()) %>% 
  filter(yoy_ranking <= 100)

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

each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, yoy_rmse_1, yoy_rmse_2, yoy_rmse_3, yoy_rmse_4, 
                yoy_rmse_5, yoy_rmse_6, model_type, arima_order, arima_seasonal) %>% 
  rename(variables = variable, rmse_1 = yoy_rmse_1, rmse_2 = yoy_rmse_2, rmse_3 = yoy_rmse_3, rmse_4 = yoy_rmse_4, 
         rmse_5 = yoy_rmse_5, rmse_6 = yoy_rmse_6, lags = lag)

each_h_model_and_ave_rmse_r <- rbind(each_h_just_model_and_ave_rmse_var, 
                                     each_h_just_model_and_ave_rmse_sarimax)

models_rmse_every_h <- each_h_model_and_ave_rmse_r %>% 
  gather(key = "rmse_horizon", value = "rmse_value", starts_with("rmse"))

h1_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_1") %>% 
  arrange(rmse_value)

h2_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_1") %>% 
  arrange(rmse_value)

h3_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_1") %>% 
  arrange(rmse_value)

h4_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_1") %>% 
  arrange(rmse_value)

h5_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_1") %>% 
  arrange(rmse_value)

h6_model_and_ave_rmse_r <- models_rmse_every_h %>% 
  filter(rmse_horizon == "rmse_1") %>% 
  arrange(rmse_value)

