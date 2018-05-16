library(tidyverse)

models_and_accu <- readRDS("./data/Bol_by_step_12345.rds")
from_sarima <- readRDS(file = "./data/sarimax_objects_Bolivia.rds")
rmse_yoy_sarimax <- from_sarima$compare_rmse_yoy
rmse_level_sarimax <- from_sarima$compare_rmse

rmse_yoy_sarimax$ave_rmse_h1h8 <- rowMeans(cbind(rmse_yoy_sarimax$yoy_rmse_1, 
                                           rmse_yoy_sarimax$yoy_rmse_2, 
                                           rmse_yoy_sarimax$yoy_rmse_3, 
                                           rmse_yoy_sarimax$yoy_rmse_4, 
                                           rmse_yoy_sarimax$yoy_rmse_5, 
                                           rmse_yoy_sarimax$yoy_rmse_6, 
                                           rmse_yoy_sarimax$yoy_rmse_7, 
                                           rmse_yoy_sarimax$yoy_rmse_8)) 

rmse_level_sarimax$ave_rmse_h1h8 <- rowMeans(cbind(rmse_level_sarimax$level_rmse_1, 
                                                 rmse_level_sarimax$level_rmse_2, 
                                                 rmse_level_sarimax$level_rmse_3, 
                                                 rmse_level_sarimax$level_rmse_4, 
                                                 rmse_level_sarimax$level_rmse_5, 
                                                 rmse_level_sarimax$level_rmse_6, 
                                                 rmse_level_sarimax$level_rmse_7, 
                                                 rmse_level_sarimax$level_rmse_8)) 

rmse_level_sarimax$ave_rmse_h1h8_stata <- rowMeans(cbind(rmse_level_sarimax$rmse1, 
                                                   rmse_level_sarimax$rmse2, 
                                                   rmse_level_sarimax$rmse3, 
                                                   rmse_level_sarimax$rmse4, 
                                                   rmse_level_sarimax$rmse5, 
                                                   rmse_level_sarimax$rmse6, 
                                                   rmse_level_sarimax$rmse7, 
                                                   rmse_level_sarimax$rmse8)) 

level_accu_r_vs_stata <- rmse_level_sarimax %>% 
  dplyr::select(variable, lag,  ave_rmse_h1h8, ave_rmse_h1h8_stata)


just_model_and_ave_rmse_1 <- models_and_accu %>% 
  dplyr::select(variables, lags, accu_yoy) %>% 
  mutate(lags = unlist(lags)) 

just_model_and_ave_rmse_2 <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, ave_rmse_h1h8) %>% 
  rename(variables = variable, accu_yoy = ave_rmse_h1h8, lags = lag)

just_model_and_ave_rmse_2_stata <- rmse_yoy_sarimax %>% 
  dplyr::select(variable, lag, ave_rmse_h1h8) %>% 
  rename(variables = variable, accu_yoy = ave_rmse_h1h8, lags = lag)

model_and_ave_rmse_r <- rbind(just_model_and_ave_rmse_1, 
                              just_model_and_ave_rmse_2) %>% 
  arrange(accu_yoy)

