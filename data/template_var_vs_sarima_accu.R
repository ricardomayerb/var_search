library(tidyverse)

models_and_accu <- readRDS("./data/Peru_by_step_12345.rds")
cv_objects <- readRDS("./data/Peru_by_step_12345_cv_objects.rds")
from_sarima <- readRDS(file = "./data/sarimax_objects_Peru.rds")



get_rmse_var_table_at_each_h <- function(data = cv_objects){
  cv_errors <- cv_objects[["cv_errors"]]
  
  all_rmses <- map(cv_errors, function(x) sqrt(colMeans( (reduce(x, rbind))^2))  )
  all_rmses_tbl <- reduce(all_rmses, rbind)
  rmse_names <- paste0("rmse_", 1:6)
  colnames(all_rmses_tbl) <- rmse_names
  row.names(all_rmses_tbl) <- NULL
  rmse_each_h <- cbind(cv_objects, all_rmses_tbl)
  rmse_each_h$cv_errors <- NULL
  return(rmse_each_h)
  
}

get_rmse_var_table_at_each_h(data = cv_objects)




# unlist(cv_errors)

# how do i unlist the cv_erros to h = 1, h =2 ...... h = 6 but keep them attached to the variables id and lags
# so: variables, lags, h =1, h = 2, h = 3, h = 4, h = 5, h = 6

# fcs_accu <- function(fc_mat, test_data_mat) {
#   
#   errors_mat <- test_data_mat - fc_mat
#   rmse_vec <- sqrt(colMeans(errors_mat^2))
#   mean_rmse <- mean(rmse_vec)
#   return(mean_rmse)
# }


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

