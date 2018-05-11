total_non_gdp_vars <- 40
pre_chosen <- 1
universe_of_free_variables <- total_non_gdp_vars - pre_chosen 

varsize <- 5
slots_to_fill <- varsize - pre_chosen - 1

# n_comb <- choose(universe_of_free_variables, slots_to_fill)
# print(n_comb)

lag_choices <- 3
n_of_cv <- 8

# total_est_and_fcs <- n_comb  * lag_choices * n_of_cv
# print(total_est_and_fcs)
# 
# time_factor <- 80/200000
# 
# running_time_in_minutes <- total_est_and_fcs * time_factor
# print(running_time_in_minutes)


how_many_minutes <- function(total_non_gdp, pre_chosen, varsize, lag_choices, n_of_cv,
                             time_factor = (80/200000)) {
  
  total_non_gdp_vars <- total_non_gdp
  pre_chosen <- pre_chosen
  universe_of_free_variables <- total_non_gdp_vars - pre_chosen 
  
  varsize <- varsize
  slots_to_fill <- varsize - pre_chosen - 1
  
  n_comb <- choose(universe_of_free_variables, slots_to_fill)
  print(n_comb)
  
  lag_choices <- lag_choices
  n_of_cv <- n_of_cv
  
  total_est_and_fcs <- n_comb  * lag_choices * n_of_cv
  print(total_est_and_fcs)
  
  time_factor <- time_factor
  
  running_time_in_minutes <- total_est_and_fcs * time_factor
  print(running_time_in_minutes)
  
}


chosen2 <- how_many_minutes(total_non_gdp = 40, pre_chosen=2, varsize=5,  lag_choices = 3, 8)
chosen1 <- how_many_minutes(total_non_gdp = 40, pre_chosen=0, varsize=4,  lag_choices = 2, 8)
chosen0 <- how_many_minutes(total_non_gdp = 40, pre_chosen=0, varsize=4,  lag_choices = 3, 8)

bart1 <- how_many_minutes(total_non_gdp = 31, pre_chosen=0, varsize=5,  3, 8)


# one possible strategy

# all VARs size 2, 0.64 minutes
all_s_2 <- how_many_minutes(total_non_gdp = 40, pre_chosen=0, varsize=2,  lag_choices = 5, 8)

# plus all VARs size 3, 9.984 minutes
all_s_3 <- how_many_minutes(total_non_gdp = 40, pre_chosen=0, varsize=3,  lag_choices = 4, 8)

# plus all VARs size 4, 3 choices of lag ,94.848 minutes
all_s_3 <- how_many_minutes(total_non_gdp = 40, pre_chosen = 0, varsize=4,  lag_choices = 3, 8)

# or all VARs size 4, 2 choices of lag ,94.848, 63.232 minutes
all_s_3 <- how_many_minutes(total_non_gdp = 40, pre_chosen = 0, varsize=4,  lag_choices = 2, 8)

# or 1 pre_chosen, size 4, 3 choices of lag , 7.1136 minutes
all_s_3 <- how_many_minutes(total_non_gdp = 40, pre_chosen = 1, varsize=4,  lag_choices = 3, 8)

# plus 1 pre_chosen, size 5, 2 choices of lag , 58.4896 minutes
all_s_3 <- how_many_minutes(total_non_gdp = 40, pre_chosen = 4, varsize=5,  lag_choices = 3, 8)




