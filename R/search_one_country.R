source('./R/one_var.R')

all_countries_tbl <- readRDS("data/all_countries_tidy_data_current.rds")


country_name <- "Argentina"

this_country <- country_name

country_tbl <- all_countries_tbl[all_countries_tbl$id == this_country, ]

country_yoy_xts <- country_tbl[["yoy_q_all_xts"]][[1]]
country_yoy_mts <- country_tbl[["yoy_q_all_mts"]][[1]]

country_yoy_fd_xts <- country_tbl[["yoy_firstdiff_q_all_xts"]][[1]]
country_yoy_fd_mts <- country_tbl[["yoy_firstdiff_q_all_mts"]][[1]]

data_ts <- country_yoy_mts
data_in_diff <- country_yoy_fd_mts

# scaled_data_ts <- scale(data_ts)
# scaled_data_in_diff <- scale(data_in_diff)
# data_in_diff <- scaled_data_in_diff


variable_names <- colnames(data_ts)
ncolumns <- ncol(data_ts)


vec_max_lags <- c(2, 3, 4, 5)
vec_n_varsize <- c(3, 4, 5, 6)
n_best <- 5

target_rgdp <- c("rgdp")
list_a_priori_groups <- list("rpc")

# Periods covered Pseudo Out of Sample Exercise:
# 1) 2013q2 – 2014q3, 2) 2013q4 – 2015q1, 3) 2014q4 – 2016q1, 4) 2015q4 – 2016q4 f bvo890


## train-test period 1
train_start_1 <- c(2004, 2)
train_end_1 <- c(2013, 1)
test_start_1 <- c(2013, 2)
test_end_1 <- c(2014, 3)

## train-test period 2
train_start_2 <- c(2004, 2)
train_end_2 <- c(2013, 3)
test_start_2 <- c(2013, 4)
test_end_2 <- c(2015, 1)

## train-test period 3
train_start_3 <- c(2004, 2)
train_end_3 <- c(2014, 3)
test_start_3 <- c(2014, 4)
test_end_3 <- c(2016, 1)

## train-test period 4
train_start_4 <- c(2004, 2)
train_end_4 <- c(2015, 3)
test_start_4 <- c(2015, 4)
test_end_4 <- c(2016, 4)



stata_dates_1 <- list(
  tra_s = train_start_1, tra_e = train_end_1,
  tes_s = test_start_1, tes_e = test_end_1)

stata_dates_2 <- list(
  tra_s = train_start_2, tra_e = train_end_2,
  tes_s = test_start_2, tes_e = test_end_2)

stata_dates_3 <- list(
  tra_s = train_start_3, tra_e = train_end_3,
  tes_s = test_start_3, tes_e = test_end_3)

stata_dates_4 <- list(
  tra_s = train_start_4, tra_e = train_end_4,
  tes_s = test_start_4, tes_e = test_end_4)

stata_dates <- list(stata_dates_1, stata_dates_2, stata_dates_3, stata_dates_4)

dates_list <- stata_dates

this_bt <- 1.7



# preview the number of models to evaluate given current choices

preview_col_names <- c("v_size", "lags", "a_priori", "combn", "subperiods", "n_models")


n_of_max_lags <- length(vec_max_lags)
n_training_sets <- length(dates_list)
n_var_sizes <- length(vec_n_varsize)
n_a_priori_groups <- length(list_a_priori_groups)


n_variations <- n_var_sizes * n_of_max_lags * n_a_priori_groups

n_preview_cols <- length(preview_col_names)
n_preview_rows <- n_variations

preview_df <- data.frame(matrix(nrow = n_preview_rows, ncol = n_preview_cols))
names(preview_df) <- preview_col_names

n_of_estimated_vars_list <- list() 

k = 1
p = 1

for (i in seq_along(vec_n_varsize)) {
  this_var_size <- vec_n_varsize[[i]]
  
  for (j in seq_along(list_a_priori_groups)) {
    this_a_priori <- list_a_priori_groups[[j]]
    
    cmm_res <- get_ccm_variables(
      df = data_in_diff,
      intended_var_size = this_var_size,
      target_variables = target_rgdp,
      a_priori_variables = this_a_priori,
      maxlag = 12, box_tiao_factor = this_bt
    )
    
    set_of_free_variables <- cmm_res$set_of_vars_by_lag[2]
    n_comb_free_vars <- ncol(set_of_free_variables[[1]])
    n_estimated_vars <- n_comb_free_vars * n_of_max_lags * n_training_sets
    
    for (m in seq_along(vec_max_lags)) {
      this_lag <- vec_max_lags[[m]]
      
      this_row <- c(this_var_size, this_a_priori, this_lag, n_comb_free_vars,
                    n_training_sets, n_comb_free_vars * n_training_sets)
      
      preview_df[m, ] <- this_row
      
      m <- m + 1
    }
    
    
    
    # print(paste("number of combn of free variables = ", n_comb_free_vars))
    # print(paste("number of lag specifications = ", n_of_max_lags))
    # print(paste("number of training sets = ", n_training_sets))
    # print(paste(
    #   "number of VARs to estimate and evalaute = ",  n_estimated_vars ))
    
    n_of_estimated_vars_list[[k]] <- n_estimated_vars 
    
    k <- k + 1

  }
    
}


n_of_estimated_vars_vec <- unlist(n_of_estimated_vars_list)

total_vars_to_estimate <- sum(n_of_estimated_vars_vec)

print(paste("Total number of VARs to estimate and test:", 
            total_vars_to_estimate))

print(paste("Running time: approx.", total_vars_to_estimate/1000, "minutes."))


# best_indiv_list <- search_over_ap_tset_lags_size_comb(
#   ap_list = list_a_priori_groups, dates_list = stata_dates,
#   lags_vec = vec_max_lags, target = target_rgdp, VAR_data = data_in_diff,
#   this_id = "new_func", bt = this_bt, sizes_vec = vec_n_varsize, n_best = n_best
# )

