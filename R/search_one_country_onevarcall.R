source('./R/utils_vars.R')
source('./R/one_var.R')


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
data_qm_xts_log_yoy_diff <- map(data_qm_xts_log_yoy, diff.xts, na.pad = FALSE)
data_qm_mts_log_yoy_diff <- map(data_qm_xts_log_yoy_diff, to_ts_q)

data_qm_mts_log <- map(data_qm_xts_log_yoy, to_ts_q)

# OK countries: bol, bra, chl, col, par, per, ury
# Singular CCM problems: arg, ecu, mex

this_country_name <- "Uruguay"  
this_country <- this_country_name
data_ts <- data_qm_mts_log[[this_country]]
data_in_diff <- data_qm_mts_log_yoy_diff[[this_country]]

variable_names <- colnames(data_ts)
ncolumns <- ncol(data_ts)

this_bt <- 1
vec_max_lags <- c(2, 3, 4, 5)
vec_n_varsize <- c(4, 5)
n_best <- 5

target_rgdp <- c("rgdp")
list_a_priori_groups <- list("rpc")

dates_list <- make_test_dates_list(ts_data = data_in_diff, type = "tscv", n = 8, h_max = 6, training_length = 20,
                                   timetk_idx = TRUE) 

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

print(paste("Running time: approx.", round(total_vars_to_estimate/1000, 1), "minutes."))

this_var_names <- c("rgdp", "rpc", "manuf")

this_tra_s <- dates_list[[1]]$tra_s
this_tra_e <- dates_list[[1]]$tra_e

this_tes_s <- dates_list[[1]]$tes_s
this_tes_e <- dates_list[[1]]$tes_e



res_one_var <- one_var(varnames = this_var_names, VAR_data = data_in_diff, 
                       thislag = 2, training_set_start_date = this_tra_s,
                       training_set_end_date = this_tra_e,
                       test_set_start_date = this_tes_s, 
                       test_set_end_date = this_tes_e,
                       use_dates = TRUE
                      )




# 
# (varnames, VAR_data, thislag = 3, f_horizons = c(3, 7),
#   start_training_set_index = NULL, last_training_index = NULL,
#   test_set_nobs = NULL, training_set_start_date = NULL,
#   training_set_end_date = NULL, test_set_start_date = NULL,
#   test_set_end_date = NULL, use_dates = FALSE,
#   return_forecast_object = TRUE) 


best_indiv_list <- search_over_ap_tset_lags_size_comb(
  ap_list = list_a_priori_groups, dates_list = dates_list,
  lags_vec = vec_max_lags, target = target_rgdp, VAR_data = data_in_diff,
  this_id = "new_func", bt = this_bt, sizes_vec = vec_n_varsize, n_best = n_best
)

