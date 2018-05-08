library(tidyverse)
source('~/var_search/R/utils_vars.R')

moac_18 <- readRDS("./data/arg_ma_long_bt18.rds")
moac_17 <- readRDS("./data/arg_ma_long_bt17.rds")
moac_16 <- readRDS("./data/arg_ma_long_bt16.rds")
moac_15 <- readRDS("./data/arg_ma_long_bt15.rds")
moac_14 <- readRDS("./data/arg_ma_long.rds")


tables_and_plots_14 <- make_tables_and_plots_vbls_lags_sizes(moac_14)
tables_and_plots_15 <- make_tables_and_plots_vbls_lags_sizes(moac_15)
tables_and_plots_16 <- make_tables_and_plots_vbls_lags_sizes(moac_16)
tables_and_plots_17 <- make_tables_and_plots_vbls_lags_sizes(moac_17)
tables_and_plots_18 <- make_tables_and_plots_vbls_lags_sizes(moac_18)

variables_table_14 <- tables_and_plots_14$variables_table
variables_table_15 <- tables_and_plots_15$variables_table
variables_table_16 <- tables_and_plots_16$variables_table
variables_table_17 <- tables_and_plots_17$variables_table
variables_table_18 <- tables_and_plots_18$variables_table

variables_diff_yoy <- reduce(list(variables_table_14[ , c("variable", "fcs_diff_yoy")],
                                  variables_table_15[ , c("variable", "fcs_diff_yoy")],
                                  variables_table_16[ , c("variable", "fcs_diff_yoy")],
                                  variables_table_17[ , c("variable", "fcs_diff_yoy")],
                                  variables_table_18[ , c("variable", "fcs_diff_yoy")]),
                             full_join, by = "variable")
names(variables_diff_yoy) <- c("variable", "bt_14", "bt_15", "bt_16", "bt_17", "bt_18")


variables_yoy <- reduce(list(variables_table_14[ , c("variable", "fcs_yoy")],
                             variables_table_15[ , c("variable", "fcs_yoy")],
                             variables_table_16[ , c("variable", "fcs_yoy")],
                             variables_table_17[ , c("variable", "fcs_yoy")],
                             variables_table_18[ , c("variable", "fcs_yoy")]),
                        full_join, by = "variable")
names(variables_yoy) <- c("variable", "bt_14", "bt_15", "bt_16", "bt_17", "bt_18")


variables_level <- reduce(list(variables_table_14[ , c("variable", "fcs_level")],
                               variables_table_15[ , c("variable", "fcs_level")],
                               variables_table_16[ , c("variable", "fcs_level")],
                               variables_table_17[ , c("variable", "fcs_level")],
                               variables_table_18[ , c("variable", "fcs_level")]),
                        full_join, by = "variable")
names(variables_level) <- c("variable", "bt_14", "bt_15", "bt_16", "bt_17", "bt_18")


print(variables_diff_yoy)
print(variables_yoy)
print(variables_level)



# print(tables_and_plots$variables_table)
# 
# print(tables_and_plots$lags_table)
# 
# print(tables_and_plots$sizes_table)
# 
# print(tables_and_plots$vbls_plot_facet)
# 
# print(tables_and_plots$vbls_plot_stacked)
# 
# print(tables_and_plots$sizes_plot_stacked)
# 
# print(tables_and_plots$lags_plot_stacked)
# 
# print(tables_and_plots$accu_best_diffs_and_their_yoy)
# 
# print(tables_and_plots$accu_best_diffs_and_their_level)
# 
# 
# 
