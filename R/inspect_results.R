source('~/var_search/R/utils_vars.R')
library(tidyverse)

# load("./data/arg_200k_estimation.RData")
models_and_accu <- readRDS("./data/arg_ma_long.rds")


tables_and_plots <- make_tables_and_plots_vbls_lags_sizes(models_and_accu)

print(tables_and_plots$variables_table)

print(tables_and_plots$lags_table)

print(tables_and_plots$sizes_table)

print(tables_and_plots$vbls_plot_facet)

print(tables_and_plots$vbls_plot_stacked)

print(tables_and_plots$sizes_plot_stacked)

print(tables_and_plots$lags_plot_stacked)

print(tables_and_plots$accu_best_diffs_and_their_yoy)

print(tables_and_plots$accu_best_diffs_and_their_level)


