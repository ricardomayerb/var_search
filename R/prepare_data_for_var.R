source('./R/utils.R')
library(xts)
library(tidyr)
library(dplyr)
library(timetk)
library(readxl)
library(lubridate)
library(purrr)
library(forecast)
library(ggplot2)
library(tibbletime)
library(purrr)
library(stringr)


# data_path <- "."
data_path <- "./data/pre_r_data/"

file_names <- list.files(path = data_path, recursive = T, pattern = '*.xlsx')
file_paths <- paste0(data_path, file_names)
country_names <- str_extract(file_names, "\\w+(?=\\.xlsx?)")
names(file_paths) <- country_names
names(file_names) <- country_names


general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                               "month", "conf_emp", "conf_ibre", "ip_ine", 
                               "vta_auto", "exist"))

extra_vars_to_drop <- list(Argentina = "", Bolivia = "igae", Brasil = "", 
                           Chile = "", Colombia = "", Ecuador = "", Mexico = "",
                           Paraguay = "", Peru = "", Uruguay = "cred")

variables_to_drop <- map2(extra_vars_to_drop, 
                          general_variables_to_drop, c)



tictoc::tic()
data_qm_xts <- get_gdp_shaped_data(data_path = data_path, 
                                    list_variables_to_drop = variables_to_drop,
                                    only_complete_cases = TRUE)

data_qm_xts_log <- get_gdp_shaped_data(data_path = data_path, 
                                    list_variables_to_drop = variables_to_drop,
                                    only_complete_cases = TRUE,
                                    apply_log = TRUE)
tictoc::toc()

foo <- data_qm_xts[[1]]

moo <- log(foo)

varhandle::inspect.na(moo)

# 
# uru <- data_qm_xts[["Uruguay"]]
# foo <- log(uru)
# moo <- colSums(is.na(foo))
# moo
# 
# data_q_m_qm <- read_gather_qm_data(data_path = data_path)
# 
# data_qm <- data_q_m_qm[["countries_merged_q_m"]]
# 
# 
# rgdp_dates <- map(data_qm, get_gdp_start_end)



# 
# chile_3_offending_variables <-  chile_tbl %>% 
#   filter(n_nas <= 3)
# 
# chile_2_offending_variables <-  chile_tbl %>% 
#   filter(n_nas <= 2)
# 
# chile_1_offending_variables <-  chile_tbl %>% 
#   filter(n_nas <= 1)
# 
# chile_0_offending_variables <-  chile_tbl %>% 
#   filter(n_nas <= 0)
# 
# chile_3_nas <- inspect.na(as.data.frame(chile_3_offending_variables), barplot = FALSE)
# chile_3_nas
# chile_2_nas <- inspect.na(as.data.frame(chile_2_offending_variables), barplot = FALSE)
# chile_2_nas
# chile_1_nas <- inspect.na(as.data.frame(chile_1_offending_variables), barplot = FALSE)
# chile_1_nas
# 
# nrow(chile_0_offending_variables)
# nrow(chile_1_offending_variables)
# nrow(chile_2_offending_variables)
# nrow(chile_3_offending_variables)
# 
# print(as.character(chile_1_nas$column_name))
# print(as.character(chile_2_nas$column_name))
# print(as.character(chile_3_nas$column_name))
# 
# title <- c("cols_with_nas",  "n_obs", "start", "end")
# info_0 <- paste0("if 0 vars remomve: starts at " , min(chile_0_offending_variables$index),
#           ", ends at ", max(chile_0_offending_variables$index), ". N obs: ", 
#           nrow(chile_0_offending_variables))
# info_0
# 
# rna <- coo2 %>% coredata %>% is.na %>% rowSums
# rna_xts <- xts(x = rna, order.by = index(coo2))
# rna_xts
# 
# 
# doo <- data_qm_xts[["Chile"]]
# doo2 <- data_qm_xts2[["Chile"]]
# 
# foo <- coredata(chile_data_qm_xts2[[1]])
# pin.na(foo)
# inspect.na(foo)
# 
# inspect.na(as.data.frame(chile_data_qm2))
# 
# foo[, "n_nas"] <- rowSums(is.na(foo))
# 
# com_chile <- chile_tbl[complete.cases(chile_tbl), ]
# com_chile_xts <- chile_xts[complete.cases(chile_xts), ]

# 
# 
>>>>>>> ea503eb419066c01d378fbc73b27713973257426
# 
# start_year_argentina <- "2006 Q4"
# start_year_bolivia <- "2005 Q3"
# start_year_brasil <- "2003 Q1"
# start_year_chile <- "2005 Q1"
# start_year_colombia <- "2003 Q1"
# start_year_ecuador <- "2008 Q4"
# start_year_mexico <- "2008 Q1"
# start_year_paraguay <- "2003 Q3"
# start_year_peru <- "2007 Q4"
# start_year_uruguay <- "2004 Q1"
# 
# end_year <- "2017 Q3"
# 
# start_years_vector <- c(start_year_argentina, start_year_bolivia, start_year_brasil, start_year_chile,
#                         start_year_colombia, start_year_ecuador, start_year_mexico, start_year_paraguay,
#                         start_year_peru, start_year_uruguay) 
# 
# years_tbl <- tibble(id = all_country_names, start_year = start_years_vector,
#                     end_year = end_year)
# 
# countries_tbl <- tibble(id = country_names, excel_q = all_files_q,
#                         excel_m = all_files_m) %>% 
#   mutate(excel_q = map(excel_q, drop_this_vars, variables_to_drop),
#          excel_m = map(excel_m, drop_this_vars, variables_to_drop),
#          excel_m = map2(excel_m, id, drop_this_vars_this_country,  "Bolivia",
#                         "igae"),
#          excel_m = map2(excel_m, id, drop_this_vars_this_country,  "Uruguay",
#                         "cred"),
#          excel_q = map(excel_q, mutate, date = as.yearqtr(date)),
#          excel_m = map(excel_m, mutate, date = as.yearqtr(date))
# ) %>% 
#   left_join(years_tbl, by = "id") %>% 
#   mutate(start_year = as.yearqtr(start_year),
#          end_year = as.yearqtr(end_year),
#          data_q_xts = map(excel_q, tk_xts, date_var = date, silent = TRUE),
#          data_m_xts = map(excel_m, tk_xts, date_var = date, silent = TRUE),
#          data_q_xts_r = pmap(list(data_q_xts, start_year, end_year),
#                              chop_start_end_xts),
#          data_m_xts_r = pmap(list(data_m_xts, start_year, end_year),
#                              chop_start_end_xts),
#          data_m_Q_xts = map(data_m_xts_r, apply.quarterly, mean),
#          yoy_q_xts = map(data_q_xts_r, make_yoy_xts),
#          yoy_m_Q_xts = map(data_m_Q_xts, make_yoy_xts),
#          yoy_firstdiff_q_xts = map(yoy_q_xts, diff.xts, na.pad = FALSE),
#          yoy_firstdiff_m_Q_xts = map(yoy_m_Q_xts, diff.xts, na.pad = FALSE),
#          yoy_q_all_xts = map2(yoy_q_xts, yoy_m_Q_xts, merge.xts),
#          yoy_firstdiff_q_all_xts = map2(yoy_firstdiff_q_xts, yoy_firstdiff_m_Q_xts,
#                                     merge.xts),
#          yoy_q_all_mts = map(yoy_q_all_xts, to_ts_q),
#          yoy_firstdiff_q_all_mts = map(yoy_firstdiff_q_all_xts, to_ts_q),
#          yoy_q_all_mts = map(yoy_q_all_xts, na.omit),
#          yoy_firstdiff_q_all_mts = map(yoy_firstdiff_q_all_mts, na.omit)
#          )
# 
# 
# date_and_time_execution <-  format(Sys.time(), "%Y_%m_%d_%H_%M_%S") 
# 
# destination_folder = "./data/"
# 
# big_tibble_filename <- paste0(destination_folder, "all_countries_tidy_data_",
#                               date_and_time_execution, ".rds")
# 
# current_tibble_filename <- paste0(destination_folder,
#                                   "all_countries_tidy_data_current.rds")
# 
# saveRDS(countries_tbl, file = big_tibble_filename)
# 
# saveRDS(countries_tbl, file = current_tibble_filename) 
# 
# 
# ### end of script 
# 
# 
# # # example of pulling out data from a specific country
# # this_country <- "Bolivia"
# # bolivia_tbl <- countries_tbl[countries_tbl$id == this_country, ]
# # 
# # 
# # bolivia_yoy_xts <- bolivia_tbl[["yoy_q_all_xts"]][[1]]
# # bolivia_yoy_mts <- bolivia_tbl[["yoy_q_all_mts"]][[1]]
# # 
# # bolivia_yoy_fd_xts <- bolivia_tbl[["yoy_firstdiff_q_all_xts"]][[1]]
# # bolivia_yoy_fd_mts <- bolivia_tbl[["yoy_firstdiff_q_all_mts"]][[1]]
# # 
# # bolivia_yoy_xts[,1]
# # bolivia_yoy_mts[,1]
# # 
# # bolivia_yoy_fd_xts[,1]
# # bolivia_yoy_fd_mts[,1]
# # 
# # this_country <- "Chile"
# # chile_tbl <- countries_tbl[countries_tbl$id == this_country, ]
# # 
# # 
# # chile_yoy_xts <- chile_tbl[["yoy_q_all_xts"]][[1]]
# # chile_yoy_mts <- chile_tbl[["yoy_q_all_mts"]][[1]]
# # 
# # chile_fd_yoy_xts <- chile_tbl[["yoy_firstdiff_q_all_xts"]][[1]]
# # chile_fd_yoy_mts <- chile_tbl[["yoy_firstdiff_q_all_mts"]][[1]]
# # 
# # chile_yoy_xts[,1]
# # chile_yoy_mts[,1]
# # 
# # chile_fd_yoy_xts[,1]
# # chile_fd_yoy_mts[,1]
# 
