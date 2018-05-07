library(tidyverse)
# library()
# load("~/GitHub/var_search/data/arg_200k_estimation.RData")

load("./data/arg_200k_estimation.RData")

ensemble_vbls_lags <- function(list_of_vbls, list_of_lags, type = "cons",
                               h_max = 8, is_diff_yoy = TRUE) {
  a = 1
}

models_and_accu <- models_and_accu %>% 
  mutate(accu_lev = map(accu_lev, unlist),
         accu_yoy = map(accu_yoy, unlist))


ma_diff_yoy <- models_and_accu %>% 
  filter(diff_ranking <= 30) %>% 
  dplyr::select(variables, lags) 
v_diff_yoy <- reduce(ma_diff_yoy[["variables"]], c)
lag_diff_yoy <- reduce(ma_diff_yoy[["lags"]], c)

ma_yoy <- models_and_accu %>% 
  filter(yoy_ranking <= 30) %>% 
  dplyr::select(variables, lags)
v_yoy <- reduce(ma_yoy[["variables"]], c)
lag_yoy <- reduce(ma_yoy[["lags"]], c)

ma_level <- models_and_accu %>% 
  filter(level_ranking <= 30) %>% 
  dplyr::select(variables, lags)
v_level_yoy <- reduce(ma_level[["variables"]], c)
lag_level_yoy <- reduce(ma_level[["lags"]], c)


plots_variables_and_lags <- function(models_and_accu, type = "all") {
  
  models_and_accu <- models_and_accu %>% 
    mutate(accu_lev = map(accu_lev, unlist),
           accu_yoy = map(accu_yoy, unlist))
  
  sel_variables <- models_and_accu$variables
  sel_variables <- reduce(sel_variables, c)
  sel_lags <- models_and_accu$lags
  sel_lags <- reduce(sel_lags, c)
  
  from_diff_yoy <- models_and_accu %>% 
    filter(diff_ranking <= 30) %>% 
    dplyr::select(variables, lags) 
  
  from_yoy <- models_and_accu %>% 
    filter(yoy_ranking <= 30) %>% 
    dplyr::select(variables, lags)
  
  from_level <- models_and_accu %>% 
    filter(level_ranking <= 30) %>% 
    dplyr::select(variables, lags)
  
  
  
  v_dyl <- full_join(v1, v2, by = "v") %>% 
    full_join(v3, by = "v") %>% 
    filter(v != "rgdp") %>% 
    gather(key = "group", value = "n", -v) %>% 
    mutate(n = ifelse(is.na(n), 0, n),
           n_diff_o = ifelse(group == "n_diff", yes = n, 
                             no = ifelse(group == "n_yoy", 0, -1))
    ) %>% 
    arrange(desc(n_diff_o)) %>% 
    mutate(v = factor(v, levels = unique(v)),
           group = factor(group, levels = c("n_level", "n_yoy", "n_diff" ))
    )
  
  sb <- ggplot(vbl_dyl, aes(x = v, y = n, fill = group)) +  
    geom_bar(stat = "identity") + 
    coord_flip()
  
  
  
}


models_and_accu <- models_and_accu %>% 
  mutate(accu_lev = map(accu_lev, unlist),
         accu_yoy = map(accu_yoy, unlist))

sel_variables <- models_and_accu$variables
sel_lags <- models_and_accu$lags
sel_variables <- reduce(sel_variables, c)
sel_lags <- reduce(sel_lags, c)



plot_variables <- function(models_and_accu) {
  
  sel_variables <- models_and_accu$variables
  sel_lags <- models_and_accu$variables
  sel_variables <- reduce(sel_variables, c)
  
  # table_variables <- sort(table(sel_variables), decreasing = TRUE)
  # barplot(table_variables, horiz = TRUE)
  
  vbl_n <- tibble(v = sel_variables) %>% group_by(v)  %>% 
    summarise(n = n()) %>% 
    arrange(desc(n))

  vbl_n$v <- factor(vbl_n$v, levels = vbl_n$v[order(-vbl_n$n, decreasing = TRUE)])
  
  g <- ggplot(data = vbl_n, aes(x = v, y = n)) + 
    geom_bar(stat = "identity") + 
    coord_flip()
  
  return(list(g, vbl_n))

}

g_all <-  plot_variables_one_group(models_and_accu = models_and_accu)
print(g_all[[1]])
vbl_all <- g_all[[2]]




g_diff <-  plot_variables(models_and_accu = moac_diff_yoy)
print(g_diff[[1]])
vbl_diff <- g_diff[[2]]
names(vbl_diff)[2] <- "n_diff"

g_yoy <-  plot_variables(models_and_accu = moac_yoy)
print(g_yoy[[1]])
vbl_yoy <- g_yoy[[2]]
names(vbl_yoy)[2] <- "n_yoy"

g_level <-  plot_variables(models_and_accu = moac_level)
print(g_level[[1]])
vbl_level <- g_level[[2]]
names(vbl_level)[2] <- "n_level"


plot(models_and_accu$accu_diff_yoy, models_and_accu$accu_yoy)
plot(models_and_accu$accu_diff_yoy, models_and_accu$accu_lev)



plot_3_groups <- function(v1, v2, v3, draw = TRUE) {
  
  vbl_dyl <- full_join(v1, v2, by = "v") %>% 
    full_join(v3, by = "v") %>% 
    filter(v != "rgdp") %>% 
    gather(key = "group", value = "n", -v) %>% 
    mutate(n = ifelse(is.na(n), 0, n),
           n_diff_o = ifelse(group == "n_diff", yes = n, 
                             no = ifelse(group == "n_yoy", 0, -1))
    ) %>% 
    arrange(desc(n_diff_o)) %>% 
    mutate(v = factor(v, levels = unique(v)),
           group = factor(group, levels = c("n_level", "n_yoy", "n_diff" ))
    )
  
  sb <- ggplot(vbl_dyl, aes(x = v, y = n, fill = group)) +  
    geom_bar(stat = "identity") + 
    coord_flip()
  
  if (draw) {
    print(sb)
    
  }
  
  return(sb)
  
}


p3d <- plot_3_groups(vbl_diff, vbl_yoy, vbl_level)




