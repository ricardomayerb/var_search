library(tidyverse)

load("./data/arg_200k_estimation.RData")



models_and_accu <- models_and_accu %>% 
  mutate(accu_lev = map(accu_lev, unlist),
         accu_yoy = map(accu_yoy, unlist))

models_and_accu <- models_and_accu %>%  
  mutate(n_variables = map(variables, length))

ma_diff_yoy <- models_and_accu %>% 
  filter(diff_ranking <= 30)

ma_yoy <- models_and_accu %>% 
  filter(yoy_ranking <= 30)

ma_level <- models_and_accu %>% 
  filter(level_ranking <= 30)

vbls_diff <- reduce(ma_diff_yoy$variables, c) 
count_vbls_diff <- vbls_diff %>% tibble(v = .) %>% 
  group_by(v) %>% summarise(n_diff = n())

vbls_yoy <- reduce(ma_yoy$variables, c)
count_vbls_yoy <- vbls_yoy %>% tibble(v = .) %>% 
  group_by(v) %>% summarise(n_yoy = n())

vbls_level <- reduce(ma_level$variables, c)
count_vbls_level <- vbls_level %>% tibble(v = .) %>% 
  group_by(v) %>% summarise(n_level = n())

n_endo_diff <- reduce(ma_diff_yoy$n_variables, c)
count_n_endo_diff <- n_endo_diff %>% tibble(size = .) %>% 
  group_by(size) %>% summarise(size_diff = n())

n_endo_yoy <- reduce(ma_yoy$n_variables, c)
count_n_endo_yoy <- n_endo_yoy %>% tibble(size = .) %>% 
  group_by(size) %>% summarise(size_yoy = n())

n_endo_level <- reduce(ma_level$n_variables, c)
count_n_endo_level <- n_endo_level %>% tibble(size = .) %>% 
  group_by(size) %>% summarise(size_level = n())

lags_diff <- reduce(ma_diff_yoy$lags, c)
count_lags_diff <- lags_diff %>% tibble(lag = .) %>% 
  group_by(lag) %>% summarise(lag_diff = n())

lags_yoy <- reduce(ma_yoy$lags, c)
count_lags_yoy <- lags_yoy %>% tibble(lag = .) %>% 
  group_by(lag) %>% summarise(lag_yoy = n())

lags_level <- reduce(ma_level$lags, c)
count_lags_level <- lags_level %>% tibble(lag = .) %>% 
  group_by(lag) %>% summarise(lag_level = n())

# vbls <- reduce(list(count_vbls_diff, count_vbls_yoy, count_vbls_level), 
#                full_join, by = "v")  %>% 
#   gather(key = "group", value = "n", -v) %>% 
#   mutate(group = factor(group, levels = c( "n_level", "n_yoy", "n_diff")),
#          v = factor(v, levels = unique(v)),
#          v = fct_reorder2(v, group, n)) 

vbls <- reduce(list(count_vbls_diff, count_vbls_yoy, count_vbls_level), 
               full_join, by = "v")  %>% 
  gather(key = "group", value = "n", -v) %>%
  mutate(group = factor(group, levels = c( "n_level" , "n_yoy", "n_diff"))) %>% 
  arrange(group, desc(n)) %>% 
  mutate(v_order = row_number())

g_vbls_facets <- ggplot(vbls, aes(x = v_order, y = n, fill = group)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ group, scales = "free") + 
  ylab("Number of appearances in selected VARs") +
  xlab("variable") + 
  scale_x_continuous(
    breaks = vbls$v_order,
    labels = vbls$v,
    expand = c(0,0)
  ) +
  coord_flip()
print(g_vbls_facets)

g_vbls_stacked <- ggplot(data = vbls, aes(x = fct_reorder2(v, group, n), 
                                          weight = n, fill = group)) + 
  geom_bar()  + 
  coord_flip()
print(g_vbls_stacked) 




endo <- reduce(list(count_n_endo_diff, count_n_endo_yoy, count_n_endo_level), 
               full_join, by = "size") %>% 
  gather(key = "group", value = "freq", -size) %>%
  mutate(group = factor(group, levels = c( "size_level" , "size_yoy", "size_diff"))) %>% 
  arrange(group, desc(freq)) %>% 
  mutate(size_order = row_number())

g_endo_facets <- ggplot(endo, aes(x = size_order, y = freq, fill = group)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ group) + 
  ylab("Number of appearances in selected VARs") +
  xlab("size") + 
  scale_x_continuous(
    breaks = endo$size_order,
    labels = endo$size
  ) +
  coord_flip()
print(g_endo_facets)

g_endo_stacked <- ggplot(data = endo, aes(x = size, 
                                          weight = freq, fill = group)) + 
  geom_bar(position = "dodge") 
print(g_endo_stacked)


n_lags <- reduce(list(count_lags_diff, count_lags_yoy, count_lags_level), 
               full_join, by = "lag") %>% 
  gather(key = "group", value = "freq", -lag) %>%
  mutate(group = factor(group, levels = c( "lag_level" , "lag_yoy", "lag_diff"))) %>% 
  arrange(group, desc(freq)) %>% 
  mutate(lag_order = row_number())

g_lag_facets <- ggplot(n_lags, aes(x = lag_order, y = freq, fill = group)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ group, scales = "free_y") + 
  ylab("Number of appearances in selected VARs") +
  xlab("lags") + 
  scale_x_continuous(
    breaks = n_lags$lag_order,
    labels = n_lags$lag
  ) +
  coord_flip()
print(g_lag_facets)


g_lag_stacked <- ggplot(data = n_lags, aes(x = lag, 
                                          weight = freq, fill = group)) + 
  geom_bar(position = "dodge") 
print(g_lag_stacked)



variables_table <- reduce(list(count_vbls_diff, count_vbls_yoy, count_vbls_level), 
                             full_join, by = "v") %>% 
  rename(variable = v,
         fcs_diff_yoy = n_diff,
         fcs_yoy = n_yoy,
         fcs_level = n_level) %>% 
  mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
         fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
         fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
         )
variables_table


lags_table <- reduce(list(count_lags_diff, count_lags_yoy, count_lags_level), 
                 full_join, by = "lag") %>% 
  rename(fcs_diff_yoy = lag_diff,
         fcs_yoy = lag_yoy,
         fcs_level = lag_level) %>% 
  mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
         fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
         fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
  )
lags_table

sizes_table <- reduce(list(count_n_endo_diff, count_n_endo_yoy, count_n_endo_level), 
                 full_join, by = "size") %>% 
  rename(fcs_diff_yoy = size_diff,
         fcs_yoy = size_yoy,
         fcs_level = size_level) %>% 
  mutate(fcs_diff_yoy = ifelse(is.na(fcs_diff_yoy),  0, fcs_diff_yoy),
         fcs_yoy = ifelse(is.na(fcs_yoy),  0, fcs_yoy),
         fcs_level = ifelse(is.na(fcs_level),  0, fcs_level)
  )
sizes_table


g_diff_vs_yoy_best_diffs <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 30),
                             aes(x = accu_diff_yoy, y = accu_yoy)) + 
  geom_point()
g_diff_vs_yoy_best_diffs


g_diff_vs_lev_best_diffs <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 30),
                                   aes(x = accu_diff_yoy, y = accu_lev)) + 
  geom_point()
g_diff_vs_lev_best_diffs


g_diff_vs_yoy_best_yoys <- ggplot(data =  models_and_accu %>% filter(yoy_ranking <= 30),
                                   aes(x = accu_yoy, y = accu_diff_yoy)) + 
  geom_point()
g_diff_vs_yoy_best_yoys


g_lev_vs_yoy_best_lev <- ggplot(data =  models_and_accu %>% filter(level_ranking <= 30),
                                 aes(x = accu_lev, y = accu_diff_yoy)) + 
  geom_point()
g_lev_vs_yoy_best_lev 


g_best_diff_accu <- ggplot(data =  models_and_accu %>% filter(diff_ranking <= 30),
                           aes(x = accu_diff_yoy)) + 
  geom_histogram(bins = 7)
print(g_best_diff_accu)

g_best_yoy_accu <- ggplot(data =  models_and_accu %>% filter(yoy_ranking <= 30),
                           aes(x = accu_yoy)) + 
  geom_histogram(bins = 7)
print(g_best_yoy_accu)

g_best_lev_accu <- ggplot(data =  models_and_accu %>% filter(level_ranking <= 30),
                          aes(x = accu_lev)) + 
  geom_histogram(bins = 7)
print(g_best_lev_accu)


