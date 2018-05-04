library(tidyverse)
# library()
# load("~/GitHub/var_search/data/arg_200k_estimation.RData")

ensemble_vbls_lags <- function(list_of_vbls, list_of_lags, type = "cons",
                               h_max = 8, is_diff_yoy = TRUE) {
  
}




sel_variables <- models_and_accu$variables
sel_lags <- models_and_accu$variables

sel_variables <- reduce(sel_variables, c)
sel_variables_f <- as.factor(sel_variables)
table_variables <- sort(table(sel_variables), decreasing = TRUE)

barplot(table_variables, horiz = TRUE)
print(table_variables)

df <- tibble(v = sel_variables)



vbl_n <- df %>% group_by(v)  %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
vbl_n


vbl_n$v <- factor(vbl_n$v, levels = vbl_n$v[order(-vbl_n$n, decreasing = TRUE)])

g <- ggplot(data = vbl_n, aes(x = v, y = n)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

print(g)

