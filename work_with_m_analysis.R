library(readxl)
library(tidyverse)
Argentina_m_analysis_rgdp <- read_excel("data/Argentina_m_analysis_rgdp.xlsx")
View(Argentina_m_analysis_rgdp)

arg_m <- Argentina_m_analysis_rgdp

all_rmse <- arg_m[, c("rmse1", "rmse2", "rmse3", "rmse4", "rmse5", "rmse6", "rmse7", "rmse8")]

arg_m$mean_rmse <- rowMeans(all_rmse)

all_rmse <- c(arg_m[ , "cond_exo"], all_rmse)

arg_m <- arg_m %>% arrange(., mean_rmse)
