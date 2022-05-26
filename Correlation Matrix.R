library(dplyr)
library(ggplot2)
library(tidyverse)
library(PerformanceAnalytics)

df %>% filter(MV == "MV") %>% ggplot(aes(x = AU_gpt, y = CU_perc)) + scale_x_log10() +scale_y_log10()+ geom_point() + facet_wrap(~LEVEL)




df_2 <-  df %>% filter(MV == "MV") 
df_3 <- df[,c(7,8,9,10,11,12)]



chart.Correlation(df_3, histogram=TRUE, pch=19)