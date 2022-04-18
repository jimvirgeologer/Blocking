library(purrr)
library(tidyverse)
library(readxl)
library(writexl)

setwd("~/current work/01_R_Projects/02_Blocking/FC_SHEETS")
file.list <- list.files(getwd(),pattern='.xls', recursive = TRUE)

# df <- lapply(file.list, function(i){
#   x = read_xls(i, sheet = 1)
# }) %>% bind_rows %>% as.data.frame()


Face_Sheet <- function(i){
  x = read_xls(i, sheet = 1)
  x[,22] <- x[3,15]
  x[,23] <- x[2,15]
  x[,24] <- x[52,4]
  x[,25] <- as.numeric(x[29,5]) * as.numeric(x[29,7])
  x[,26] <- ""
  VWG <- as.numeric(x[29,5]) * as.numeric(x[29,7])
  for(j in 10:16) {
    x[j,26] <- if((0.517)%%VWG==0) {
      "mv" } else {
        ""
      }
    }
 
  x <- x[-6,]
  x <- x[,-c(1:2,16:21)]
  return(x)
}


df <- lapply(file.list, Face_Sheet) %>%
  bind_rows %>%
  as.data.frame()




df_2 <- df %>%
transmute( DATE = as.numeric(...16),
    DATE = as.Date(DATE, origin = "1899-12-30"),
    SHEET = as.character(...14),  
    SAMPLE_ID = as.numeric(...1),
         LEVEL = as.numeric(...15),
          FROM = as.numeric(...2),
          TO = as.numeric(...3),
          LENGTH = as.numeric(...4),
          AU_gpt = as.numeric(SURVEY),
          AG_gpt = as.numeric(...7),
          CU_perc = as.numeric (...8),
          PB_perc = as.numeric(...9),
          ZN_perc = as.numeric (...10))

