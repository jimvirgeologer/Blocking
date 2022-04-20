library(purrr)
library(tidyverse)
library(readxl)
library(writexl)
library(adagio)

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
          FROM = round(as.numeric(...2),2),
          TO = round(as.numeric(...3),2),
          LENGTH = round(as.numeric(...4),2),
          AU_gpt = round(as.numeric(SURVEY),2),
          AG_gpt = round(as.numeric(...7),2),
          CU_perc = round(as.numeric (...8),2),
          PB_perc = round(as.numeric(...9),2),
          ZN_perc = round(as.numeric (...10),2),
          W_AU = AU_gpt * LENGTH,
    MV = as.character(NA)) 

df_3 <- df_2 %>% 
  filter(!is.na(W_AU),
         !is.na(FROM))

MV_TXG <- (as.numeric(df[28,3])* as.numeric(df[28,5])) 


a <- df_3["W_AU"] %>%
  filter(W_AU <= MV_TXG) %>% as_vector()

a <- a * 10000
MV_TXG <- MV_TXG * 10000

sol <- subsetsum(a,MV_TXG)
j <- sol["inds"] %>% unlist()
df_3[j,"MV"] <- "MV" 
