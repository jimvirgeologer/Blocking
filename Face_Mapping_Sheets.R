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
  # x <- x[-6,]
  # x <- x[,-c(1,16:21)]
  
  return(x)
}


df <- lapply(file.list, Face_Sheet) %>%
  bind_rows %>%
  as.data.frame()

