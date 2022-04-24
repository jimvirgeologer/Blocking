library(purrr)
library(tidyverse)
library(readxl)
library(writexl)
library(adagio)
library(dplyr)
library(MASS)
library(visdat)
###########DATA BASE###############
setwd("~/current work/01_R_Projects/02_Blocking/FC_SHEETS")
file.list <- list.files(getwd(),pattern='.xls', recursive = TRUE)
file.list <- file.list[!grepl("000",file.list)]

# txt_paths <- vec[grepl(".txt$", vec)]

########### READ EXCEL ############3
Face_Sheet <- function(i){
  x = read_xls(i, sheet = 1)
  x[,22] <- x[3,15]
  x[,23] <- x[2,15]
  x[,24] <- x[52,4]
  x[,25] <- as.numeric(x[29,5]) * as.numeric(x[29,7])
  x[,26] <- ""
  x <- x[-6,]
  x <- x[,-c(1:2,16:21)]
  colnames(x) <- c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14","c15","c16","c17","c18")
  
  MV_TXG <- as.numeric(x[28,3])* as.numeric(x[28,5])
  
  x <- x %>% transmute( SHEET = as.character(c14),
                        DATE = as.numeric(c16),
                        DATE = as.Date(DATE, origin = "1899-12-30"),
                        SAMPLE_ID = as.numeric(c1),
                        LEVEL = as.numeric(c15),
                        FROM = round(as.numeric(c2),2),
                        TO = round(as.numeric(c3),2),
                        LENGTH = round(as.numeric(c4),10),
                        AU_gpt = as.numeric(c6),
                        AG_gpt = round(as.numeric(c7),2),
                        CU_perc = round(as.numeric (c8),2),
                        PB_perc = round(as.numeric(c9),2),
                        ZN_perc = round(as.numeric (c10),2),
                        W_AU = AU_gpt * LENGTH,
                        MV = as.character(NA)) %>%
    filter(!is.na(W_AU),
      !is.na(FROM),
      W_AU !=0)
  
S <- as.integer(unlist(x["W_AU"] * 10000))
t <- (MV_TXG * 10000)
# 

t <- ifelse(t <= 0 ,2, t) %>% as.integer()
S <- ifelse(S > t , t - 1, S) %>% as.integer()
S <- ifelse(S <1 , 1, S) %>% as.integer()

  
# 

# which <- which.max(unlist(x["W_AU"] %% 1))
# S[which]<- S[which] + 1


subsum <- function(b,c) {
  sol <- subsetsum(b,c)
  sol$inds
  
}

subsum2 <- function(f,g,h) {
  which <- which.max(unlist(f["W_AU"] %% 1))
  g[which]<- g[which] + 1
  sol <- subsetsum(g,h)
  sol$inds
  
}



# x[,"MV"] <- "PAWER"
# result <- subsum2(x,S,t)



# sol <- subsetsum(S,t)
# j <- sol$inds
# x[j,"MV"] <- "MV"




######### Sub set sum #############
T <- try(subsum(S,t),silent = T)
result <- T


if (inherits(T, "try-error")) {subsum2(x,S,t)}

x[result,"MV"] <- "MV"

# 
# result <- subsum2(x,S,t)
# 
# x[result,"MV"] <- "MV"

######### Return Vaues ########
a <- is.integer(S) %>% as.data.frame()
b <- is.integer(t) %>% as.data.frame()
d <-  S <= t
S <- S %>% as.data.frame()
# wew <- wew %>% as.data.frame()
final <- cbind(S,a,t,b,d,x)


return(final)

}


df <- lapply(file.list, Face_Sheet) %>%
bind_rows %>%
as.data.frame()


vis_miss(df)



# df_2 <- df %>% transmute(S = as.integer(....1))


colSums(df["....1"])
colSums(df["W_AU"])


