library(purrr)
library(tidyverse)
library(readxl)
library(writexl)
library(adagio)
library(dplyr)
library(MASS)
library(visdat)
library(Rmpfr)



###########DATA BASE###############
setwd("~/current work/01_R_Projects/02_Blocking/Blocking")
file.list <- list.files(path = './FC_SHEETS', pattern = '.xls', recursive = TRUE, full.names = TRUE)
file.list <- file.list[!grepl("000", file.list)]



########### Simple Read Excel ############ 
face_sheet_read <- function(i) {
  x = read_xls(i,sheet = 1)
  x[, 22] <- x[3, 15]
  x[, 23] <- x[2, 15]
  x[, 24] <- x[52, 4]
  x[, 25] <- as.numeric(x[29, 5]) * as.numeric(x[29, 7])
  x[, 26] <- ""
  x <- x[-6, ]
  x <- x[, -c(1:2, 16:21)]
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11",
      "c12",
      "c13",
      "c14",
      "c15",
      "c16",
      "c17",
      "c18"
    )
  
  
  
  ########## MV_TXG ############
  MV_TXG <- as.numeric(x[28, 3]) * as.numeric(x[28, 5])
  t <- (MV_TXG * 30000) %>% zapsmall(10)
  
  ########### Transmuting x (database) #########
  x <- x %>% transmute(
    SHEET = as.character(c14),
    DATE = as.numeric(c16),
    DATE = as.Date(DATE, origin = "1899-12-30"),
    SAMPLE_ID = as.numeric(c1),
    LEVEL = as.numeric(c15),
    FROM = round(as.numeric(c2), 2),
    TO = round(as.numeric(c3), 2),
    LENGTH = round(as.numeric(c4), 16),
    AU_gpt = round(as.numeric(c6), 16),
    AG_gpt = round(as.numeric(c7), 2),
    CU_perc = round(as.numeric (c8), 2),
    PB_perc = round(as.numeric(c9), 2),
    ZN_perc = round(as.numeric (c10), 2),
    W_AU = round(AU_gpt * LENGTH, 16),
    MV = as.character("WALLROCK")
  ) %>%
    filter(!is.na(W_AU),!is.na(FROM),
           W_AU != 0)
  
  x$SHEET <- gsub("FC_","",as.character(x$SHEET)) ######### Removing FC in names ########
  
  
  sol3 <- x$W_AU * 30000 %>% round(digits = 15)
  S <- sol3 %>% zapsmall(1)
  
  
  t <- ifelse(t <= 0 , 2, t) %>% as.integer()
  S <- ifelse(S > t , t - 1, S) %>% as.integer()
  
  ########## Subsum ###############3
  subsum <- function(b, c) {
    sol <- subsetsum(b, c)
    sol$inds
  }
  
  MV_loc<- subsum(S,t)
  result <- MV_loc
  x[result, "MV"] <- "MV"
  
  
  
  ################### Vein Parameters #########
  floating <- "FW"
  
  for (i in 1:nrow(x)) {
    x[i,14] <- if(x[i,14] == "WALLROCK") {
      floating
    } else if (x[i,14] == "MV") {
      "MV"
    }
    
  floating <- if(x[i,14] == "WALLROCK") {
      "FW"
    } else if (x[i,14] == "MV") {
      "HW"
    }
    
    
  }
  
  
  
  
  
# floating <- "FW"
#   
#   MV_VAL <- function(i) {
#             if(i == "WALLROCK") {
#               floating
#             } else if (i == "MV") {
#              i <- "MV"
#              floating = "mark" 
#             }
#   }
#   
#   
# x$MV <- lapply(x$MV,MV_VAL)





x <- x %>% mutate(AU_gpt_2= ifelse(AU_gpt >25, 25, AU_gpt),
                                      LEN_AU = LENGTH * AU_gpt_2)


x <- x %>% group_by(SHEET,MV) %>% summarize(LENGTH = sum(LENGTH),
                                AU_gpt = sum(LEN_AU)/sum(LENGTH))
MV_L <-dplyr::select(filter(x, MV == "MV"), LENGTH)

WL_L <- (3- MV_L[[2]] )/ 2
  
x$Block <- (MV_L[[2]] * x[3,4] + WL_L * x[1,4] + WL_L * x[2,4] )/3
  
  ############### Binding##############
  final <- cbind(x)
  return(final)
}

######## Applying function to all in the file.list ############
df_trial <- lapply(file.list[1:15], face_sheet_read) %>%
  bind_rows %>%
  as.data.frame()