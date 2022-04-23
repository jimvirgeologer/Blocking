x = read_xls(file.list[[4]], sheet = 1)
x[,22] <- x[3,15]
x[,23] <- x[2,15]
x[,24] <- x[52,4]
x[,25] <- as.numeric(x[29,5]) * as.numeric(x[29,7])
x[,26] <- ""

x <- x[-6,]
x <- x[,-c(1:2,16:21)]
colnames(x) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
# colnames(x) <- c("SAMPLE_ID", "FROM", "TO", "LENGTH","TONNAGE", "Au_gpt", "Ag_gpt","Cu_perc", "Pb_perc", "Zn_perc", "VOLUME", "SG","1", "SHEET","LEVEL" ,"DATE", "MV")

which( duplicated( names( x ) ) )

x<- x %>% as.data.frame()
str(x)


