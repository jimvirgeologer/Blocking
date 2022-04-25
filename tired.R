########### Solution ###############333
sol1 <- mpfr(df$W_AU,52)
sol1 <- round(sol1,digits = 15)
sol2 <- sol1 %>% as.numeric() * 10000
sol2 <- as.integer(sol2)

############ Solution



df_2 <- df %>% 
  transmute(W_AU = W_AU,
            W_AU2 = sol2)




