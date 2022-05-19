  ########### Solution ###############333
  sol1 <- mpfr(df$W_AU,52)
  sol1 <- round(sol1,digits = 15)
  solu <- as.integer(sol1 * 10000)
  
  sol2 <- sol1 %>% as.numeric() * 10000
  sol2 <- as.integer(sol2)
  
  ############ Solution
  
  
  
  df_2 <- df %>% 
    transmute(SHEET = SHEET,
              W_AU_RAW = W_AU,
              sol1 = solu,
               W_AU = as.integer(W_AU * 10000),
              W_AU2 = sol2,
              diff = W_AU - W_AU2)
  
  
  
  
