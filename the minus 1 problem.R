S <- df["W_AU"] * 10000 
S1 <- S[11,1]
S1 



S <- S %>% unlist() %>% as.integer()
S2 <- S[11]
S2 

S1 == S2
S1 > S2
S1 < S2

S1 == 5600

S2 == 5600

as.integer(5600.00) > as.integer(S[11])

5600 > S[11]
S[11]


str(5600)
str(S[11])

as.vector(2.0) %>% as.integer()

as.vector(5600.00) %>% as.integer()

str[S11]
S[50] <- 50

S2 <- S %>% as.integer()
S2
