# This script is a function that generates spatial-temporal correlated data
# for one single subject


N <- 100 # sample size
# generate individual score
xi_mat <- mvtnorm::rmvnorm(N, mean = rep(0, 2*K), sigma = diag(rep(1, 2*K)))



for(i in 1:N){
  
  # moving average noise
  Zmat <- matrix(rnorm(length(s1)*length(s2), 0, 1), length(s1), length(s2))
  MAmat <- MA_rand_field(15, Zmat)
  df_all[df_all$id==i, "err"] <- as.vector(MAmat)
  
  df_i <- expand.grid(s1=s1, s2=s2) %>% mutate(err = as.vector(MAmat))
  
  # the first outcome
  
  
  
  
}

GenData <- function(K1=2, K2=2, ma_size=15, s1=1:256, s2 = 1:256, 
                    t = seq(0.2, 1, by = 0.2), xi){

  # moving average noise
  Zmat <- matrix(rnorm(length(s1)*length(s2), 0, 1), length(s1), length(s2))
  MAmat <- MA_rand_field(ma_size, Zmat)
  df_i <- expand.grid(s1=s1, s2=s2) %>% mutate(err = as.vector(MAmat))
  
  # generate score
  
  
}

K <- 2
# randoms core of Y1 and Y2
xi <- mvtnorm::rmvnorm(1, mean = rep(0, 2*K), sigma = diag(rep(1, 2*K)))
```


```{r}
df_ma$phi1 <- sqrt((df_ma$s1/256-mean(s1)/256)^2+(df_ma$s2/256-mean(s2)/256)^2)
df <- expand_grid(df_ma, t=t)

# the first outcome
df <- df %>%
  mutate(Y1 = phi1*xi[1]+t*xi[2]+k15)

# the second outcome
df <- df %>% mutate(Y2 = 2*t*Y1+xi[3]*phi1+xi[4]*t)
df$Y2 <- df$Y2+rnorm(nrow(df), 0, 0.2)

```