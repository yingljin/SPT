
# Simulate spatial-temporal data for multiple subjects

#### set-up ####

rm(list = ls())

set.seed(123)

library(mgcv)
library(nlme)
library(tidyverse)
library(plotly)
library(mvtnorm)
library(ggpubr)
library(here)


#### One subject example ####

# set up grid
# generate a 2D image of 256 by 256
s1 <- s2 <- 1:256
t <- seq(0.2, 1, by = 0.2)
nT <- length(t)

df_i <- expand_grid(s1=s1, s2=s2, t=t)


####  MA error ####
source(here("Code/RandomField.R"))

df_i$ma_err <- NA

for(this_t in t){
  Zmat <- matrix(rnorm(length(s1)*length(s2), 0, 1), length(s1), length(s2))
  ma_mat <- MA_rand_field(15, Zmat) # kernel size = 15
  
  df_i[df_i$t==this_t, "ma_err"] <- as.vector(ma_mat)
}

#### Distance ####

# calculate standardized distance to center
df_i <- df_i %>%
  mutate(s1=scale(s1), s2 = scale(s2)) %>%
  mutate(dist = sqrt(s1^2+s2^2))

df_i %>% ggplot()+
  geom_tile(aes(x=s1, y=s2, fill = dist))+
  facet_wrap(~t)

#### Outcomes Y1, Y2 ####

# scores
xi <- mvtnorm::rmvnorm(1, mean = rep(0, 2), sigma = diag(rep(5, 2)))

df_i <- df_i %>%
  mutate(Y1 = dist*xi[1]+t*xi[2]+ma_err) 
# to increase the effect of spatial correlation
# increase the magnitude of MA error

df_i <- df_i %>%
  mutate(Y2 = 2*t*Y1+rnorm(nrow(df_i)))

df_i %>%
  mutate(diff = Y2-2*t*Y1)

df_i %>%
  ggplot()+
  geom_tile(aes(x=s1, y=s2, fill=Y2))+
  facet_wrap(~t)

df_i %>%
  ggplot()+
  geom_point(aes(x=Y1, y=Y2))+
  facet_wrap(~t)

# LM model (one subject at one time point)

fit_lm <- df_i %>% group_by(t) %>%
  group_modify(~get_beta_ci(.))

fit_lm
confint(fit_lm) 

# LM severely underestimates the slope? what is wrong? 



df_test <- data.frame(x = rnorm(256)) %>% 
  mutate(y = 0.4*x)
df_test$y <- df_test$y+rnorm(nrow(df_test))

confint(lm(y~x, data=df_test))

