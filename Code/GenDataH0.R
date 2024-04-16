# This script generate data using a weighted average filter
# Under the null hypothesis that Y1 and Y2 are not correlated
# Using functions defined in RandomField.R


##### Set-up #####

# the function to generate moving average field from white noise field
source(here("Code/RandomField.R"))

nS <- 32 # space grid
Nf <- 1000 # generate 1000 subjects over all, but use only 100 for the block bootstrap

#### Null Hypothesis, Filter size ####

# generated data using simple average with different filter size 
ksize_vec <- seq(1, 9, by = 2) # filter size

# container
df_ksize<- expand_grid(ksize = ksize_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)
df_ksize$Y1 <- df_ksize$Y2 <- NA

# generate outcomes
pb <- txtProgressBar(min=0, max=Nf, style = 3)

t1 <- Sys.time()
for(i in 1:Nf){ # fix a subject
  
  for(k in seq_along(ksize_vec)){ # fix a filter size
    
    # simple average: equal weight
    wt <- matrix(1, ksize_vec[k], ksize_vec[k])
    
    # generate Y1
    ## a moving average error
    Y1 <- MWA_rand_field(ksize_vec[k], nS, wt = wt)
    df_ksize$Y1[df_ksize$ksize==ksize_vec[k] & df_ksize$id==i] <- as.vector(Y1)
    
    # generate Y2
    ## a moving average error
    Y2 <- MWA_rand_field(ksize_vec[k], nS, wt = wt)
    df_ksize$Y2[df_ksize$ksize==ksize_vec[k] & df_ksize$id==i] <- as.vector(Y2)
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # 7 minutes

# save data
save(df_ksize, file = here("Data/sim_H0_ksize.RData"))


#### Null Hypothesis, Exponential Weight ####

alpha_vec <- c(0, 0.5, 1, 2.5, 5) # exponential weight parameters

# create weight matrix
# weight proportional to inverse exponential Euclidian to center (to avoid zero demonimator)
# filter size fixed to 5
wt_exp <- array(NA, dim = c(5, 5, length(alpha_vec)))
for(m in seq_along(alpha_vec)){
  for(i in 1:5){
    for(j in 1:5){
      wt_exp[i,j, m] <- exp(-alpha_vec[m]*((i-3)^2+(j-3)^2))
    }
  }
  
  # normalization
  wt_exp[,, m] <- wt_exp[,,m]*(5^2)/sum(wt_exp[,,m])
}

# generate data 
# container
df_expwt <- expand_grid(alpha=alpha_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)
df_expwt$Y1 <- df_expwt$Y2 <- NA

# generate outcomes
pb <- txtProgressBar(min=0, max=Nf, style = 3)

t1 <- Sys.time()
for(i in 1:Nf){ # fix a subject
  
  for(k in seq_along(alpha_vec)){ # fix a time point
    
    # simple average: equal weight
    wt <- wt_exp[,,k]
    
    # generate Y1
    ## a moving average error
    Y1 <- MWA_rand_field(5, nS, wt = wt)
    df_expwt$Y1[df_expwt$alpha==alpha_vec[k] & df_expwt$id==i] <- as.vector(Y1)
    
    # generate Y2
    ## a moving average error
    Y2 <- MWA_rand_field(5, nS, wt = wt)
    df_expwt$Y2[df_expwt$alpha==alpha_vec[k] & df_expwt$id==i] <- as.vector(Y2)
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # 7 minutes

# save data
save(df_expwt, file = here("Data/sim_H0_expwt.RData"))

# save weight
save(wt_exp, file = here("Data/wt_exp_mat.RData"))


#### Null Hypothesis, Weight type ####

# types of weight matrices to look into
# also fix filter size to be 5
wt_type_vec <- c("wt_inc", "wt_dec", "wt_u", "wt_sin", "wt_cos")

wt_type <- expand_grid(s1 = 1:5, s2 = 1:5)
## Euclidean distance to center
wt_type$dist <- sqrt((wt_type$s1-3)^2+(wt_type$s2-3)^2)
# range(df_wt_type$dist)
# exp(2.83)

# what if weight is a reverse U shape function wrt distance? 
wt_type <- wt_type %>% 
  mutate(wt_u = (dist-1.5)^2) %>% 
  mutate(wt_inc = exp(dist-max(dist))) %>% 
  mutate(wt_dec = exp(-dist)) %>% 
  mutate(wt_sin = sin(2*pi*dist/max(dist))) %>%
  mutate(wt_cos = cos(2*pi*dist/max(dist))) 

# scale so that they have the same baseline variation
wt_type <- wt_type %>% mutate_at(vars(starts_with("wt_")),
                                 function(x){x*5/sqrt(sum(x^2))}) 

wt_type %>% summarise_at(vars(starts_with("wt_")),
                         function(x){mean(x^2)})



# generate data
df_wt_type <- expand_grid(wt_type = wt_type_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)
df_wt_type$Y1 <- df_wt_type$Y2 <- NA

pb <- txtProgressBar(min=0, max=Nf, style = 3)

t1 <- Sys.time()
for(i in 1:Nf){ # fix a subject
  
  for(k in seq_along(wt_type_vec)){ # fix a type of weight
    
    # weight matrix
    this_wt <- matrix(as_vector(wt_type[, wt_type_vec[k]]), 5, 5)
    
    # generate Y1
    MAerr_mat1 <- MWA_rand_field(kf = 5, ki = 32, wt = this_wt)
    df_wt_type$Y1[df_wt_type$id==i & df_wt_type$wt_type==wt_type_vec[k]] <- as.vector(MAerr_mat1)
    
    # generate Y1
    MAerr_mat2 <- MWA_rand_field(kf = 5, ki = 32, wt = this_wt)
    df_wt_type$Y2[df_wt_type$id==i & df_wt_type$wt_type==wt_type_vec[k]] <- as.vector(MAerr_mat2)
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # about 7 minutes

# save data
save(df_wt_type, file = here("Data/sim_H0_wt_type.RData"))

# save weight
save(wt_type, file = here("Data/wt_type.RData"))


#### Null hypothesis, stride #####
stride_vec <- seq(0, 8, by = 2)

# generate data
# only one outcome
# for one single subject
df_stride <- expand_grid(stride = stride_vec, s2=1:nS, s1 = 1:nS)
df_stride$Y1 <- NA


for(i in seq_along(stride_vec)){
  sti_size <- stride_vec[i]
  MAerr_mat <- MA_rand_field_step(kf = 5, ki = 32, stride = sti_size)
  df_stride$Y1[df_stride$stride==stride_vec[i]] <- as.vector(MAerr_mat)
}

# save data
save(df_stride, file = here("Data/sim_H0_stride.RData"))


##### Null hypothesis, decreasing weight #####

# further look into the decreasing weight function
# I am trying to figure out why the type I error this decreasing function 
# is not going down to the same level as in the exponential weight case
# since one is exp(-d) and the other is exp(-d^2), could it be the power? 

# my speculation: the flatter the weight is, the harder it is to lower type I error

# Euclidean distance
wt_dec <- expand_grid(s1 = 1:5, s2 = 1:5)
## Euclidean distance to center
wt_dec$dist <- sqrt((wt_dec$s1-3)^2+(wt_dec$s2-3)^2)

# weight
wt_dec <- wt_dec %>%
  mutate(exp_wt0.5 = exp(-sqrt(dist)),
         exp_wt1 = exp(-dist),
         exp_wt2 = exp(-dist^2),
         exp_wt3 = exp(-dist^3))

wt_dec %>% pivot_longer(starts_with("exp_wt")) %>% 
  ggplot()+
  geom_tile(aes(x=s1, y=s2, fill=value))+
  facet_wrap(~name)

# The higher the power, the more weight is put in center
wt_dec_vec <- colnames(wt_dec)[4:7]

# generate data
df_wt_dec <- expand_grid(wt_dec = wt_dec_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)
df_wt_dec$Y1 <- df_wt_dec$Y2 <- NA

pb <- txtProgressBar(min=0, max=Nf, style = 3)

t1 <- Sys.time()
for(i in 1:Nf){ # fix a subject
  
  for(k in seq_along(wt_dec_vec)){ # fix a type of weight
    
    # weight matrix
    this_wt <- matrix(as_vector(wt_dec[, wt_dec_vec[k]]), 5, 5)
    
    # generate Y1
    MAerr_mat1 <- MWA_rand_field(kf = 5, ki = 32, wt = this_wt)
    df_wt_dec$Y1[df_wt_dec$id==i & df_wt_dec$wt_dec==wt_dec_vec[k]] <- as.vector(MAerr_mat1)
    
    # generate Y1
    MAerr_mat2 <- MWA_rand_field(kf = 5, ki = 32, wt = this_wt)
    df_wt_dec$Y2[df_wt_dec$id==i & df_wt_dec$wt_dec==wt_dec_vec[k]] <- as.vector(MAerr_mat2)
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # about 7 minutes

# figure
df_wt_dec %>% filter(id==1) %>%
  ggplot(aes(x=s1, y=s2, fill= Y1))+
  geom_tile()+
  facet_wrap(~wt_dec)


# save data
save(df_wt_type, file = here("Data/sim_H0_wt_type.RData"))

