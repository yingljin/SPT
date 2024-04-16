# This script generate data using a weighted average filter
# Under the alternative hypothesis that Y1 and Y2 are correlated
# Using functions defined in RandomField.R


#### Set-up ####

# the function to generate moving average field from white noise field
source(here("Code/RandomField.R"))

nS <- 32 # space grid
Nf <- 1000 # generate 1000 subjects over all, but use only 100 for the block bootstrap
beta_true <- c(0, 1) # true coefficients

#### Filter size ####

# generated data using simple average with different filter size 
ksize_vec <- seq(1, 9, by = 2) # filter size

df_ksize_h1 <- expand_grid(ksize = ksize_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)

df_ksize_h1$Y1 <- df_ksize_h1$Y2 <- NA

# generate individual scores
# true_xi <- matrix(rnorm(2*N, 0, 1.5), nrow = N, ncol = 2)
# true_zeta <- matrix(rnorm(2*N, 0, 1.5), nrow = N, ncol = 2)

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
    df_ksize_h1$Y1[df_ksize_h1$ksize==ksize_vec[k] & df_ksize_h1$id==i] <- as.vector(Y1)
    
    # generate Y2
    ## a moving average error
    Y2_err <- MWA_rand_field(ksize_vec[k], nS, wt = wt)
    Y2_err <- as.vector(Y2_err)
    Y2_mat <- matrix(c(rep(1, nS^2), as.vector(Y1)), ncol=2, byrow = F)
    df_ksize_h1$Y2[df_ksize_h1$ksize==ksize_vec[k] & df_ksize_h1$id==i] <- Y2_mat %*% beta_true+Y2_err
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # 6 minutes

# save data
save(df_ksize_h1, file = here("Data/sim_H1_ksize.RData"))

#### Exponential weight ####

# load weight matrix
load(here("Data/wt_exp_mat.RData"))

alpha_vec <- c(0, 0.5, 1, 2.5, 5) # exponential weight parameters

df_expwt_h1 <- expand_grid(alpha=alpha_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)
df_expwt_h1$Y1 <- df_expwt_h1$Y2 <- NA

# generate individual scores
# true_xi <- matrix(rnorm(2*N, 0, 1.5), nrow = N, ncol = 2)
# true_zeta <- matrix(rnorm(2*N, 0, 1.5), nrow = N, ncol = 2)

# generate outcomes
pb <- txtProgressBar(min=0, max=Nf, style = 3)

t1 <- Sys.time()
for(i in 1:Nf){ # fix a subject
  
  for(k in seq_along(alpha_vec)){ # fix a filter size 
    
    # simple average: equal weight
    wt <- wt_exp[,,k]
    
    # generate Y1
    ## a moving average error
    Y1 <- MWA_rand_field(5, nS, wt = wt)
    df_expwt_h1$Y1[df_expwt_h1$alpha==alpha_vec[k] & df_expwt_h1$id==i] <- as.vector(Y1)
    
    # generate Y2
    ## a moving average error
    Y2_err <- MWA_rand_field(5, nS, wt = wt)
    Y2_err <- as.vector(Y2_err)
    Y2_mat <- matrix(c(rep(1, nS^2), as.vector(Y1)), ncol=2, byrow = F)
    df_expwt_h1$Y2[df_expwt_h1$alpha==alpha_vec[k] & df_expwt_h1$id==i] <- Y2_mat %*% beta_true+Y2_err
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # 7 minutes

# save data
save(df_expwt_h1, file = here("Data/sim_H1_expwt.RData"))

#### Functional shape of weight ####

# load weight matrix
load(here("Data/wt_type.RData"))

# weight type
wt_type_vec <- c("wt_inc", "wt_dec", "wt_u", "wt_sin", "wt_cos")

# container
df_wt_type_h1 <- expand_grid(wt_type=wt_type_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)
df_wt_type_h1$Y1 <- df_wt_type_h1$Y2 <- NA

# generate outcomes
pb <- txtProgressBar(min=0, max=Nf, style = 3)

t1 <- Sys.time()
for(i in 1:Nf){ # fix a subject
  
  for(k in seq_along(wt_type_vec)){ # fix a filter size 
    
    # simple average: equal weight
    wt <- wt_type[, wt_type_vec[k]]
    wt <- matrix(as_vector(wt), 5, 5)
    
    # generate Y1
    ## a moving average error
    Y1 <- MWA_rand_field(5, nS, wt = wt)
    df_wt_type_h1$Y1[df_wt_type_h1$wt_type==wt_type_vec[k] & df_wt_type_h1$id==i] <- as.vector(Y1)
    
    # generate Y2
    ## a moving average error
    Y2_err <- MWA_rand_field(5, nS, wt = wt)
    Y2_err <- as.vector(Y2_err)
    Y2_mat <- matrix(c(rep(1, nS^2), as.vector(Y1)), ncol=2, byrow = F)
    df_wt_type_h1$Y2[df_wt_type_h1$wt_type==wt_type_vec[k] & df_wt_type_h1$id==i] <- Y2_mat %*% beta_true+Y2_err
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # 10 minutes

# save data
save(df_wt_type_h1, file = here("Data/sim_H1_wt_type.RData"))
