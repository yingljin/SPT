# This script generate data using a weighted average filter

##### Set-up #####

alpha_vec <- c(0, 0.5, 1, 2.5, 5) # weight matrix
stride_vec <- seq(0, 8, by = 2)

nS <- 32 # space grid
Nf <- 1000 # generate 1000 subjects over all, but use only 100 for the block bootstrap

## block bootstrap
max_size <- 9 # max block
M <- 1000 # number of bootstrap iter

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
  
  for(k in seq_along(ksize_vec)){ # fix a time point
    
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


#### Null Hypothesis, Weight ####

#### Null Hypothesis, Weight type ####

#### Different weight matrix type ####
# types of weight matrices to look into
wt_type_vec <- c("wt_inc", "wt_dec", "wt_u", "wt_sin", "wt_cos")

df_subj3 <- expand_grid(wt_type = wt_type_vec, id=1:Nf, s2=1:nS, s1 = 1:nS)
df_subj3$Y1 <- df_subj3$Y2 <- NA



pb <- txtProgressBar(min=0, max=Nf, style = 3)

t1 <- Sys.time()
for(i in 1:Nf){ # fix a subject
  
  for(k in seq_along(wt_type_vec)){ # fix a type of weight
    
    # weight matrix
    this_wt <- matrix(as_vector(df_wt_type[, wt_type_vec[k]]), 5, 5)
    
    # generate Y1
    MAerr_mat1 <- MWA_rand_field2(kf = 5, ki = 32, wt = this_wt)
    df_subj3$Y1[df_subj3$id==i & df_subj3$wt_type==wt_type_vec[k]] <- as.vector(MAerr_mat1)
    
    # generate Y1
    MAerr_mat2 <- MWA_rand_field2(kf = 5, ki = 32, wt = this_wt)
    df_subj3$Y2[df_subj3$id==i & df_subj3$wt_type==wt_type_vec[k]] <- as.vector(MAerr_mat2)
  }
  
  setTxtProgressBar(pb, i)
}
t2 <- Sys.time()

close(pb)

t2-t1 # about 8 minutes







df_subj3 %>% 
  filter(id==1) %>%
  pivot_longer(starts_with("Y")) %>%
  ggplot()+
  geom_tile(aes(x=s1, y=s2, fill = value))+
  facet_grid(cols=vars(wt_type), rows = vars(name))+
  labs(title = "Generated data of subject ID = 15")

