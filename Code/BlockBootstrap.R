# This script implements Block Bootstrap on simulated data
# Using function from BlockBootFunc.R
# And datafrom GenDataH0.R

#### Set up ####

source(here("Code/BlockBootFunc.R"))

# for computational time, reduce the sample size
N <- 100
# number of bootstrap iteration
M <- 1000 # number of bootstrap iter
# block size to explore
max_size <- 9
# image size 
nS <- 32

#### H0, filter size #####
df_ksize <- df_ksize %>% filter(id %in% 1:N)
ksize_vec <- unique(df_ksize$ksize)

# containers
slope_est <- array(NA, dim = c(length(ksize_vec), max_size, N, M))

# bootstrap
pb <- txtProgressBar(0, length(ksize_vec)*max_size*N, style = 3)
ct <- 0
t1 <- Sys.time()

for(k in seq_along(ksize_vec)){ # filter size for data generation
  
  for(b in 1:max_size){ # block size for bootstrap
    
    for(i in 1:N){ # for each subject
      
      this_df <- df_ksize %>% filter(ksize==ksize_vec[k] & id==i)
      slope_est[k, b, i, ] <- BlockBoot(this_df, b, 1000, 32)
        
      ct <- ct+1
      setTxtProgressBar(pb, ct)
      
    }
  }
}
t2 <- Sys.time()


# save data
slope_est_ksize <- slope_est
save(slope_est_ksize, file = here("Data/block_boot_H0_ksize.RData"))

#### H0, exponential weight #####
df_expwt <- df_expwt %>% filter(id %in% 1:N)
alpha_vec <- unique(df_expwt$alpha)

# containers
slope_est_expwt <- array(NA, dim = c(length(alpha_vec), max_size, N, M))

# bootstrap
pb <- txtProgressBar(0, length(alpha_vec)*max_size*N, style = 3)
ct <- 0
t1 <- Sys.time()

for(k in seq_along(alpha_vec)){ # filter size for data generation
  
  for(b in 1:max_size){ # block size for bootstrap
    
    for(i in 1:N){ # for each subject
      
      this_df <- df_expwt %>% filter(alpha==alpha_vec[k] & id==i)
      slope_est_expwt[k, b, i, ] <- BlockBoot(this_df, b, 1000, 32)
      
      ct <- ct+1
      setTxtProgressBar(pb, ct)
      
    }
  }
}
t2 <- Sys.time()
close(pb)

t2-t1 # almost 2 hours


# save data
save(slope_est_expwt, file = here("Data/block_boot_H0_expwt.RData"))

#### H0, weight type #####
df_wt_type <- df_wt_type %>% filter(id %in% 1:N)
wt_type_vec <- unique(df_wt_type$wt_type)

# containers
slope_est_wt_type <- array(NA, dim = c(length(wt_type_vec), max_size, N, M))

# bootstrap
pb <- txtProgressBar(0, length(wt_type_vec)*max_size*N, style = 3)
ct <- 0
t1 <- Sys.time()

for(k in seq_along(wt_type_vec)){ # filter size for data generation
  
  for(b in 1:max_size){ # block size for bootstrap
    
    for(i in 1:N){ # for each subject
      
      this_df <- df_wt_type %>% filter(wt_type==wt_type_vec[k] & id==i)
      slope_est_wt_type[k, b, i, ] <- BlockBoot(this_df, b, 1000, 32)
      
      ct <- ct+1
      setTxtProgressBar(pb, ct)
      
    }
  }
}
t2 <- Sys.time()
close(pb)

t2-t1 # almost 2 hours


# save data
save(slope_est_wt_type, file = here("Data/block_boot_H0_wt_type.RData"))


#### H1, filter size ####

load(here("Data/sim_H1_ksize.RData"))
df_ksize_h1 <- df_ksize_h1 %>% filter(id %in% 1:N)
ksize_vec <- unique(df_ksize_h1$ksize)

# containers
slope_est_ksize_h1 <- array(NA, dim = c(length(ksize_vec), max_size, N, M))

# bootstrap
pb <- txtProgressBar(0, length(ksize_vec)*max_size*N, style = 3)
ct <- 0
t1 <- Sys.time()

for(k in seq_along(ksize_vec)){ # filter size for data generation
  
  for(b in 1:max_size){ # block size for bootstrap
    
    for(i in 1:N){ # for each subject
      
      this_df <- df_ksize_h1 %>% filter(ksize==ksize_vec[k] & id==i)
      slope_est_ksize_h1[k, b, i, ] <- BlockBoot(this_df, b, 1000, 32)
      
      ct <- ct+1
      setTxtProgressBar(pb, ct)
      
    }
  }
}
t2 <- Sys.time()

t2-t1 # 1.7 hours


save(slope_est_ksize_h1, file = here("Data/block_boot_H1_ksize.RData"))

#### H1, exponential weight ####

load(here("Data/sim_H1_expwt.RData"))
df_expwt_h1 <- df_expwt_h1 %>% filter(id %in% 1:N)
alpha_vec <- unique(df_expwt_h1$alpha)

# containers
slope_est_expwt_h1 <- array(NA, dim = c(length(alpha_vec), max_size, N, M))

# bootstrap
pb <- txtProgressBar(0, length(alpha_vec)*max_size*N, style = 3)
ct <- 0
t1 <- Sys.time()

for(k in seq_along(alpha_vec)){ # filter size for data generation
  
  for(b in 1:max_size){ # block size for bootstrap
    
    for(i in 1:N){ # for each subject
      
      this_df <- df_expwt_h1 %>% filter(alpha==alpha_vec[k] & id==i)
      slope_est_expwt_h1[k, b, i, ] <- BlockBoot(this_df, b, 1000, 32)
      
      ct <- ct+1
      setTxtProgressBar(pb, ct)
      
    }
  }
}
t2 <- Sys.time()

t2-t1 # 1.8 hours


save(slope_est_expwt_h1, file = here("Data/block_boot_H1_expwt.RData"))


#### H1, weight shape ####

load(here("Data/sim_H1_wt_type.RData"))
df_wt_type_h1 <- df_wt_type_h1 %>% filter(id %in% 1:N)
wt_type_vec <- unique(df_wt_type_h1$wt_type)

# containers
slope_est_wttype_h1 <- array(NA, dim = c(length(wt_type_vec), max_size, N, M))

# bootstrap
pb <- txtProgressBar(0, length(wt_type_vec)*max_size*N, style = 3)
ct <- 0
t1 <- Sys.time()

for(k in seq_along(wt_type_vec)){ # filter size for data generation
  
  for(b in 1:max_size){ # block size for bootstrap
    
    for(i in 1:N){ # for each subject
      
      this_df <- df_wt_type_h1 %>% filter(wt_type==wt_type_vec[k] & id==i)
      slope_est_wttype_h1[k, b, i, ] <- BlockBoot(this_df, b, 1000, 32)
      
      ct <- ct+1
      setTxtProgressBar(pb, ct)
      
    }
  }
}
t2 <- Sys.time()

t2-t1 # 1.7 hours


save(slope_est_wttype_h1, file = here("Data/block_boot_H1_wt_type.RData"))
