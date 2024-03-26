
# This script writes the function for simple block bootstrap
# The boostrap statistics is the slope from simple linear model

# parameters:
## df: a dataframe containing spatial index and the two outcomes to be tested (Y1, Y2)
## bsize: block size. We only use square block for now
## M: number of boostrap iteration
## nS: image size

# Notes: blocks are sampled with equal probablity
#       so that the expectated size of sampled image is the same with the original image

BlockBoot <- function(df, bsize, M=1000, nS = 32){
  
    # divide the matrix into blocks
    img <- matrix(df$Y2, nS, nS)
    rblock <- (row(img)-1)%/%bsize+1
    cblock <- (col(img)-1)%/%bsize+1
    block_id_mat <- (rblock-1)*max(cblock) + cblock
    nblock <- max(block_id_mat) # number of blocks
    
    # sample blocks
    # sample the same number of blocks as the original image
    df$block_id <- as.vector(block_id_mat)
    block_list <- split(df, f = df$block_id)
    
    ## container
    slope_est <- rep(NA, M)
    
    # bootstrap
    for(m in 1:M){
      boot_block <- sample(1:nblock, size = nblock, replace = T)
      boot_df <- bind_rows(block_list[boot_block])
      # fit model
      boot_lm <-  summary(lm(Y2~Y1, data = boot_df))$coefficients
      slope_est[m] <- boot_lm["Y1", "Estimate"]
      
    }

  return(slope_est)
}