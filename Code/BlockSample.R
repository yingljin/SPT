# In this script, I'd like to write a function that sample blocks 
# from a 2D space

i <- tid <- 1
df_it <- df_subj %>% filter(id==i & t==this_t)

# outcome matrix
Y_mat <- df_it %>%
  select(sid1, sid2, Y2) %>%
  pivot_wider(values_from = Y2, names_from = sid2) %>%
  select(-sid1)
Y_mat <- as.matrix(Y_mat)

# assign block
bsize <- 4
nS%/%bsize

rblock <- (row(Y_mat)-1)%/%bsize+1
cblock <- (col(Y_mat)-1)%/%bsize+1
block_id_mat <- (rblock-1)*max(cblock) + cblock
df_it$block_id <- as.vector(t(block_id_mat))
block_list <- split(df_it, f = df_it$block_id)
nblock <- max(block_id_mat)

# sample block
boot_block <- sample(1:nblock, size = nblock, replace = T)
boot_list <- block_list[boot_block]
boot_list <- bind_rows(boot_list)

matsplitter<-function(M, r, c) {
  rg <- (row(Y_mat)-1)%/%bsize+1
  cg <- (col(Y_mat)-1)%/%bsize+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(Y_mat))/bsize/bsize
  cv <- unlist(lapply(1:N, function(x) Y_mat[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
} 

data("BodyWeight")
