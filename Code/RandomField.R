# This script saves a function 
# for a 2D moving-average square filter of white noise
# and edges are filled with zero padding

# k: filter size
# Zmat: white noise image

MA_rand_field <- function(k, Zmat){
  
  s1 <- nrow(Zmat)
  s2 <- ncol(Zmat)
  
  
  psize <- (k-1)/2 # padding size 
  # add zero padding
  pad_mat <- cbind(matrix(0, nrow = s1, ncol = psize),
                   Zmat, 
                   matrix(0, nrow = nrow(Zmat), ncol = psize)) # pad columns
  pad_mat <- rbind(matrix(0, nrow = psize, ncol = ncol(pad_mat)),
                   pad_mat, 
                   matrix(0, nrow = psize, ncol = ncol(pad_mat)))
  
  # moving average
  ma_mat <- matrix(NA, s1, s2)
  
  for(i in 1:s1){
    for(j in 1:s2){ 
      
      ma_mat[i, j] <- mean(pad_mat[i:(i+k-1), j:(j+k-1)])
      
    }
  }
  
  return(ma_mat)
  
}
