# This script saves function for a 2D moving-average square filter of white noise

##### Simple average #####

# kf: filter size (odd number)
# ki: image size
# let's say both filter and image are squares at this point
# Also, let's use only odd-size filters for now (borrowed from image analysis). 
#   It has a center pixel which makes operation convenient 

MA_rand_field <- function(kf, ki){

  # generate a white noise image
  # notice that this image is larger than the target image
  # so that there is no need for padding
  kz <- ki+2*(kf%/%2) # size of the white noise 
  Zmat <- matrix(rnorm(kz^2, 0, 1), kz, kz)
  
  # moving average
  ma_mat <- matrix(NA, ki, ki)
  for(i in 1:ki){
    for(j in 1:ki){ 
      # simple zverage
      ma_mat[i, j] <- mean(Zmat[j:(j+2*(kf%/%2)), i:(i+2*(kf%/%2))])
      
    }
  }
  
  return(ma_mat)
  
}

#### Wighted average #####

# kf: filter size (odd number)
# ki: image size
# wt: weight matrix

MWA_rand_field <- function(kf, ki, wt){
  
  # generate a white noise image
  # notice that this image is larger than the target image
  # so that there is no need for padding
  kz <- ki+2*(kf%/%2) # size of the white noise 
  Zmat <- matrix(rnorm(kz^2, 0, 1), kz, kz)
  
  # moving average
  ma_mat <- matrix(NA, ki, ki)
  for(i in 1:ki){
    for(j in 1:ki){ 
      # simple zverage
      ma_mat[i, j] <- sum(Zmat[j:(j+2*(kf%/%2)), i:(i+2*(kf%/%2))]*wt)
      
    }
  }
  
  return(ma_mat)
  
}