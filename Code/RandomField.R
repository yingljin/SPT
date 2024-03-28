# This script saves function for a 2D moving-average square filter of white noise

##### Simple average #####

# kf: filter size (odd number)
# ki: image size
# let's say both filter and image are squares at this point
# Also, let's use only odd-size filters for now (borrowed from image analysis). 
#   It has a center pixel which makes operation convenient 

MA_rand_field <- function(kf, ki, step_size = 1){

  # generate a white noise image
  # notice that this image is larger than the target image
  # so that there is no need for padding
  kz <- ki+2*(kf%/%2) # size of the white noise 
  Zmat <- matrix(rnorm(kz^2, 0, 1), kz, kz)
  
  # step index
  step_id <- seq(1, ki, by = step_size)
  
  # moving average
  ma_mat <- matrix(NA, ki, ki)
  for(i in 1:ki){
    for(j in 1:ki){ 
      # simple zverage
      ma_mat[i, j] <- mean(Zmat[i:(i+2*(kf%/%2)), j:(j+2*(kf%/%2))])
      
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
      ma_mat[i, j] <- sum(Zmat[i:(i+2*(kf%/%2)), j:(j+2*(kf%/%2))]*wt)
      
    }
  }
  
  return(ma_mat)
  
}


#### Step size ####

# I'd like to write a function that moves the filter with varying step size
# kf: filter size
# ki: result image size
# stride: step size when moving filter
# pad: padding (edges) size

MA_rand_field_step <- function(kf, ki, stride){
  
  # generate a white noise image
  # notice that this image is larger than the target image
  # so that there is no need for padding
  # size of the white noise 
  kz <- (ki-1)*(stride+1)+kf
  Zmat <- matrix(rnorm(kz^2, 0, 1), kz, kz)
  
  # moving average
  ma_mat <- matrix(NA, ki, ki)
  for(i in 1:ki){
    for(j in 1:ki){ 
      # simple zverage
      start_i <- i+(i-1)*stride
      start_j <- j+(j-1)*stride
      ma_mat[i, j] <- mean(Zmat[start_i:(start_i+2*(kf%/%2)), start_j:(start_j+2*(kf%/%2))])
    }
  }
  
  return(ma_mat)
  
}