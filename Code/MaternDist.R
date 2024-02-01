# This script explores Matern family distribution

library(spaMM)
library(fields)
library(geoR)
library(tidyverse)


#### Documentation ezample ####

d<- seq(0,5,,200) # distance

#  effect of smooth parameter 
sm<- seq( .5, 8,,5) # smooth parameter
temp<- matrix(NA, 200, 5)

for(k in 1:5){
  temp[,k] <- matern(d, phi=0.25, kappa=sm[k])
}
plot(d, temp[, 1], type = "l")
lines(d, temp[, 2], col=2)
lines(d, temp[, 3], col=3)
lines(d, temp[, 4], col=4)
lines(d, temp[, 5], col=5)

# effect of range parameter 
rg<- seq(0.2, 1, by = 0.2) # smooth parameter
temp<- matrix(NA, 200, 5)

for(k in 1:5){
  temp[,k] <- matern(d, phi=rg[k], kappa=0.5)
}
plot(d, temp[, 1], type = "l")
lines(d, temp[, 2], col=2)
lines(d, temp[, 3], col=3)
lines(d, temp[, 4], col=4)
lines(d, temp[, 5], col=5)

matplot( d, temp, type="l", lty=1)

#### 2D coordinate example ####
s1 <- s2 <- seq(-1, 1, by = 0.1)
coords <- expand.grid(s1, s2) %>%
  mutate(space_id = 1:(length(s1)*length(s2)))

dist <- expand.grid(coords, coords)


cov <- matern.image.cov(ind1 = coords, ind2 = coords, 
                        )


# Matern covariance matrix ( marginal variance =1) for the ozone
# locations 
out<- matern.cov( ozone$x, theta=100, smoothness=1.0)
# out is a 20X20 matrix

out2<- matern.cov( ozone$x[6:20,],ozone$x[1:2,], theta=100, 
                   smoothness=1.0)

# out2 is 15X2 cross covariance matrix 

# Kriging fit using a Matern covariance and where the nugget  and 
# sill variances are found by GCV 