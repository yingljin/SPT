# Simulate lesion images
# https://hopstat.wordpress.com/2014/06/17/fslr-an-r-package-interfacing-with-fsl-for-neuroimaging-analysis/
# https://cran.r-project.org/web/packages/oro.nifti/oro.nifti.pdf
# https://cran.r-project.org/web/packages/RGAN/RGAN.pdf

#### Set up ####

# library(oro.nifti) # Whitcher2011
library(neurobase) # https://cran.r-project.org/web/packages/neurobase/index.html
library(tidyverse)
library(here)
#library(keras)
#library(abind)
library(RGAN)
library(torch)

#### File paths ####
# list.files("DataRaw")
paths <- read.csv(here("DataRaw/WM_lesion_QC_result.csv"))
# 82 scans
# 8 subjects
# 4 sites
# two scans each sites

#### Read processed image and segmentation ####
paths <- paths %>% 
  separate(., col=flair_files, 
           into = c(rep(NA, 8), "registration", "flair", "name1"),
           sep = "/") %>% 
  separate(., col=mimosa_files, 
           into = c(rep(NA, 8), "mimosa", "name2"),
           sep = "/") 

paths <- paths %>%
  mutate(subject = gsub("-", "", subject)) %>%
  mutate(
  file_path = paste0("DataRaw/processed/data/sub-", subject, "/ses-", session, 
                     "/", registration, "/", flair, "/", name1),
  seg_path =  paste0("DataRaw/processed/data/sub-", subject, "/ses-", session, 
                     "/", mimosa, "/", name2)
) 

# let's use one subject as an example 
paths_01001 <- paths %>% filter(subject=="01001") 

# reading in and display
list_mat <- list()
list_img <- list()
list_seg_img <- list()
list_seg_mat <- list()
for(p in seq_along(paths_01001$file_path)){
  
  # image
  img_p <- readNIfTI(paths_01001$file_path[p], reorient = F)
  list_img[[p]] <- img_p
  list_mat[[p]] <- img_p@.Data
  # segmentation
  seg_p <- readNIfTI(paths_01001$seg_path[p], reorient = F)
  list_seg_img[[p]] <- seg_p
  list_seg_mat[[p]] <- seg_p@.Data
}

#### Plot ####
# whole image
par(mfrow = c(1, 2))
ortho2(x=list_img[[1]], y=list_seg_img[[1]], crosshair=FALSE)
ortho2(x=list_img[[2]], y=list_seg_img[[2]], crosshair=FALSE)
hist(list_img[[1]])
hist(list_img[[2]])

# only lesion image
l1 <- list_img[[1]][list_seg_img[[1]]==1]
l2 <- list_img[[2]][list_seg_img[[2]]==1]
dim(l1)

image(list_seg_img[[1]], plot.type = "single", z=128)

mask = ifelse(list_seg_img[[1]]==1, 1, NA)
overlay(list_img[[1]], y = mask, plot.type="single", z=128)




summary(list_img[[1]]@.Data)

lapply(list_01001, dim)

img1 <- readNIfTI(paths_01001$file_path[1], reorient = F)
img1
aux_file(img1)
descrip(img1)
image(img1, plot.type="single", z=100) # plot a slice 
orthographic(img1) 


#### Exploratory ####



# I think I know too little about image analysis
# That I couldn't identify what to do just by looking at the images

# reading in and display
img1 <- readNIfTI("Data/mscamras/sub-01001_Hopkins_01_T1w.nii")
img1
aux_file(img1)
descrip(img1)
image(img1, plot.type="single", z=210) # plot a slice 
orthographic(img1) 

# how is data stored
slotNames(img1)
## array 
imgX <- img1@.Data
class(imgX)
dim(imgX)
imgX[100:104, 100:104, 100:104]
max(imgX)
min(imgX)
hist(imgX, breaks = 50)

#### Load data ####

# based on how they store data, how can I loop over all files? 
# Also unzip them? 
# let try on subject 01001
fileNames <- list.files("Data/mscamras")

img_01001 <- list()

for(i in seq_along(fileNames)){
  file_i <- paste0("Data/mscamras/", fileNames[i])
  img_i <- tryCatch(expr = {readNIfTI(fname = file_i, reorient = FALSE)},
    error = function(e){return(NA)})
  img_01001[[i]] <- img_i@.Data

}

names(img_01001) <- fileNames
img_01001 # only the hopkins scanes can be read in? 

orthographic(img_01001[[2]]) 
img1@.Data


# looks like the subject had a lot of changes between the two scans? 

# So I can read in the data, and plot histograms
# How can I train a GAN model then? 

#### Toy data ####

# Sample some toy data to play with.
data <- sample_toydata()

# Transform (here standardize) the data to facilitate learning.
# First, create a new data transformer.
# can I transform my data?
transformer <- data_transformer$new()

# Fit the transformer to your data.
transformer$fit(imgX)
transformer
# Use the fitted transformer to transform your data.
transformed_data <- transformer$transform(data)

# Have a look at the transformed data.
par(mfrow = c(3, 2))
plot(
  transformed_data,
  bty = "n",
  col = viridis::viridis(2, alpha = 0.7)[1],
  pch = 19,
  xlab = "Var 1",
  ylab = "Var 2",
  main = "The Real Data",
  las = 1
)

# Set the device you want to train on.
# First, we check whether a compatible GPU is available for computation.
use_cuda <- torch::cuda_is_available()

# If so we would use it to speed up training (especially for models with image data).
device <- ifelse(use_cuda, "cuda", "cpu")

# Now train the GAN and observe some intermediate results.
res <-
  gan_trainer(
    transformed_data,
    eval_dropout = TRUE,
    plot_progress = TRUE,
    plot_interval = 600,
    device = device
  )
#> Training the GAN ■■                                 3% | ETA:  1m
#> Training the GAN ■■                                 5% | ETA:  1m
#> Training the GAN ■■■■                              10% | ETA:  1m
#> Training the GAN ■■■■■■                            16% | ETA: 48s
#> Training the GAN ■■■■■■■                           21% | ETA: 45s
#> Training the GAN ■■■■■■■■■                         26% | ETA: 42s
#> Training the GAN ■■■■■■■■■■                        32% | ETA: 39s
#> Training the GAN ■■■■■■■■■■■■                      37% | ETA: 36s
#> Training the GAN ■■■■■■■■■■■■■■                    42% | ETA: 32s
#> Training the GAN ■■■■■■■■■■■■■■■                   48% | ETA: 30s
#> Training the GAN ■■■■■■■■■■■■■■■■■                 53% | ETA: 27s
#> Training the GAN ■■■■■■■■■■■■■■■■■■                58% | ETA: 24s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 21s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■             68% | ETA: 19s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■■■           73% | ETA: 16s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA: 13s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA: 10s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■■■■■■■       88% | ETA:  7s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     93% | ETA:  4s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    98% | ETA:  1s
#> Training the GAN ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s


#### GAN ####

generate_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 256, input_shape = c(100)) %>% 
    layer_leaky_relu(0.2) %>% 
    layer_dense(units = 512) %>% 
    layer_leaky_relu(0.2) %>% 
    layer_dense(units = 1024) %>% 
    layer_leaky_relu(0.2) %>% 
    layer_dense(units = 784, activation = 'tanh')
  
  return(model)
}

discriminate_model <- function(){
  model <- keras_model_sequential() %>% 
    layer_dense(units = 1024, input_shape = c(784)) %>% 
    layer_leaky_relu(0.2) %>% 
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 512) %>% 
    layer_leaky_relu(0.2) %>% 
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 256) %>% 
    layer_leaky_relu(0.2) %>% 
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 1, activation = 'sigmoid')
  
  return(model)
}

