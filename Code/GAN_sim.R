
#### Set up ####
library(neurobase) # https://cran.r-project.org/web/packages/neurobase/index.html
library(tidyverse)
library(here)
#library(keras)
#library(abind)
library(RGAN) # https://github.com/mneunhoe/RGAN/blob/main/README.md
library(torch)


#### Load MS leasion data ####
# use the CSV file as a guide
paths <- read.csv(here("DataRaw/WM_lesion_QC_result.csv"))
# 82 scans
# 8 subjects
# 4 sites
# two scans each sites

#### Read processed image and segmentation ####
# for intensity: use whitestripe/FLAIR_space/t1_n4_brain_reg_flair_ws.nii.gz
# for segmentation: use minosa/mimosa_mask.nii.gz

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
                       "/whitestripe/FLAIR_space/t1_n4_brain_reg_flair_ws.nii.gz"),
    seg_path =  paste0("DataRaw/processed/data/sub-", subject, "/ses-", session, 
                       "/", mimosa, "/", name2)
  ) 

# example with the MS lesion scan
# for the sake of time, I'll start with only one subject
paths <- paths %>% filter(subject=="01001")

img_list <- list()

for(p in 1:nrow(paths)){
  
  img_p <- readNIfTI(here(paths$file_path[p]), reorient = F)
  # seg_p <- readNIfTI(here(paths$seg_path[p]), reorient = F)
  # don't need segmentation information for simulation?
  
  img_list[[p]] <- img_p
  
}


  
#### Generate Image ####


# Also for the sake of time, play with 2 images first
ortho2(x=img_list[[1]], crosshair=FALSE)
ortho2(x=img_list[[2]], crosshair=FALSE)


# First, standardize
img_mat <- matrix(NA, nrow = 176*256*256, ncol = length(img_list[1:2]))

for(c in 1:ncol(img_mat)){
  img_mat[, c] <- as.vector(img_list[[p]]@.Data)
}


head(img_mat)

## Use a new data transformer.
transformer <- data_transformer$new()
## Fit the transformer to your data.
transformer$fit(img_mat)
## Use the fitted transformer to transform your data.
df_img_norm <- transformer$transform(img_mat)
head(df_img_norm)

# Have a look at the transformed data.
hist(df_img_norm[, 1], 30)
hist(df_img_norm[, 2], 30)

# Set the device you want to train on.
# First, we check whether a compatible GPU is available for computation.
use_cuda <- torch::cuda_is_available()

# If so we would use it to speed up training (especially for models with image data).
device <- ifelse(use_cuda, "cuda", "cpu")

# Now train the GAN and observe some intermediate results.
res <-
  gan_trainer(
    df_img_norm,
    eval_dropout = TRUE,
    plot_progress = TRUE,
    plot_interval = 600,
    device = device
  )




# After training you can work with the resulting GAN to sample synthetic data
# or potentially keep training for further steps.

# If you want to sample synthetic data from your GAN 
# you need to provide a GAN Generator and a noise vector 
# (that needs to be a torch tensor and should come from the same distribution that you used during training). 
# For example, we could look at the difference of synthetic data generated 
# with and without dropout during generation/inference
# (using the same noise vector).

par(mfrow = c(1, 2))

# Set the noise vector.
noise_vector <- torch::torch_randn(c(nrow(transformed_data), 2))$to(device = device)

# Generate synthetic data from the trained generator with dropout during generation.
synth_data_dropout <- expert_sample_synthetic_data(res$generator, noise_vector,eval_dropout = TRUE)

# Plot data and synthetic data
GAN_update_plot(data = transformed_data, synth_data = synth_data_dropout, main = "With dropout")

synth_data_no_dropout <- expert_sample_synthetic_data(res$generator, noise_vector,eval_dropout = F)

GAN_update_plot(data = transformed_data, synth_data = synth_data_no_dropout, main = "Without dropout")


# If you want to continue training you can pass the generator,
# discriminator as well as the respective optimizers to gan_trainer like that:
  
res_cont <- gan_trainer(transformed_data,
                          generator = res$generator,
                          discriminator = res$discriminator,
                          generator_optimizer = res$generator_optimizer,
                          discriminator_optimizer = res$discriminator_optimizer,
                          epochs = 10
  )



