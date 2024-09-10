# Simulate lesion images
# references: 
# https://hopstat.wordpress.com/2014/06/17/fslr-an-r-package-interfacing-with-fsl-for-neuroimaging-analysis/
# https://cran.r-project.org/web/packages/oro.nifti/oro.nifti.pdf
# https://cran.r-project.org/web/packages/RGAN/RGAN.pdf

#### Set up ####

library(neurobase) # https://cran.r-project.org/web/packages/neurobase/index.html
library(tidyverse)
library(here)
#library(keras)
#library(abind)
library(RGAN)
library(torch)

#### File paths ####
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


# add index for site and scan
paths <- paths %>% 
  select(subject, session, file_path, seg_path) %>%
  separate(col=session, sep = "_", into = c("site", "scan"), remove = F) %>%
  distinct(.) # duplicate scans

#### Data Process ####
# I am trying to put all the lesion data in to a long data.frame
df_scans <- data.frame(id = NA, site = NA, scan = NA, pix = NA, seg = NA)

for(p in 1:nrow(paths)){
  
  img_p <- readNIfTI(here(paths$file_path[p]), reorient = F)
  seg_p <- readNIfTI(here(paths$seg_path[p]), reorient = F)
  
  df_p <- data.frame(id = paths$subject[p],
                     site = paths$site[p], 
                     scan = paths$scan[p],
                     pix = as.vector(img_p@.Data),
                     seg = as.vector(seg_p@.Data))
  
  df_scans <- df_scans %>% add_row(df_p)
    
}

##### Check #####
table(df_scans$id)

# remove the first empty row
df_scans <- df_scans %>% filter(complete.cases(.))
head(df_scans)  

save(df_scans, file = here("DataOutput/df_scans.RData"))


