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


#### Elizabeth code ####
dataDir = '/Users/jinyin/Desktop/SPT/DataRaw/data'
subs = list.dirs(dataDir, full.names = F, recursive = F)
subs = subs[grepl("sub", subs)] # folder names of subjects

# list the sites for each subject
site = lapply(subs, function(sub) {
  list.dirs(paste0(dataDir, '/', sub), full.names = F, recursive = F)
})

# subset session 1
ses1 = lapply(site, function(s1) {
  s1[grepl("_01$", s1)]
})

# subset session 2
ses2 = lapply(site, function(s2) {
  s2[grepl("_02$", s2)]
})

# get flair and mimosa mask for each sub and site for session 1
ses1_files = lapply(seq_along(subs), function(sub) {
  lapply(seq_along(ses1[[sub]]), function(ses) {
    # get flair files
    flair_files = list.files(paste0(dataDir, '/', subs[sub], '/', ses1[[sub]][ses], '/whitestripe/FLAIR_space'), full.names = TRUE, recursive = FALSE)
    flair_files = flair_files[grepl(pattern = 'flair_n4_brain_ses_ws', flair_files)]
    
    # get T1 files
    t1_files = list.files(paste0(dataDir, '/', subs[sub], '/', ses1[[sub]][ses], '/whitestripe/FLAIR_space'), full.names = TRUE, recursive = FALSE)
    t1_files = t1_files[grepl(pattern = 't1', t1_files)]
    
    # get mimosa mask files
    mimosa_files = list.files(paste0(dataDir, '/', subs[sub], '/', ses1[[sub]][ses], '/mimosa'), full.names = TRUE, recursive = FALSE)
    mimosa_files = mimosa_files[grepl(pattern = 'mimosa_mask.nii.gz', mimosa_files)]
    
    # list both flair and mimosa files
    list(flair = flair_files, t1 = t1_files, mimosa = mimosa_files)
  })
})


# get flair and mimosa mask for each sub and site for session 2
ses2_files = lapply(seq_along(subs), function(sub) {
  lapply(seq_along(ses2[[sub]]), function(ses) {
    # get flair files
    flair_files = list.files(paste0(dataDir, '/', subs[sub], '/', ses2[[sub]][ses], '/whitestripe/FLAIR_space'), full.names = TRUE, recursive = FALSE)
    flair_files = flair_files[grepl(pattern = 'flair_n4_brain_ses_ws', flair_files)]
    
    # get T1 files
    t1_files = list.files(paste0(dataDir, '/', subs[sub], '/', ses2[[sub]][ses], '/whitestripe/FLAIR_space'), full.names = TRUE, recursive = FALSE)
    t1_files = t1_files[grepl(pattern = 't1', t1_files)]
    
    # get mimosa mask files
    mimosa_files = list.files(paste0(dataDir, '/', subs[sub], '/', ses2[[sub]][ses], '/mimosa'), full.names = TRUE, recursive = FALSE)
    mimosa_files = mimosa_files[grepl(pattern = 'mimosa_mask.nii.gz', mimosa_files)]
    
    # list both flair and mimosa files
    list(flair = flair_files, t1 = t1_files, mimosa = mimosa_files)
  })
})

# Function to process a pair of mimosa and flair files
process_files = function(mimosa_file, flair_file, t1_file) {
  # Read in the MIMOSA segmentation files
  mimosa = readNIfTI(mimosa_file, reorient = FALSE)
  
  # Create a unique voxel identifier
  index = niftiarr(mimosa, 1:c(dim(mimosa)[1] * dim(mimosa)[2] * dim(mimosa)[3]))
  
  # Read in flair images
  flair = readNIfTI(flair_file, reorient = FALSE) 
  
  # Read in T1 images
  t1 = readNIfTI(t1_file, reorient = FALSE) 
  
  # Use when looking at lesion voxels only
  label = mimosa[mimosa != 0] # Get the lesion labels; find all the lesion voxels
  flair_intensities = flair[mimosa != 0] # Image intensities
  t1_intensities = t1[mimosa != 0] # Image intensities
  voxel = index[mimosa != 0] # Voxel identifier
  
  # Use when looking at whole brain
  #label = mimosa[mimosa == 0 | mimosa != 0] 
  #intensities = flair[mimosa == 0 | mimosa != 0] 
  #voxel = index[mimosa == 0 | mimosa != 0] 
  
  # Create a data frame with the pulled data
  pulled_data = data.frame(
    voxel = voxel,
    label = label, # Lesion label
    flair_intensity = flair_intensities, # Image intensities
    t1_intensity = t1_intensities # Image intensities
  )
  
  # Get the x, y, z coordinates corresponding to each voxel
  res = which(index@.Data == index, arr.ind = TRUE) # `res` is a matrix 
  tt = data.frame(res, voxel = index[res]) %>% 
    rename(x = "dim1", y = "dim2", z = "dim3")
  
  # Join intensity data with x, y, z coordinates to create final data
  final_result = pulled_data %>% 
    left_join(tt, by = "voxel")
  
  return(final_result)
}


# Process each sub and site for session 1
results_ses1 = lapply(seq_along(subs), function(sub) {
  lapply(seq_along(ses1[[sub]]), function(ses) {
    process_files(ses1_files[[sub]][[ses]]$mimosa, ses1_files[[sub]][[ses]]$flair, ses1_files[[sub]][[ses]]$t1)
  })
})

# Process each sub and site for session 2
results_ses2 = lapply(seq_along(subs), function(sub) {
  lapply(seq_along(ses2[[sub]]), function(ses) {
    process_files(ses2_files[[sub]][[ses]]$mimosa, ses2_files[[sub]][[ses]]$flair, ses2_files[[sub]][[ses]]$t1)
    # if (!is_empty(ses2_files[[sub]][[ses]]$mimosa) & !is_empty(ses2_files[[sub]][[ses]]$flair) & !is_empty(ses2_files[[sub]][[ses]]$t1)) {
    #   process_files(ses2_files[[sub]][[ses]]$mimosa, ses2_files[[sub]][[ses]]$flair, ses2_files[[sub]][[ses]]$t1)
    # } else {
    #   names = c('voxel', 'label', 'flair_intensity2', 't1_intensity2', 'x', 'y', 'z')
    #   df = data.frame(matrix(ncol = length(names), nrow = 0))
    #   colnames(df) = names
    #   data.frame(NA)
    # }
  })
})

# Rename 'flair_intensity' in results_ses1 to 'flair_intensity1'
results_ses1 = lapply(results_ses1, function(sub_list) {
  lapply(sub_list, function(df) {
    names(df)[names(df) == "flair_intensity"] <- "flair_intensity1"
    names(df)[names(df) == "t1_intensity"] <- "t1_intensity1"
    return(df)
  })
})

# Rename 'flair_intensity' in results_ses2 to 'flair_intensity2'
results_ses2 = lapply(results_ses2, function(sub_list) {
  lapply(sub_list, function(df) {
    names(df)[names(df) == "flair_intensity"] <- "flair_intensity2"
    names(df)[names(df) == "t1_intensity"] <- "t1_intensity2"
    return(df)
  })
})

dim(results_ses1[[1]][[1]])
dim(results_ses1[[2]][[1]])

save(results_ses1, results_ses2, file = here("DataOutput/registered.RData"))
