# Simulate lesion images
# https://hopstat.wordpress.com/2014/06/17/fslr-an-r-package-interfacing-with-fsl-for-neuroimaging-analysis/
# https://cran.r-project.org/web/packages/oro.nifti/oro.nifti.pdf
# https://cran.r-project.org/web/packages/RGAN/RGAN.pdf


library(oro.nifti)
library(fslr)
library(here)

img1 <- readNIfTI("Data/defaced/sub-01001/ses-Hopkins_01/anat/sub-01001_Hopkins_01_T1w.nii")
img2 <- readNIfTI("Data/defaced/sub-01001/ses-Hopkins_01/anat/sub-01001_Hopkins_01_T2w.nii")


#### Explore data ####

hist(img1)
hist(img2)

orthographic(img1) 
orthographic(img2)

# looks like the subject had a lot of changes between the two scans? 


#### GAN ####

