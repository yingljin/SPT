###################################################
### chunk number 1: preliminaries
###################################################
#line 94 "dicom_nifti3.Rnw"
library("oro.dicom")
library("bitops")
library("XML")
library("splines")
library("oro.nifti")
options(width=75)
options(prompt="R> ")


###################################################
### chunk number 2: DICOM Abdo 01
###################################################
#line 312 "dicom_nifti3.Rnw"
fname <- system.file(file.path("dcm", "Abdo.dcm"), package="oro.dicom")
abdo <- dicomInfo(fname)
names(abdo)
head(abdo$hdr)
tail(abdo$hdr) # [nrow(abdo$hdr)-4:0,]


###################################################
### chunk number 3: DICOM Abdo 02
###################################################
#line 330 "dicom_nifti3.Rnw"
extractHeader(abdo$hdr, "BitsAllocated")
extractHeader(abdo$hdr, "Rows")
extractHeader(abdo$hdr, "Columns")


###################################################
### chunk number 4: DICOM HK40 01
###################################################
#line 355 "dicom_nifti3.Rnw"
fname <- system.file("hk-40", package="oro.dicom")
hk40 <- dicomSeparate(fname, verbose=TRUE, counter=10)
unlist(lapply(hk40, length))


###################################################
### chunk number 5: DICOM HK40 02
###################################################
#line 377 "dicom_nifti3.Rnw"
hk40.info <- dicomTable(hk40$hdr)
write.csv(hk40.info, file="hk40_header.csv")
sliceloc.col <- which(hk40$hdr[[1]]$name == "SliceLocation") 
sliceLocation <- as.numeric(hk40.info[, sliceloc.col])
head(sliceLocation)
head(diff(sliceLocation))
unique(extractHeader(hk40$hdr, "SliceThickness"))


###################################################
### chunk number 6: DICOM HK40 03
###################################################
#line 395 "dicom_nifti3.Rnw"
head(extractHeader(hk40$hdr, "SliceLocation"))
modality <- extractHeader(hk40$hdr, "Modality", numeric=FALSE)
head(matchHeader(modality, "mr"))
(seriesTime <- extractHeader(hk40$hdr, "SeriesTime", numeric=FALSE))
str2time(seriesTime)


###################################################
### chunk number 7: abdo-png
###################################################
#line 415 "dicom_nifti3.Rnw"
png(filename="dicom_abdo.png", width=2*480, height=2*480, bg="black")
par(mar=rep(0,4))


###################################################
### chunk number 8: abdo-image
###################################################
#line 419 "dicom_nifti3.Rnw"
image(t(abdo$img), col=grey(0:64/64), axes=FALSE, xlab="", ylab="")


###################################################
### chunk number 9: abdo-dev.off
###################################################
#line 422 "dicom_nifti3.Rnw"
dev.off()
#par(mar=c(5,4,4,2)+0.1)


###################################################
### chunk number 10: DICOM Abdo 03
###################################################
#line 450 "dicom_nifti3.Rnw"
extractHeader(abdo$hdr, "Manufacturer", numeric=FALSE)
extractHeader(abdo$hdr, "RepetitionTime")
extractHeader(abdo$hdr, "EchoTime")


###################################################
### chunk number 11: DICOM Siemens 01
###################################################
#line 476 "dicom_nifti3.Rnw"
fname <- system.file(file.path("dcm", "MR-sonata-3D-as-Tile.dcm"),
                     package="oro.dicom")
dcm <- dicomInfo(fname)
dim(dcm$img)
dcmImage <- create3D(dcm, mosaic=TRUE)
dim(dcmImage)


###################################################
### chunk number 12: DICOM Siemens 02
###################################################
#line 485 "dicom_nifti3.Rnw"
dcmNifti <- dicom2nifti(dcm, mosaic=TRUE)
png(filename="dcmImage.png", width=2*480, height=2*480, bg="black")
image(t(dcm$img), col=grey(0:64/64), zlim=c(16, 1024), axes=FALSE, 
      xlab="", ylab="")
dev.off()
png(filename="dcmNifti.png", width=2*480, height=2*480, bg="black")
image(dcmNifti, zlim=c(16, 1024))
dev.off()


###################################################
### chunk number 13: mniLR_nifti
###################################################
#line 572 "dicom_nifti3.Rnw"
fname <- system.file(file.path("nifti", "mniLR.nii.gz"), package="oro.nifti")
(mniLR <- readNIfTI(fname))
aux.file(mniLR)
descrip(mniLR)


###################################################
### chunk number 14: mniLR-png
###################################################
#line 603 "dicom_nifti3.Rnw"
png(filename="mniLR.png", width=2*480, height=2*480, bg="black")


###################################################
### chunk number 15: mniLR-image
###################################################
#line 606 "dicom_nifti3.Rnw"
image(mniLR)


###################################################
### chunk number 16: mniLR-dev.off
###################################################
#line 609 "dicom_nifti3.Rnw"
dev.off()


###################################################
### chunk number 17: mniRL-read
###################################################
#line 646 "dicom_nifti3.Rnw"
fname <- system.file(file.path("nifti", "mniRL.nii.gz"), package="oro.nifti")
(mniRL <- readNIfTI(fname))


###################################################
### chunk number 18: mniRL-png
###################################################
#line 650 "dicom_nifti3.Rnw"
png(filename="mniRL.png", width=2*480, height=2*480, bg="black")


###################################################
### chunk number 19: mniRL-image
###################################################
#line 653 "dicom_nifti3.Rnw"
image(mniRL)


###################################################
### chunk number 20: mniRL-dev.off
###################################################
#line 655 "dicom_nifti3.Rnw"
dev.off()


###################################################
### chunk number 21: mniRL-ortho-png
###################################################
#line 685 "dicom_nifti3.Rnw"
png(filename="mniRL_orthographic.png", width=2*480, height=2*480, bg="black")


###################################################
### chunk number 22: mniRL-orthographic
###################################################
#line 688 "dicom_nifti3.Rnw"
orthographic(mniRL)


###################################################
### chunk number 23: mniRL-ortho-dev.off
###################################################
#line 691 "dicom_nifti3.Rnw"
dev.off()


###################################################
### chunk number 24: NIfTI-slots
###################################################
#line 730 "dicom_nifti3.Rnw"
slotNames(mniRL)
c(mniRL@"cal_min", mniRL@"cal_max")
range(mniRL)
mniRL@"datatype"
convert.datatype(mniRL@"datatype")


###################################################
### chunk number 25: NIfTI-constructor
###################################################
#line 763 "dicom_nifti3.Rnw"
n <- 100
(random.image <- nifti(array(runif(n*n), c(n,n,1))))
random.image@"dim_"
dim(random.image)


###################################################
### chunk number 26: NIfTI-write
###################################################
#line 782 "dicom_nifti3.Rnw"
writeNIfTI(random.image, "random")
list.files(pattern="random")


###################################################
### chunk number 27: niftiAuditTrail eval=FALSE
###################################################
## #line 808 "dicom_nifti3.Rnw"
## options(niftiAuditTrail=FALSE)


###################################################
### chunk number 28: NIfTI audit.trail 01
###################################################
#line 821 "dicom_nifti3.Rnw"
audit.trail(mniLR)


###################################################
### chunk number 29: EBImage01 eval=FALSE
###################################################
## #line 840 "dicom_nifti3.Rnw"
## mniLR.range <- range(mniLR)
## display((mniLR - min(mniLR)) / diff(mniLR.range))


###################################################
### chunk number 30: ffd
###################################################
#line 888 "dicom_nifti3.Rnw"
(ffd <- readNIfTI("filtered_func_data"))


###################################################
### chunk number 31: ffd-png
###################################################
#line 891 "dicom_nifti3.Rnw"
png(filename="ffd.png", width=2*480, height=2*480, bg="black")


###################################################
### chunk number 32: ffd-image
###################################################
#line 894 "dicom_nifti3.Rnw"
image(ffd, zlim=range(ffd)*0.85)


###################################################
### chunk number 33: ffd-dev.off
###################################################
#line 897 "dicom_nifti3.Rnw"
dev.off()


###################################################
### chunk number 34: ffd-ortho-png
###################################################
#line 913 "dicom_nifti3.Rnw"
png(filename="ffd_orthographic.png", width=2*480, height=2*480, bg="black")


###################################################
### chunk number 35: ffd-orthographic
###################################################
#line 916 "dicom_nifti3.Rnw"
orthographic(ffd, xyz=c(34,29,10), zlim=range(ffd)*0.85)


###################################################
### chunk number 36: ffd-ortho-dev.off
###################################################
#line 919 "dicom_nifti3.Rnw"
dev.off()


###################################################
### chunk number 37: ffd-glm-design
###################################################
#line 961 "dicom_nifti3.Rnw"
## visual (30s on/off) and auditory (45s on/off) stimulus
#visual <- rep(c(-0.5,0.5), each=30, times=12)
#auditory <- rep(c(-0.5,0.5), each=45, times=12)
## haemodynamic response function with mean=6 and sd=3
#hrf <- c(dgamma(1:15, 4, scale=1.5))
## convolve stimuli with hrf
#visual.hrf <- auditory.hrf <- rep(NA, 540)
#for (k in 60:599) {
#  visual.hrf[k-59] <- sum(visual[k-(1:15)] * hrf)
#}
#for (k in 90:629) {
#  auditory.hrf[k-89] <- sum(auditory[k-(1:15)] * hrf)
#}
## add phase
#visual.hrf <- c(rep(0.5, 30), visual.hrf)
#auditory.hrf <- c(rep(0.5, 45), auditory.hrf)
## TR = 3s
#index <- seq(3, 540, by=3)
#visual.hrf2 <- visual.hrf[index] - mean(visual.hrf[index]) # center
#auditory.hrf2 <- auditory.hrf[index] - mean(auditory.hrf[index]) # center
## visual (30s on/off)

visual <- rep(c(-0.5,0.5), each=30, times=9)
## auditory (45s on/off) stimulus
auditory <- rep(c(-0.5,0.5), each=45, times=6)
## haemodynamic response function with mean=6 and sd=3
hrf <- c(dgamma(1:15, 4, scale=1.5))
## convolve visual stimulus with HRF
hrf0 <- c(hrf, rep(0, length(visual)-length(hrf)))
visual.hrf <- convolve(hrf0, visual)
## convolve auditory stimulus with HRF
hrf0 <- c(hrf, rep(0, length(auditory)-length(hrf)))
auditory.hrf <- convolve(hrf0, auditory)
index <- seq(3, 540, by=3)
visual.hrf <- visual.hrf[index] # - mean(visual.hrf[index]) # center
auditory.hrf <- auditory.hrf[index] # - mean(auditory.hrf[index]) # center


###################################################
### chunk number 38: ffd-design.png
###################################################
#line 999 "dicom_nifti3.Rnw"
pdf("ffd_design.pdf", width=10, height=5)
par(mfrow=c(1,2), mex=0.85)
plot(index, visual.hrf, type="l", 
     xlab="Acquisition Index", ylab="Visual Stimulus")
plot(index, auditory.hrf, type="l", 
     xlab="Acquisition Index", ylab="Auditory Stimulus") 
dev.off()


###################################################
### chunk number 39: ffd-glm
###################################################
#line 1018 "dicom_nifti3.Rnw"
## background threshold: 10% max intensity
voxel.lsfit <- function(x, thresh) { # general linear model
  ## check against background threshold
  if (max(x) < thresh) {
    return(rep(NA, 5))
  }
  ## glm
  ## output <- lm(x ~ visual.hrf + auditory.hrf)
  output <- lsfit(cbind(visual.hrf, auditory.hrf), x)
  ## extract t-statistic, p-values
  ## c(as.vector(summary(output)$coeff[2:3,3:4]), summary(output)$fstatistic[1])
  output.t <- ls.print(output, print.it=FALSE)$coef.table[[1]][2:3,3:4]
  output.f <- ls.print(output, print.it=FALSE)$summary[3]
  c(output.t, as.numeric(output.f))
  ## as.vector(output.t)
}
## apply local glm to each voxel
ffd.glm <- apply(ffd, 1:3, voxel.lsfit, thresh=0.1 * max(ffd))


###################################################
### chunk number 40: zstat1
###################################################
#line 1065 "dicom_nifti3.Rnw"
## t-statistics
##t.visual <- nifti(ffd.glm[1,,,], datatype=16)
##t.auditory <- nifti(ffd.glm[2,,,], datatype=16)
## p-values
#p.visual <- nifti(ffd.glm[3,,,], datatype=16)
#p.auditory <- nifti(ffd.glm[4,,,], datatype=16)
## F-statistics
#F <- nifti(ffd.glm[5,,,], datatype=16)
# Z-statistic: normalized t-statistic
dof <- ntim(ffd) - 1
Z.visual <- nifti(qnorm(pt(ffd.glm[1,,,], dof, log.p=TRUE), log.p=TRUE),
                  datatype=16)
Z.auditory <- nifti(qnorm(pt(ffd.glm[2,,,], dof, log.p=TRUE), log.p=TRUE),
                    datatype=16)


###################################################
### chunk number 41: zstat1-png
###################################################
#line 1081 "dicom_nifti3.Rnw"
png("ffd_zstat1.png", width=2*480, height=2*480, bg="black")


###################################################
### chunk number 42: zstat1-overlay
###################################################
#line 1084 "dicom_nifti3.Rnw"
overlay(ffd, ifelse(Z.visual > 5, Z.visual, NA), 
        zlim.x=range(ffd)*0.85, zlim.y=range(Z.visual, na.rm=TRUE))


###################################################
### chunk number 43: zstat1-dev.off
###################################################
#line 1087 "dicom_nifti3.Rnw"
dev.off()


###################################################
### chunk number 44: zstat2-png
###################################################
#line 1090 "dicom_nifti3.Rnw"
png("ffd_zstat2.png", width=2*480, height=2*480, bg="black")


###################################################
### chunk number 45: zstat2-overlay
###################################################
#line 1093 "dicom_nifti3.Rnw"
overlay(ffd, ifelse(Z.auditory > 5, Z.auditory, NA), 
        zlim.x=range(ffd)*0.85, zlim.y=range(Z.auditory, na.rm=TRUE))


###################################################
### chunk number 46: zstat2-dev.off
###################################################
#line 1096 "dicom_nifti3.Rnw"
dev.off()


###################################################
### chunk number 47: DICOM2NIFTI HK40 01
###################################################
#line 1171 "dicom_nifti3.Rnw"
dput(formals(dicom2nifti))
(hk40n <- dicom2nifti(hk40))


###################################################
### chunk number 48: DICOM2NIFTI HK40 02 eval=FALSE
###################################################
## #line 1175 "dicom_nifti3.Rnw"
## image(hk40n)
## orthographic(hk40n, col.crosshairs="green")


###################################################
### chunk number 49: DICOM2NIFTI HK40 03
###################################################
#line 1179 "dicom_nifti3.Rnw"
png("hk40n_image.png", width=2*480, height=2*480, bg="black")
image(hk40n, zlim=c(0,1024))
dev.off()
png("hk40n_orthographic.png", width=2*480, height=2*480, bg="black")
orthographic(hk40n, zlim=c(0,1024), col.crosshairs="green")
dev.off()


###################################################
### chunk number 50: DICOM2NIFTI HK40 04
###################################################
#line 1195 "dicom_nifti3.Rnw"
(hk40n <- dicom2nifti(hk40, DIM=4))


###################################################
### chunk number 51: RIDER Neuro MRI
###################################################
#line 1226 "dicom_nifti3.Rnw"
subject <- "1086100996"
DCM <- dicomSeparate(subject, verbose=TRUE, counter=100)
seriesInstanceUID <- extractHeader(DCM$hdr, "SeriesInstanceUID", FALSE)
for (uid in unique(seriesInstanceUID)) {
  index <- which(unlist(lapply(DCM$hdr, function(x) uid %in% x$value)))
  uid.dcm <- list(hdr=DCM$hdr[index], img=DCM$img[index])
  patientsName <- extractHeader(uid.dcm$hdr, "PatientsName", FALSE)
  studyDate <- extractHeader(uid.dcm$hdr, "StudyDate", FALSE)
  seriesDescription <- extractHeader(uid.dcm$hdr, "SeriesDescription", FALSE)
  fname <- paste(gsub("[^0-9A-Za-z]", "", 
                      unique(c(patientsName, studyDate, seriesDescription))), 
                 collapse="_")
  cat("##  ", fname, fill=TRUE)
  if (gsub("[^0-9A-Za-z]", "", unique(seriesDescription)) == "axtensor") {
    D <- 4
    reslice <- FALSE
  } else {
    D <- 3
    reslice <- TRUE
  }
  uid.nifti <- dicom2nifti(uid.dcm, DIM=D, reslice=reslice,
                           descrip=c("PatientID", "SeriesDescription"))
  writeNIfTI(uid.nifti, fname)
}


