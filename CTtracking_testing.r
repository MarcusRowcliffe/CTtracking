source("C:/Users/rowcliffe.m/Documents/GitHub/CTtracking/CTtracking_1_4.r")

dir <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test2"
remove.file.spaces(dir)
exf <- read.exif(dir)
exf$CreateDate
times <- strptime(exf$CreateDate[1:34], "%Y:%m:%d %H:%M:%S", tz="UTC")
(fps <- 1 / as.numeric(mean(times[2*(1:17)] - times[2*(1:17)-1])))

inf <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test2/f1"
outf <- paste0(inf, "_frames")
extract.frames(0.1, inf, outf, copy.jpegs=TRUE, stamp.time=FALSE)
exf <- read.exif(outf)
View(exf)
crop(outf, outf, suffix="-cropped")

inf <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test2/f2"
outf <- paste0(inf, "_frames")
undebug(extract.frames)
debug(copy.images)
extract.frames(0.1, inf, outf, copy.jpegs=TRUE, stamp.time=FALSE, relative.res=1.704)
debug(crop)
crop(outf, outf, suffix="-cropped")

inf <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test2/f3"
outf <- paste0(inf, "_frames")
extract.frames(0.1, inf, outf, copy.jpegs=TRUE, stamp.time=FALSE, relative.res=2.3, x.offset=-250, y.offset=-150)
crop(outf, outf, suffix="-cropped")

exf <- read.exif(outf)
View(exf[,c("VideoXorigin","VideoYorigin", "VideoHeight", "VideoWidth", "VideoHeightOnImage", "VideoWidthOnImage")])
View(exf[,c("FileName", "CreateDate", "VideoCreateDate", "FrameExtractRate", "FrameNumber", "CreateTimeOffset")])

debug(copy.images)

indir <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test2/f3"
inf <- file.path(indir, "A400_2018_07_04_08_08_09_.jpg")
inf <- file.path(indir, "A800_2018_01_17_13_45_34_.jpg")
outf <- file.path(paste0(indir,"_frames"), "cropped.jpg")
img <- image_read(inf)
img <- image_crop(img, "4416x3312+-100+100")
image_write(img, outf)

library(magick)
res2 <- (3256*1830)/(1920*1080)
res <- sqrt(res2)
res <- 1.704
res <- 2.3
for(res in seq(1.69, 1.71, 0.001)){
  imgW <- 4416/res
  imgH <- 3312/res
  vidW <- 1920
  vidH <- 1080
  Hmargin <- res*(imgH-vidH)/2-150
  Wmargin <- res*(imgW-vidW)/2-250
  H <- res*(vidH)
  W <- res*vidW
  if(Hmargin<0){
    H <- H+Hmargin
    Hmargin <- 0
  }
  if(Wmargin<0){
    W <- W+Wmargin
    Wmargin <- 0
  }
  img <- image_read(inf)
  outf <- paste0(indir,"/cropped", res, ".jpg")
  img <- image_crop(img, paste0(res*vidW,"x",res*vidH,"-",Wmargin,"+",Hmargin))
  image_write(img, outf)
}
outf
setwd("C:/exiftool")
cmd <- paste0("exiftool -videoxorigin=", 1, " -videoyorigin=", 1, " -relativeresolution=", 1, " ", 
              fff, " -overwrite_original")
fff <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test2/f1_frames/*.jpg"
shell(cmd)
exf <- read.exif("C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test2/f1_frames")
exf$vide
exf$XOrigin
exf$RelativeResolution
