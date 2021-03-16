#Using V0.2 (Laura Vargas Zarco version of Animaltracker)

source("CTtracking.r")
folder <- "./Survey_yyy"

#Camera calibration models
campth <- file.path(folder, "Cameras")
cam.exdat <- read.exif(campth)
camdat <- read.digidat(campth, cam.exdat)
camdat <- pairup(camdat, pair=c("folder", "image_name"))
View(camdat)
cmods <- cal.cam(camdat, "folder")
plot(cmods)

#Do this first time
deppth <- file.path(folder, "Deployments")
View(peep.exif(deppth))
dep.exdat <- read.exif(deppth, tagfield="Keywords")
View(dep.exdat)
write.csv(dep.exdat, file.path(folder, "exifdata.csv"), row.names = FALSE)

##In subsequent sessions just run this:
deppth <- file.path(folder, "Deployments")
dep.exdat <- read.csv(file.path(folder, "exifdata.csv"), stringsAsFactors = FALSE)

#Deployment calibration models
deptab <- read.csv(file.path(folder, "deptable.csv"))
depdat <- read.digidat(deppth, exifdat=dep.exdat)
animdat <- subset(depdat, species!="calibration")
caldat <- pairup(subset(depdat, species=="calibration"), c("folder", "image_name"))
View(depdat)
View(animdat)
View(caldat)

dmods <- cal.dep(caldat, cmods, "deployment", deptab)
plot(smods)

posdat <- predict.pos(animdat, smods)
View(animdat)
View(posdat)
hist(posdat$radius)
hist(posdat$angle)
as.POSIXct(animdat$DateTimeOriginal, tz="UTC", format="%Y:%m:%d %H:%M:%S")

seqdat <- seq.summary(posdat)
View(seqdat)

install.packages("jpeg")
library(jpeg)
show.image(animdat, file.path(deppth, "S01"), "anim")
View(animdat)
??readJPEG
