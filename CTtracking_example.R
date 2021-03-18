#Using V0.2 (Laura Vargas Zarco version of Animaltracker)

devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/master/CTtracking.r")
source("CTtracking.r")

install.exiftool()
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
peep <- head(peep.exif(deppth))
peep$Value <- strtrim(peep$Value, 29)
peep
dep.exdat <- read.exif(deppth, tagfield="Keywords")
View(dep.exdat)
write.csv(dep.exdat, file.path(folder, "exifdata.csv"), row.names = FALSE)

##In subsequent sessions just run this:
deppth <- file.path(folder, "Deployments")
dep.exdat <- read.csv(file.path(folder, "exifdata.csv"), stringsAsFactors = FALSE)

#Deployment calibration models
depdat <- read.digidat(deppth, exifdat=dep.exdat)
caldat <- pairup(subset(depdat, species=="calibration"), c("folder", "image_name"))
View(depdat)
View(animdat)
View(caldat)

(deptab <- read.csv(file.path(folder, "deptable.csv")))
dmods <- cal.dep(caldat, cmods, "deployment", deptab)
plot(dmods)

animdat <- subset(depdat, species!="calibration")
posdat <- predict.pos(animdat, dmods)
View(animdat)
View(posdat)
hist(posdat$radius)
hist(posdat$angle)

seqdat <- seq.summary(posdat)
View(seqdat)

install.packages("jpeg")
library(jpeg)
show.image(animdat, file.path(deppth, "S01"), "anim")
View(animdat)

