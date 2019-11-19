#Using V0.2 (Laura Vargas Zarco version of Animaltracker)

source("CTtracking.r")

exifdat.cam <- read.exif("./Survey_yyy/CameraImages")

camdat <- read.digidat("./Survey_yyy/CameraData", 
                       exifdat=exifdat.cam,
                       datatype="pole")
View(camdat)
cmods <- cal.cam(camdat)
plot(cmods)

#Do this first time
exifdat <- read.exif("./Survey_yyy/DeploymentImages")
write.csv(exifdat, "./Survey_yyy/exifdata.csv", row.names = FALSE)

##In subsequent sessions just run this:
exifdat <- read.csv("./Survey_yyy/exifdata.csv", stringsAsFactors = FALSE)
depdat <- read.digidat(path="./Survey_yyy/DeploymentData", 
                       exifdat=exifdat,
                       datatype="both")
View(depdat$animal)
View(depdat$pole)

deptab <- read.csv("./Survey_yyy/deptable.csv")

View(deptab)
smods <- cal.site(depdat$pole, cmods, deptab)
plot(smods)

posdat <- predict.pos(depdat$animal, smods)
View(posdat)
hist(posdat$radius)
hist(posdat$angle)

seqdat <- seq.summary(posdat)
View(seqdat)
