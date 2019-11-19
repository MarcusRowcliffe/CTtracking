#Using V0.2 (Laura Vargas Zarco version of Animaltracker)

source("CTtracking.r")

path <- "C:/Users/rowcliffe.m/Documents/OneDriveZSL/GitHub/CTtracking/Survey_yyy"

exifdat.cam <- read.exif("./Survey_yyy/CameraImages")

camdat <- read.digidat("./Survey_yyy/CameraData", 
                       exifdat=exifdat.cam,
                       datatype="pole")
View(camdat)
cmods <- cal.cam(camdat)
plot(cmods)

exifdat <- read.exif("./Survey_yyy/DeploymentImages")
write.csv(exifdat, "./Survey_yyy/exifdata.csv", row.names = FALSE)

##In subsequent sessions just run this:
exifdat <- read.csv("./Survey_yyy/exifdata.csv", stringsAsFactors = FALSE)
depdat <- read.digidat(path="./Survey_yyy/DeploymentData", 
                       exifdat=exifdat,
                       datatype="both")
View(depdat$animal)
View(depdat$pole)

deptab <- read.csv("./Survey_yyy/deptab.csv", colClasses = "character")

View(deptab)
smods <- cal.site(depdat$pole, cmods, deptab)
plot(smods)

posdat <- predict.pos(depdat$animal, smods)
View(posdat)
hist(posdat$radius)
hist(posdat$angle)

seqdat <- seq.summary(posdat)
View(seqdat)
