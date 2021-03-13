#Using V0.2 (Laura Vargas Zarco version of Animaltracker)

source("CTtracking.r")
folder <- "./Survey_yyy"

#Camera calibration models
campth <- file.path(folder, "Cameras")
fields <- c(dir="Directory", file="FileName", 
            xdim="ImageWidth", ydim="ImageHeight")
cam.exdat <- read.exif(campth, fields)
camdat <- read.digidat(campth, cam.exdat)
camdat <- pairup(camdat, pair=c("folder", "image_name"))
View(camdat)
cmods <- cal.cam(camdat, "folder")
plot(cmods)

#Do this first time
deppth <- file.path(folder, "Deployments")
View(peep.exif(deppth))
fields <- c(dir="Directory", file="FileName", datetime="DateTimeOriginal", 
            xdim="ImageWidth", ydim="ImageHeight")
dep.exdat <- read.exif(deppth, fields, "Keywords")
View(exifdat)
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

#CHECKING
cd <- caldat[, names(caldat) %in% c("xb","xt","yb","length")]

debug(calc.distance)
calc.distance(cd, cmods[1]) #OK
calc.distance(cd, cmods) #ERROR idtag missing
calc.distance(cd, cmods, "camera") #ERROR idtag not in dat
calc.distance(cd, cmods, "deployment") #ERROR lookup table missing
calc.distance(cd, cmods, "deploymentasd") #ERROR idtag not in dat
calc.distance(cd, cmods, lookup=deptab) #ERROR idtag missing
debug(calc.distance)
calc.distance(cd, cmods, "deployment", deptab)
calc.distance(cd, cmods, "deploymenas", deptab) #ERROR idtag missing

plusdist <- calc.distance(cd, cmods, "deployment", deptab)

minusrows <- head(caldat, -15)
pld.mir <- head(plusdist, -15)
debug(cal.dep)

#dat only
m <- cal.dep(caldat) #fails - no distance column
m <- cal.dep(plusdist) #OK
m <- cal.dep(pld.mir) #OK
###ROWID STICKING IN OUTPUT FROM ABOVE
###DEPMODEL NAMING BROKEN
#dat and one cmod
m[[1]]$data$
m <- cal.dep(caldat, cmods[1]) #OK
m <- cal.dep(caldat, cmods[1], "deployment", deptab) #OK
m <- cal.dep(caldat, cmods[1], lookup=deptab) #OK
m <- cal.dep(caldat, cmods[1], "deployment") #OK

#dat and cmods
m <- cal.dep(caldat, cmods) #fails - needs idtag and lookup
m <- cal.dep(caldat, cmods, "deployment")  #fails - needs idtag and lookup
m <- cal.dep(caldat, cmods, lookup=deptab)  #fails - needs idtag and lookup
m <- cal.dep(caldat, cmods, "deployment", deptab) #OK
m <- cal.dep(caldat, cmods, "deploymentasd", deptab) #fails - idtag typo

smods <- cal.dep(caldat, cmods, "deployment", deptab)
plot(smods)

debug
names(animdat)
posdat <- predict.pos(animdat, smods)
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
??readJPEG
