setwd("C:/Users/scott/Desktop/Thesis/Github/CTtracking")
library(data.table)
source("CTtracking.r")

<<<<<<< HEAD
cdat <- read.poledat("./Gee data/cam_cal2.csv", "pole_id;distance;length")
=======
#Edit cam_cal file (new version created: cam_cal2)
# (need to reshape annotations to give single uniqe pole_id, and rename some columns)
dat <- read.csv("./Gee data/cam_cal.csv")
seqan <- strsplit(as.character(dat$sequence_annotation), ";")
i <- unlist(lapply(seqan, length))==3
seqan <- seqan[i]
dat <- dat[i,]
id <- unlist(lapply(seqan, function(x) paste(x[1],x[2],sep=".")))
dist <- unlist(lapply(seqan, function(x) x[1]))
len <- unlist(lapply(seqan, function(x) x[3]))
dat$sequence_annotation <- paste(id,dist,len, sep=";")
names(dat)[names(dat)=="Camera_ID"] <- "cam_id"
names(dat)[names(dat)=="xres"] <- "xdim"
names(dat)[names(dat)=="yres"] <- "ydim"
write.csv(dat, "./Gee data/cam_cal2.csv", row.names=F)

#Edit full_data file (new version created: full_data2)
# (need to rename some columns)
dat <- read.csv("./Gee data/full_data.csv")
names(dat)[names(dat)=="CTsite"] <- "site_id"
names(dat)[names(dat)=="Camera_ID"] <- "cam_id"
names(dat)[names(dat)=="xres"] <- "xdim"
names(dat)[names(dat)=="yres"] <- "ydim"
write.csv(dat, "./Gee data/full_data2.csv", row.names=F)


#Extract data for camera calibration model
cdat <- read.poledat("./Gee data/cam_cal2.csv", "pole_id;distance;length")
View(cdat)

#Fit camera calibration model(s)
>>>>>>> upstream/master
cmod <- cal.cam(cdat)

<<<<<<< HEAD
#pth <- paste(getwd(), "Gee data/DigiDat", sep="/")
#merge.csv(pth, sitecol="CTsite")
sdat <- read.poledat("./Gee data/DigiDat/merged/merged.csv", "height")
sctable <- read.csv("./Gee data/cam_table.csv")
=======
#Show diagnostic plots
par(mfrow=c(1,2))
lapply(cmod, plot)

#Extract data for camera calibration model
sdat <- read.poledat("./Gee data/full_data2.csv", "height")
View(sdat)

#FIXING A COUPLE OF PROBLEMS
#
#1. One site_id has no cam_id associated - assigning an arbitrary one for now 
sdat$cam_id[sdat$cam_id==""] <- "B15"
sctable[27,2] <- "B15"
#
#2. One site has more than one set of image dimensions - assigning arbitrary dimensions for now
which(apply(table(sdat$site_id, sdat$xdim), 1, function(x) sum(x!=0)) > 1)
which(apply(table(sdat$site_id, sdat$ydim), 1, function(x) sum(x!=0)) > 1)
subset(sdat, site_id=="OCCAJ17")[,c("xdim","ydim")]
sdat[sdat$site_id=="OCCAJ17", ]$xdim <- 3264
sdat[sdat$site_id=="OCCAJ17", ]$ydim <- 2488

#Create site-to-camera lookup table
site.by.cam <- table(sdat$site_id, sdat$cam_id)
sctable <- data.frame(site_id=rownames(site.by.cam),
                      cam_id=colnames(site.by.cam)[apply(site.by.cam, 1, function(x) which(x>0))]
)
View(sctable)

#Fit site calibration models
undebug(cal.site)
>>>>>>> upstream/master
smod <- cal.site(cmod, sdat, sctable)


#Show diagnostic plots
par(mfrow=c(1,2))
lapply(smod, plot)

#Predict positions (angle and radius) for animal data
posdat <- predict.pos(file="./Gee data/DigiDat/merged/merged.csv", mod=smod, fields="species")
View(posdat)
#Might want to check how many radii are infinite, or finite but improbably large, eg 
sum(is.infinite(posdat$radius))
sum(posdat$radius>1500)
hist(radius)

#Extract 1) trigger position data; and 2) movement sequence data
dat <- seq.summary(posdat, 0.5)
View(dat$trigdat)
View(dat$movdat)
