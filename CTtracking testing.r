setwd("C:/Users/rowcliffe.m/Documents/GitHub/CTtracking")
source("CTtracking.r")

#Sort out cam_cal file
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
write.csv(dat, "./Gee data/cam_cal2.csv")
View(dat)

#Extract data for camera calibration model
debug(read.poledat)
cdat <- read.poledat("./Gee data/cam_cal2.csv", "pole_id;distance;length")
View(cdat)

#Fit camera calibration model(s)
cmod <- cal.cam(cdat)

#Show diagnostic plots
plot(cmod$cam)

#Stick all the ditisation csv files together
pth <- paste(getwd(), "Gee data/DigiDat", sep="/")
merge.csv(pth, sitecol="CTsite")

#Extract data for camera calibration model
sdat <- read.poledat("./Gee data/DigiDat/merged/merged.csv", "height")
View(sdat)
sctable <- read.csv("./Gee data/cam_table.csv")

#Fit site calibration models
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
