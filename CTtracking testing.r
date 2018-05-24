setwd("C:/Users/rowcliffe.m/Documents/GitHub/CTtracking")
source("CTtracking.r")

#Extract data for camera calibration model
cdat <- read.poledat("./Gee data/camcal.csv", "pole_id;distance;length")
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
