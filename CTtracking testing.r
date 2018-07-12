setwd("C:/Users/scott/Desktop/Thesis/Github/CTtracking")
library(data.table)
source("CTtracking.r")

cdat <- read.poledat("./Gee data/cam_cal2.csv", "pole_id;distance;length")
cmod <- cal.cam(cdat)
plot(cmod$cam)

#pth <- paste(getwd(), "Gee data/DigiDat", sep="/")
#merge.csv(pth, sitecol="CTsite")
sdat <- read.poledat("./Gee data/DigiDat/merged/merged.csv", "height")
sctable <- read.csv("./Gee data/cam_table.csv")
smod <- cal.site(cmod, sdat, sctable)
lapply(smod, plot)

posdat <- predict.pos(file="./Gee data/DigiDat/merged/merged.csv", mod=smod, fields="species")
#Might want to check how many radii are infinite, or finite but improbably large, eg 
sum(is.infinite(posdat$radius))
sum(posdat$radius>1500)

dat <- seq.summary(posdat, 2)
View(dat$trigdat)
View(dat$movdat)
