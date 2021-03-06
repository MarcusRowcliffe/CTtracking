setwd("C:/Users/scott/Desktop/Thesis/Github/CTtracking")
library(data.table)
source("CTtracking_1_0.r")

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
# (need to create unique sequence_id values, convert height units cm to m, and rename some columns)
dat <- read.csv("./Gee data/new_full_data.csv", stringsAsFactors=F)
dat$sequence_id <- paste(dat$CTsite, dat$sequence_id, sep="_")
hts <- as.numeric(dat$sequence_annotation)/100
dat$sequence_annotation[!is.na(hts)] <- hts[!is.na(hts)]
names(dat)[names(dat)=="CTsite"] <- "site_id"
names(dat)[names(dat)=="Camera_ID"] <- "cam_id"
names(dat)[names(dat)=="xres"] <- "xdim"
names(dat)[names(dat)=="yres"] <- "ydim"
dat <- dat[-c(264,265,266,267,268,269), ] 
write.csv(dat, "./Gee data/new_full_data2.csv", row.names=F)


#Extract data for camera calibration model
cdat <- read.poledat("./Gee data/cam_cal.csv", "pole_id;distance;length")
View(cdat)

#Fit camera calibration model(s)
cmod <- cal.cam(cdat)


#Show diagnostic plots
par(mfrow=c(1,2))
lapply(cmod, plot)


#Extract data for camera calibration model
sdat <- read.poledat("./Gee data/REM_dig.csv", "height")
View(sdat)

#FIXING A PROBLEM
#One site_id has no cam_id associated - assigning an arbitrary one for now 
sdat$cam_id[sdat$cam_id==""] <- "B12"
sdat <- read.poledat("./Gee data/new_full_data2.csv", "height")
View(sdat)

#FIXING A COUPLE OF PROBLEMS
#
#1. One site_id has no cam_id associated - assigning an arbitrary one for now 
sdat$cam_id[sdat$cam_id==""] <- "B12"
#
>>>>>>> a60d7aa01247219561f03d925ed7e8db3cbeed5c

#Create site-to-camera lookup table
site.by.cam <- table(sdat$site_id, sdat$cam_id)
sctable <- data.frame(site_id=rownames(site.by.cam),
                      cam_id=colnames(site.by.cam)[apply(site.by.cam, 1, function(x) which(x>0))]
)
View(sctable)

#Fit site calibration models
smod <- cal.site(cmod, sdat, sctable)

#Show diagnostic plots
par(mfrow=c(1,2))
lapply(smod, plot)

#Predict positions (angle and radius) for animal data
posdat <- predict.pos(file="./Gee data/REM_dig.csv", mod=smod, fields="species")
posdat <- predict.pos(file="./Gee data/new_full_data2.csv", mod=smod, fields="species")
View(posdat)
#Might want to check how many radii are infinite, or finite but improbably large
sum(is.infinite(posdat$radius))
sum(posdat$radius>20)
#Let's discuss what to do with these once you've had a closer look

#Extract 1) trigger position data; and 2) movement sequence data
dat <- seq.summary(posdat, 0.5)
View(dat$trigdat)
View(dat$movdat)

dat$trigdat$dtime <- decimal.time(dat$trigdat$Time)
dat$trigdat$rtime <- dat$trigdat$dtime * 2*pi
dat$trigdat$absangle <- abs(dat$trigdat$angle)

write.csv(dat$trigdat, "./Gee data/trigdat.csv", row.names=F)
write.csv(dat$movdat, "./Gee data/movdat.csv", row.names=F)

    
#############################################
#REM ANALYSIS
#############################################
#Load data
trigdat <- read.csv("./Gee data/trigdat.csv")
movdat <- read.csv("./Gee data/movdat.csv")
full.metadata <- read.csv("./Gee data/SVP_OC_2012-2017.csv")
REM.full.dat <- read.csv("./Gee data/REM_full_data.csv")

#Load source code for functions
source("./Source code/REM_tools.r")
source("./Source code/distancedf.r")
source("./Source code/SpeedCode.r")
source("./Source code/traprate_code.r")
library(activity)
library(lubridate)

#Create dataframe summarising numbers of conacts and camera time per placment
DateTime <- parse_date_time(paste(full.metadata$Date, full.metadata$Time),
                            c("d/m/Y H:M:S", "d/m/y H:M:S", "d/m/Y H:M", "d/m/y H:M"), "UTC")
o <- as.POSIXct("1970-01-01 00:00:00", tz="UTC")
start <- as.POSIXct(tapply(DateTime, full.metadata$CTsite, min), origin=o)
stop <- as.POSIXct(tapply(DateTime, full.metadata$CTsite, max), origin=o)
secs <- as.numeric(stop-start)
days <- secs/(24*60^2)
full.sitetimes <- data.frame(site_id=names(stop), start, stop, secs, days)
View(full.sitetimes[order(days),])

sites.used <- unique(REM.full.dat$site_id)
sitedat <- subset(full.sitetimes, site_id %in% sites.used)
View(sitedat[order(sitedat$days), ])

trigs <- table(trigdat[ , c("site_id", "species")])
sitedat[,c("Da","Lt","Mt")] <- 0
sitedat[match(rownames(trigs), sitedat$site_id), c("Da","Lt","Mt")] <- trigs

#Pending fixes, removing sites with missing dates and deployment length >80 days
sitedat <- subset(sitedat, !is.na(days) & days<80)


#====================================================================
#Detection function analysis
#1. Fit half-normal and hazard-rate models (function fitdf)
#2. Check AICs (information criterion) to select best model
#3. Check goodness of fit of models (visual inspection of plots)
#4. Extract effective detection angle/radius estimates
#====================================================================
#angle
#====================================================================
sp <- "Da"
amodN <- fitdf(absangle~1, subset(trigdat, species==sp))
amodH <- fitdf(absangle~1, subset(trigdat, species==sp), key="hr")
amodN$ddf$criterion
amodH$ddf$criterion
plot(amodN$ddf)
(angle <- amodN$edd)

#====================================================================
#distance
#====================================================================
dmodN<- fitdf(radius~1, subset(trigdat, species==sp), transect="point")
dmodH<- fitdf(radius~1, subset(trigdat, species==sp), transect="point", key="hr")
dmodN$ddf$criterion
dmodH$ddf$criterion
plot(dmodN$ddf, pdf=TRUE)
plot(dmodH$ddf, pdf=TRUE)
(distance <- dmodH$edd)

#====================================================================
#Activity analysis
#1. Load activity package
#2. Fit activity model (function fitact)
#3. Plot model to inspect fit (function plot with CircFit object input)
#====================================================================
times <- subset(trigdat, species==sp & !is.na(rtime))$rtime
(activity <- fitact(times, reps=100))
plot(activity)

#====================================================================
#Speed analysis
#1. Fit size-biased log-normal and Weibull models to speed data
#2. Check AICs (information criterion) to select best model
#3. Check goodness of fit of model (visual inspection of plots)
#====================================================================
speed.lognorm <- fit.spd(speed~1, subset(movdat, species==sp), pdf="lnorm")
speed.weibull <- fit.spd(speed~1, subset(movdat, species==sp), pdf="weibull")
speed.gamma <- fit.spd(speed~1, subset(movdat, species==sp), pdf="gamma")
AIC(speed.lognorm)
AIC(speed.weibull)
AIC(speed.gamma)
plot(speed.weibull)
(speed <- predict.sbm(speed.weibull))

#####################################################################
#Density analysis
#####################################################################

#====================================================================
#Create parameter and parameter SE lists, harmonising units
#	In this case, camera deployment time is in days, and we want 
#	to estimate density in square km, so detection zone (m) is divided 
#	by 1000, while speed (m per s) is multiplied by 86.4 (60*60*24 seconds in
#	a day divided by 1000 m in a km). Detection angle (theta) data were measured from 
# centre line, so must be doubled in the parameter list.
#====================================================================
params <- list(r=distance$estimate/1000, theta=angle$estimate*2,
               v=speed$speed*86.4, p=activity@act["act"])
paramSEs <- list(r=distance$se/1000, theta=angle$se*2,
                 v=speed$se*86.4, p=activity@act["se"])

#====================================================================
#Calculate density (function bootTRD)
#====================================================================

#Finally, the result
(dens <- bootTRD(sitedat[,sp], sitedat$days, params, paramSEs))
