setwd("C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool2018/Gee data")

cdat <- read.poledat("camcal.csv", "pole_id;distance;length")
ddat <- data.frame(x=3264, y=2449)
cmod <- cal.cam(cdat, ddat)
par(mfrow=c(1,2))
plot(cmod)

#ccdat <- rbind(cdat, cdat)
#ccdat$cam_id <- rep(c("a","b"), each=nrow(cdat))
#ddat <- rbind(ddat, ddat)
#ddat$cam_id <- c("a","b")
#ccmod <- cal.cam(ccdat, ddat)
#lapply(ccmod, plot)

sdat <- read.poledat("sitecal.csv", "height")
smod <- cal.site(cmod, sdat)
plot(smod)

#ssdat <- rbind(sdat,sdat)
#ssdat$site_id <- rep(c("a","b"), each=nrow(sdat))
#ssmod <- cal.site(cmod, ssdat)
#lapply(ssmod, plot)


