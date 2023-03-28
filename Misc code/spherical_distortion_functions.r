##########################################################
#spehrical distortion correction
##########################################################
library(tidyverse)

#Correct pixel positions for spherical distortion
#INPUT
# K: distortion coefficient
# x,y: arrays of x,y pixel values to correct
# xdim,ydim: x,y image dimensions
#OUTPUT
#A list with components:
# x,y: corrected x,y pixel arrays
correct <- function(K, x, y, xdim, ydim){
  relx <- (x - 0.5*xdim) / ydim
  rely <- y / ydim
  r2 <- relx^2 + (rely-0.5)^2
  denom <- 1 + K * r2
  list(x = relx / denom, y = 0.5 + (rely-0.5) / denom)
}

#Calculate pixel length of objects on the corrected scale
#INPUT
# K: distortion coefficient
# dat: dataframe containing xb,xt,yb,yt, ImageHeight, ImageWidth
#OUTPUT
#A data frame with columns for corrected x,y base and top pixel positions plus
#corrected pixel length.
calc_pixlen <- function(K, dat){
  corxy <- correct(K, select(dat, xb, xt), select(dat, yb, yt), 
                 dat$ImageWidth, dat$ImageHeight)
  data.frame(corxy$x, corxy$y,
       pixlen=sqrt((corxy$x$xb - corxy$x$xt)^2 + (corxy$y$yb - corxy$y$yt)^2))
}

#Create a camera calibration model
#INPUT
#A dataframe holding:
# xb,xt,yb,yt: x,y base and top pixels
# length: lengths of pole between digitised points
# distance: distances of poles from camera
cam_cal <- function(dat){
  #Estimated focal length
  foclen <- function(K){
    pl <- calc_pixlen(K, dat)
    with(dat, distance * pl$pixlen / length)
  }
  
  #Objective function for estimation of K (distortion coefficient)
  obfun <- function(K){
    fl <- foclen(K)
    sd(fl) / mean(fl)
  }
  
  res <- optimise(obfun, c(0, 1))
  cxy <- calc_pixlen(res$minimum, dat)
  list(K=res$minimum, focal_length=mean(foclen(res$minimum)), data=dat, corxy=cxy)
}

#Calculate distance based on pole pixel pairs and a camera calibration model
#INPUT
# dat: a dataframe containing xb,xt,yb,yt, ImageHeight, ImageWidth, length
# cam_model: a camera calibration model
#OUTPUT
# A dataframe containing calc_pixlen output plus predicted distance
calc_distance <- function(dat, cam_model){
  cdat <- calc_pixlen(cam_model$K, dat)
  cdat$distance <- cam_model$focal_length * dat$length / cdat$pixlen
  cdat
}

#Create deployment calibration model
#INPUT
# dat: a dataframe that can be passed to calc_distance
# cam_model: a camera calibration model
#OUTPUT
#A list with components:
# cam_model: the input camera calibration model
# model: the nls deployment surface model
# data: the input dat
# corxy: the corrected pole pixel positions (xb,xt,yb,yt), plus extrapolated 
#        pixel positions of pole ground contact (x,y) and estimated distances
#        (distance)
dep_cal <- function(dat, cam_model){
  cxy <- calc_distance(dat, cam_model)  
  cxy$x <- with(cxy, xb-dat$hb*(xt-xb)/dat$length)
  cxy$y <- with(cxy, yb-dat$hb*(yt-yb)/dat$length)

  b1.start <- runif(1,0,max(cxy$distance))
  repeat{
       mod <- try(nls(distance~b1/(y-(b2+b3*x)), data=cxy, algorithm="port", 
                      start=list(b1=b1.start, b2=min(cxy$y)*0.9, b3=0),
                      lower=c(b1=0,b2=-Inf,b3=-Inf), 
                      upper=c(b1=Inf,b2=min(cxy$y),b3=Inf),
                      trace=F ))
    if(class(mod)=="nls") break
  }
  res <- list(cam.model=cam_model, model=mod, data=dat, corxy=cxy)
}

#Predict radial distance given pixel positions and a deployment calibration model
#INPUT
# dat: a dataframe with pixel positions (x,y) and image dimensions (xdim,ydim)
# mod: a deployment calibration model
predict_r <- function(dat, mod){
  corxy <- as.data.frame(correct(mod$cam.model$K, dat$x, dat$y, dat$ImageWidth, dat$ImageHeight))
  predict(mod$model, newdata=corxy)
}
