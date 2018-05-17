setClass("camcal", representation("list"))
setClass("sitecal", representation("list"))

#Converts matrix to dataframe, converting columns to numeric when possible
matrix.data.frame <- function(mat){
  f <- function(x){
    y <- suppressWarnings(as.numeric(x))
    if(any(is.na(y))) x else y
  }
  df <- data.frame(mat, stringsAsFactors=FALSE)
  data.frame(lapply(df,f))
}

#Reads pole digitisation data for either camera or site calibration
# file: character value giving name of tracker output csv file to read (including path if not in working directory)
# fields: column headings for the sequence_annotation field, given as single text string with values separated by sep
# sep: single character separating the column names in fields
#
#Use the following column names within fields when the relevant information is present:
# cam_id: camera identifier
# pole_id: pole identifier; if not provided, frame_number is taken to be the pole identifier
# distance: distance from camera; required for camera calibration
# length: length of pole digitised; required for camera calibration
# height: height of digitised point off the ground; required for site calibration
read.poledat <- function(file, fields, sep=";"){
  dat <- read.csv(file, stringsAsFactors=FALSE)
  col.names <- unlist(strsplit(fields, sep))
  if(is.numeric(dat$sequence_annotation)) notes <- dat$sequence_annotation else
    notes <- strsplit(dat$sequence_annotation, sep)
  
  if(any(unlist(lapply(notes, length))!=length(col.names)))
    stop(paste("fields gives", length(col.names), "headings but Some annotations have <>3"))

  notes <- matrix(unlist(notes), nrow=nrow(dat), byrow=T, dimnames=list(NULL, col.names))
  dat2 <- cbind(matrix.data.frame(notes), dat[,c("frame_number", "sequence_id", "x", "y")])
  if(!"pole_id" %in% col.names) 
    names(dat2)[names(dat2)=="frame_number"] <- "pole_id"

  tab <- table(dat2$pole_id)
  duff <- !tab==2
  if(any(duff)){
    dat <- droplevels(dat[!dat$pole_id %in% names(which(duff)), ])
    warning(paste("Some poles did not have exactly 2 points digitised and were removed:", 
                  paste(names(which(duff)), collapse=" ")))
  }
  if("distance" %in% col.names){
    duff <- with(dat2, tapply(distance, pole_id, min) != tapply(distance, pole_id, max))
    if(any(duff))
      stop(paste("Some poles did not have matching distance for top and base:",
                 paste(names(which(duff)), collapse=" ")))
  }
  
  dat2 <- dat2[order(dat2$pole_id, dat2$y), ]
  i <- 2*(1:(nrow(dat)/2))
  xy <- cbind(dat2[i, c("x","y")], dat2[i-1, c("x","y")])
  names(xy) <- c("xb","yb","xt","yt")
  if("height" %in% col.names)
    xy <- cbind(xy, hb=dat2$height[i], ht=dat2$height[i-1])
  cbind(dat2[i, !(names(dat2) %in% c("height","x","y"))], xy)
}

#poledat: data frame of pole digitisation data with columns:
# pole_id: pole ID codes (Not sure this is actually needed)
# distance: pole distances from camera
# length: pole lengths
# xt,yt,xb,yb: x,y pixel positions of pole tops (t) and bases (b) in image
# cam_id: camera ID code for each record (optional - see below)
#dimdat: data frame of image dimensions for each camera with columns:
# x,y: x and y pixel dimensions
# cam_id: camera ID code for each record (optional - see below)
#If data are for multiple cameras, cam_id must be present in both input data frames, and
# camera IDs must be perfectly matched. If data are for a single camera, cam_id can be omitted
# from both input data frames.
cal.cam <- function(poledat, dimdat){
  #Internal function fits a single camera calibration model
  cal <- function(dat, dim){
    dat$pixlen <- with(dat, sqrt((xb-xt)^2 + (yb-yt)^2))
    dat$relx <- apply(dat[c("xb","xt")], 1, mean)/dim$x-0.5
    FSratio <- with(dat, distance*pixlen/(length*dim$y))
    APratio <- mean(with(dat, (acos(1-length^2/(2*(distance^2+(length/2)^2)))/pixlen)))
    mod <- lm(FSratio~I(relx^2), data=dat)
    res <- list(model=mod, APratio=APratio, dim=dim, data=dat)
    class(res) <- "camcal"
    res
  }
  
  if(class(poledat) != "data.frame" | class(dimdat) != "data.frame") 
    stop("poledat and dimdat must both be dataframes")
  if(any(!c("x","y") %in% names(dimdat)))
    stop("dimdat must contain at least x and y columns")
  
  if("cam_id" %in% names(poledat) & "cam_id" %in% names(dimdat)){
    if(!all(sort(unique(poledat$cam_id))==sort(unique(dimdat$cam_id))))
      stop("cam_id not perfectly matched between poledat and dimdat")
    out <- lapply(dimdat$cam_id, 
                  function(cam) 
                    cal(subset(poledat, cam_id==cam), subset(dimdat, cam_id==cam))
                  )
    names(out) <- dimdat$cam_id
  } else
    
  if(!"cam_id" %in% names(poledat) & !"cam_id" %in% names(dimdat)){
    if(nrow(dimdat)>1) 
      stop("dimdat must have only 1 row if data provided for a single camera")
    out <- cal(poledat, dimdat)
  } else 
    
    stop("cam_id column must be either present in both poledat and dimdat or neither")
  out
}

plot.camcal <- function(mod){
  dat <- mod$data
  cols <- grey.colors(11, start=0, end=0.8)

#PLOT POLE:PIXEL RATIO V DISTANCE RELATIONSHIP
  x <- abs(dat$relx)
  i <- round(1 + (x-min(x))*10/diff(range(x)))
  with(dat, plot(distance, length/pixlen, col=cols[i], pch=16, main=mod$dim$cam_id,
                 ylab="pole:pixel ratio", xlab="distance", sub="Shading from image centre (dark) to edge"))
  FS <- predict(mod$mod, newdata=data.frame(relx=c(0,0.5)))
  dr <- range(dat$distance)
  lines(dr, dr/(FS[1]*mod$dim$y), col=cols[1])
  lines(dr, dr/(FS[2]*mod$dim$y), col=cols[11])

#PLOT POLE IMAGE
  d <- dat$distance
  i <- round(1 + (d-min(d))*10/diff(range(d)))
  plot(c(0,mod$dim$x), c(0,-mod$dim$y), type="n", asp=1, main=mod$dim$cam_id,
       xlab="x pixel", ylab="y pixel", sub="Shading from near camera (dark) to far")
  for(p in 1:nrow(dat))
    lines(dat[p,c("xb","xt")], -dat[p,c("yb","yt")], type="l", lwd=2, col=cols[i[p]])
  lines(c(0,rep(c(mod$dim$x,0),each=2)), c(rep(c(0,-mod$dim$y),each=2),0), lty=2)
}

cal.site <- function(cmod, dat){
  cal <- function(cmod, dat){
    xdiff <- (dat$xt-dat$xb)
    ydiff <- (dat$yt-dat$yb)
    dat$pixlen <- sqrt(xdiff^2 + ydiff^2)
    dat$xg <- with(dat, xb - xdiff*hb/(ht-hb))
    dat$yg <- with(dat, yb - ydiff*hb/(ht-hb))
    dat$rely <- dat$yg/cmod$dim$y
    dat$relx <- (dat$xb+dat$xt)/(2 * cmod$dim$x) - 0.5
    FSratio <- predict(cmod$model, newdata = data.frame(relx=dat$relx))
    dat$r <- FSratio * (dat$ht-dat$hb) * cmod$dim$y/dat$pixlen
    mod <- nls(r~b1/(rely-(b2+b3*relx)), start=list(b1=2, b2=0, b3=0), data=dat)
    res <- list(cam.model=cmod, site.model=list(model=mod, data=dat))
    class(res) <- "sitecal"
    res
  }
  
  if("site_id" %in% names(dat)){
    site_id <- unique(dat$site_id)
    if(class(cmod)=="list"){
      if(!all(site_id %in% names(cmod))) stop("Not all cam_id values in dat have a matching name in cmod")
      out <- lapply(site_id, function(s) cal(cmod[[s]], subset(dat, site_id==s)))
    } else
      out <- lapply(site_id, function(s) cal(cmod, subset(dat, site_id==s)))
    names(out) <- site_id
  } else{
    if(class(cmod)!="camcal") stop("cmod must be a single camcal object if dat does not contain data for multiple sites")
    out <- cal(cmod, dat)
  }
  out
}

predict.r <- function(mod, relx, rely){
  res <- predict(mod, newdata=data.frame(relx=relx, rely=rely))
  res[res<0] <- Inf
  res
}

plot.sitecal <- function(mod){
  dim <- mod$cam.model$dim
  dat <- mod$site.model$data
  colrange <- grey.colors(11, start=0, end=0.8)
  
#PLOT DISTANCE V Y-PIXEL RELATIONSHIP
  cols <- with(dat, colrange[1+round(10*((relx-min(relx))/diff(range(relx))))])
  with(dat, plot(rely, r, col=cols, pch=16, xlim=c(0,1.5), ylim=c(0, 1.5*max(r)),
                 xlab="Relative y pixel position", ylab="Distance from camera",
                 main=unique(dat$site_id), 
                 sub="Shading from image left (dark) to right edge"))
  sq <- seq(0, 1.5, len=100)
  lines(sq, predict.r(mod$site.model$model, -0.5, sq), col=colrange[1])
  lines(sq, predict.r(mod$site.model$model, 0, sq), col=colrange[6])
  lines(sq, predict.r(mod$site.model$model, 0.5, sq), col=colrange[11])

#PLOT POLE IMAGE
  plot(c(min(c(dat$xg, 0)), max(c(dat$xg, dim$x))),
       -c(min(c(dat$yg, 0)), max(c(dat$yg, dim$y))), 
       asp=1, xlab="x pixel", ylab="y pixel", type="n", 
       main=unique(dat$site_id), sub="Shading from near camera (dark) to far")
  lines(c(0,rep(c(dim$x,0),each=2)), c(rep(c(0,-dim$y),each=2),0), lty=2)
  cols <- with(dat, colrange[1+round(10*((r-min(r))/diff(range(r))))])
  for(i in 1:nrow(mod$site.model$data)){
    with(dat, lines(c(xg[i],xt[i]), -c(yg[i],yt[i]), col=cols[i], lwd=2))
    with(dat, points(c(xb[i],xt[i]), -c(yb[i],yt[i]), pch=18, cex=0.7))
  }
}



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


###########
