setClass("camcal", representation("list"))
setClass("sitecal", representation("list"))
require(data.table)

#Converts matrix to dataframe, converting columns to numeric when possible
matrix.data.frame <- function(mat){
  f <- function(x){
    y <- suppressWarnings(as.numeric(x))
    if(any(is.na(y))) x else y
  }
  df <- data.frame(mat, stringsAsFactors=FALSE)
  data.frame(lapply(df,f))
}

#Merges all csv files within given directory and renumbers sequence_id field sequentially
# path: text string defining directory containing files to merge
# sitecol: text string giving name of column containing site identifier; if not "site_id" it is the column name is changed
merge.csv <- function(path, sitecol="site_id"){
  renumber <- function(x){
    res <- diff(x)
    res[res!=0] <- 1
    c(0, cumsum(res))
  }

  files <- list.files(path, pattern=".csv", full.names=TRUE, ignore.case=TRUE)
  df.list <- lapply(files, read.csv)

  colnames <- lapply(df.list, names)
  if(length(unique(unlist(lapply(colnames, length)))) > 1)
    stop("Not all files have the same number of columns")
  colnames <- matrix(unlist(colnames), ncol=length(colnames))
  if(any(apply(colnames, 1, function(x) length(unique(x))) != 1))
    stop("Not all files have the same column headings")
  
  df <- rbindlist(df.list)
  df$sequence_id_original <- df$sequence_id
  df$sequence_id <- renumber(df$sequence_id)
  if(sitecol!="site_id") names(df)[names(df)==sitecol] <- "site_id"
  df$pole_id <- paste(df$site_id, df$frame_number, sep="_")
  mpth <- paste0(pth,"/merged")
  dir.create(mpth)
  write.csv(df, paste0(mpth,"/merged.csv"), row.names=FALSE)
}

#Reads pole digitisation data for either camera or site calibration
# file: character value giving name of tracker output csv file to read (including path if not in working directory)
# fields: column headings for the sequence_annotation field, given as single text string with values separated by sep
# sep: single character separating the column names in fields
#
#Use the following column names within fields when the relevant information is present:
# cam_id: camera identifier
# site_id: site identifier
# pole_id: pole identifier; if not provided, frame_number is taken to be the pole identifier
# distance: distance from camera; required for camera calibration
# length: length of pole digitised; required for camera calibration
# height: height of digitised point off the ground; required for site calibration
#Records are discarded if they have non-numeric distance, length or height values
read.poledat <- function(file, fields, sep=";"){
  dat <- read.csv(file, stringsAsFactors=FALSE)
  col.names <- unlist(strsplit(fields, sep))
  if(gregexpr(sep,fields)[[1]][1]==-1) notes <- dat$sequence_annotation else
    notes <- strsplit(dat$sequence_annotation, sep)
  
  if(any(unlist(lapply(notes, length))!=length(col.names)))
    stop(paste("fields gives", length(col.names), "headings but some annotations do not have this many entries"))

  notes <- matrix(unlist(notes), nrow=nrow(dat), byrow=T, dimnames=list(NULL, col.names))
  dat2 <- cbind(matrix.data.frame(notes), subset(dat, select=-c(sequence_annotation)))
  
  if("height" %in% names(dat2)){
    dat2$height <- suppressWarnings(as.numeric(as.character(dat2$height)))
    dat2 <- subset(dat2, !is.na(height))
  }
  if("distance" %in% names(dat2)){
    dat2$height <- suppressWarnings(as.numeric(as.character(dat2$distance)))
    dat2 <- subset(dat2, !is.na(distance))
  }
  if("length" %in% names(dat2)){
    dat2$height <- suppressWarnings(as.numeric(as.character(dat2$length)))
    dat2 <- subset(dat2, !is.na(length))
  }

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
  i <- 2*(1:(nrow(dat2)/2))
  xy <- cbind(dat2[i, c("x","y")], dat2[i-1, c("x","y")])
  names(xy) <- c("xb","yb","xt","yt")
  if("height" %in% col.names)
    xy <- cbind(xy, hb=dat2$height[i], ht=dat2$height[i-1])
  cbind(dat2[i, !(names(dat2) %in% c("height","x","y"))], xy)
}

#poledat: data frame of pole digitisation data with columns:
# pole_id: pole ID codes
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
cal.cam <- function(poledat){
  #Internal function fits a single camera calibration model
  cal <- function(dat){
    dim <- as.list(apply(dat[,c("xdim","ydim")], 2, unique))
    if(length(unlist(dim))>2) stop("There is more than one unique value per camera for xdim and/or ydim in poledat")
    names(dim) <- c("x","y")
    dat$pixlen <- with(dat, sqrt((xb-xt)^2 + (yb-yt)^2))
    dat$relx <- apply(dat[c("xb","xt")], 1, mean)/dim$x-0.5
    FSratio <- with(dat, distance*pixlen/(length*dim$y))
    APratio <- mean(with(dat, (acos(1-length^2/(2*(distance^2+(length/2)^2)))/pixlen)))
    mod <- lm(FSratio~I(relx^2), data=dat)
    res <- list(model=mod, APratio=APratio, dim=dim, data=dat)
    class(res) <- "camcal"
    res
  }

  if(class(poledat) != "data.frame") 
    stop("poledat must be a dataframes")
  required <- c("xb", "yb", "xt", "yt", "xdim", "ydim", "distance", "length")
  if(!all(required %in% names(poledat))) 
    stop(paste("poledat must contain all of these columns:", paste(required, collapse=" ")))

  if("cam_id" %in% names(poledat)){
    cams <- unique(poledat$cam_id)
    out <- lapply(cams, function(cam) cal(subset(poledat, cam_id==cam)))
    names(out) <- cams
  } else
    out <- list(cam=cal(poledat))
  out
}

plot.camcal <- function(mod){
  dat <- mod$data
  cols <- grey.colors(11, start=0, end=0.8)

#PLOT POLE:PIXEL RATIO V DISTANCE RELATIONSHIP
  x <- abs(dat$relx)
  i <- round(1 + (x-min(x))*10/diff(range(x)))
  with(dat, plot(distance, length/pixlen, col=cols[i], pch=16, main=mod$dim$cam_id,
                 ylab="m/pixel", xlab="distance", 
                 sub="Shading from image centre (dark) to edge", cex.sub=0.7))
  FS <- predict(mod$mod, newdata=data.frame(relx=c(0,0.5)))
  dr <- range(dat$distance)
  lines(dr, dr/(FS[1]*mod$dim$y), col=cols[1])
  lines(dr, dr/(FS[2]*mod$dim$y), col=cols[11])

#PLOT POLE IMAGE
  d <- dat$distance
  i <- round(1 + (d-min(d))*10/diff(range(d)))
  plot(c(0,mod$dim$x), c(0,-mod$dim$y), type="n", asp=1, main=mod$dim$cam_id,
       xlab="x pixel", ylab="y pixel", 
       sub="Shading from near camera (dark) to far", cex.sub=0.7)
  for(p in 1:nrow(dat))
    lines(dat[p,c("xb","xt")], -dat[p,c("yb","yt")], type="l", lwd=2, col=cols[i[p]])
  lines(c(0,rep(c(mod$dim$x,0),each=2)), c(rep(c(0,-mod$dim$y),each=2),0), lty=2)
}

cal.site <- function(cmod, dat, lookup=NULL){
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
    sites <- unique(dat$site_id)
    if(is.null(lookup) & length(sites)>1) stop("lookup table must be provided if dat records are site-specific")
    if(!all(sites %in% lookup$site_id)) stop("Not all dat$site_id values have a matching value in lookup$site_id")
    if(any(!lookup$cam_id[match(sites, lookup$site_id)] %in% names(cmod))) stop("Can't find all the necessary camera models in cmod - check lookup table and names(cmod)")
    out <- lapply(sites, function(s) 
      cal(cmod[[lookup$cam_id[match(s, lookup$site_id)]]], subset(dat, site_id==s)))
    names(out) <- sites
  } else{
    if(length(cmod)>1) warning("Multiple camera models provided but no dat$site_id column: first camera model applied\n Is that OK?")
    out <- cal(cmod[[1]], dat)
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
                 sub="Shading from image left (dark) to right edge", cex.sub=0.7))
  sq <- seq(0, 1.5, len=100)
  lines(sq, predict.r(mod$site.model$model, -0.5, sq), col=colrange[1])
  lines(sq, predict.r(mod$site.model$model, 0, sq), col=colrange[6])
  lines(sq, predict.r(mod$site.model$model, 0.5, sq), col=colrange[11])

#PLOT POLE IMAGE
  plot(c(min(c(dat$xg, 0)), max(c(dat$xg, dim$x))),
       -c(min(c(dat$yg, 0)), max(c(dat$yg, dim$y))), 
       asp=1, xlab="x pixel", ylab="y pixel", type="n", 
       main=unique(dat$site_id), sub="Shading from near camera (dark) to far", cex.sub=0.7)
  lines(c(0,rep(c(dim$x,0),each=2)), c(rep(c(0,-dim$y),each=2),0), lty=2)
  cols <- with(dat, colrange[1+round(10*((r-min(r))/diff(range(r))))])
  for(i in 1:nrow(mod$site.model$data)){
    with(dat, lines(c(xg[i],xt[i]), -c(yg[i],yt[i]), col=cols[i], lwd=2))
    with(dat, points(c(xb[i],xt[i]), -c(yb[i],yt[i]), pch=18, cex=0.7))
  }
}

#Predicts position relative to camera given image pixel positions and site calibration models 
#INPUT
# file: text string giving name of tracker file containing data to process 
# mod: named list of site calibration models; names must be matched by site_id column in file
# fields: column headings for the sequence_annotation field, given as single text string with values separated by sep
#         Must contain at least "species"
# sep: single character separating the column names in fields
#OUTPUT
#dataframe of original data with radial and angular distances from camera appended
predict.pos <- function(file, mod, fields, sep=";"){
  dat <- read.csv(file, stringsAsFactors=FALSE)
  col.names <- unlist(strsplit(fields, sep))
  if(gregexpr(sep,fields)[[1]][1]==-1) notes <- dat$sequence_annotation else
    notes <- strsplit(dat$sequence_annotation, sep)
  
  if(any(unlist(lapply(notes, length))!=length(col.names)))
    stop(paste("fields gives", length(col.names), "headings but some annotations do not have this many entries"))
  
  notes <- matrix(unlist(notes), nrow=nrow(dat), byrow=T, dimnames=list(NULL, col.names))
  dat2 <- cbind(matrix.data.frame(notes), subset(dat, select=-c(sequence_annotation)))
  dat2 <- subset(dat2, is.na(suppressWarnings(as.numeric(as.character(dat2$species)))))
  
  sites <- unique(dat2$site_id)
  if(!any(sites %in% names(mod))) stop("Not all records have a matching site calibration model")
  
  res <- lapply(sites, function(s){
    dt <- subset(dat2, site_id==s)
    cm <- mod[[s]]$cam.model
    sm <- mod[[s]]$site.model$model
    data.frame(dt, radius=predict.r(sm, dt$x/cm$dim$x-0.5, dt$y/cm$dim$y),
               angle=cm$APratio * (dt$x - cm$dim$x/2))
  })
  res <- as.data.frame(rbindlist(res))
}

############################################################
#Data summary Functions
############################################################

#Dataframe of image-to-image changes for each row in dat:
#INPUT
#Dataframe dat produced by predict.pos with columns:
#   sequence_id
#   x,y
#   radius, angle
#OUTPUT
#Dataframe of:
#   imgcount: image number within sequence
#   pixdiff: pixel displacement within image
#   displacement: estimated linear distance moved
#   d.angle: change in angle from camera
seq.data <- function(dat){
  coseqn <- function(r1,r2,theta) sqrt(r1^2+r2^2-2*r1*r2*cos(abs(theta)))
  imgcount <-  sequence(table(dat$sequence_id))
  pixdiff <- c(NA, sqrt(diff(dat$x)^2 + diff(dat$y)^2))
  pixdiff[imgcount==1] <- NA
  d.angle <- c(NA, diff(dat$angle))
  d.angle[imgcount==1] <- NA
  displacement <- coseqn(dat$radius[-nrow(dat)], dat$radius[-1], d.angle[-1])
  cbind(dat, imgcount=imgcount, pixdiff=pixdiff, displacement=c(NA,displacement), d.angle=d.angle)
}

#Summarise sequences
#INPUT
#Dataframe dat produced by predict.pos with columns:
# ...
#interval: time between frames within sequences

#OUTPUT
#Dataframe of original data plus:
#pixdiff=pixdiff,
#   dist: total displacement
#   time: time taken
#   speed: dist/seconds
#   n: number of images in sequence
seq.summary <- function(dat, interval){
  calc.mov <- function(dat){
    n <- as.numeric(table(dat$sequence_id))
    dat2 <- seq.data(dat)
    pixdiff <- with(dat2, tapply(pixdiff, sequence_id, sum, na.rm=T) )
    mvdist <- with(dat2, tapply(displacement, sequence_id, sum, na.rm=T) )
    mvtime <- (n-1) * interval
    cbind(dat2[dat2$imgcount==1, !(names(dat2) %in% c("imgcount","pixdiff","displacement","d.angle"))],
          pixdiff=pixdiff,
          dist=mvdist,
          time=mvtime,
          speed=mvdist/mvtime,
          frames=n
    )
  }

#  dat <- posdat
  n <- table(dat$sequence_id)
  i <- dat$sequence_id %in% names(n)[n==1]
  list(trigdat=subset(dat, i), movdat=calc.mov(subset(dat, !i)))
}
