setClass("camcal", representation("list"))
setClass("sitecal", representation("list"))
require(data.table)
library(magick)

# Code to extract stills from camera trap videos - UNFINISHED

# need to install ffmpeg - as far as I understood it's a command line software
# I had to google how to instal it and found a good tutorial on youtube

#Download ffmpeg build
#Unzip and rename resulting folder FFmpeg
#Copy FFmpeg folder to an appropriate location (e.g root directory C:\)
#Open system environment variables (search from start menu)
#In the system variables box, highlight the Path entry and click Edit
#In the new dialogue, click New and type or paste the path of the FFmpeg/bin folder (e.g. C:\FFmpeg\bin)
#OK
#Video tutorial here: https://www.youtube.com/watch?v=pHR3ttH5t-w

#Crop image files in infolder and move to outfolder 
#exf: dataframe of exif data from infolder
move.images <- function(infolder, outfolder, exf=NULL, crop=TRUE, imgtype=".JPG", suffix="-01."){
  if(is.null(exf)) exf <- read.exif(infolder, subdirs=FALSE)
  stills <- list.files(infolder, imgtype)
  imgs <- image_read(paste0(infolder, "/", stills))
  if(crop){
    imgW <- unique(subset(exf, FileType=="JPEG")$ImageWidth)
    imgH <- unique(subset(exf, FileType=="JPEG")$ImageHeight)
    vidW <- unique(subset(exf, FileType=="MP4")$ImageWidth)
    vidH <- unique(subset(exf, FileType=="MP4")$ImageHeight)
    Hmargin <- (imgH-vidH)/2
    Wmargin <- (imgW-vidW)/2
    imgs <- image_crop(imgs, paste0(vidW,"x",vidH,"+",Wmargin,"+",Hmargin))
  }
  for(i in 1:length(stills))
    image_write(imgs[i], paste0(outfolder, "/", gsub("\\.", suffix, stills[i])))
}

extract.frames <- function(infolder, outfolder, 
                           fpath="C:/FFmpeg", epath="C:/Users/Rowcliffe.M/Documents/APPS/ExifTool",
                           vidtype=".MP4", imgtype=".JPG", fps=0.5, delete.first=FALSE){
  fullfiles <- list.files(infolder, vidtype)
  files <- gsub(vidtype, "", fullfiles)
  exf <- read.exif(infolder, subdirs=FALSE)
  dt <- strptime(exf$CreateDate, "%Y:%m:%d %H:%M:%S", tz="UTC")[exf$FileType=="MP4"]
  
  for(i in 1:length(files)){
    wd <- getwd()
#Extract stills
    setwd(fpath)
    cmd <- paste0("ffmpeg -i ", infolder, "/", fullfiles[i], 
                  " -r ", fps, " ",
                  outfolder, "/", files[i], "-%02d.jpg")
    shell(cmd)

#Add CreateDate field to metadata
    setwd(epath)
    newfiles <- list.files(outfolder, files[i], full.names=TRUE)
    if(delete.first==TRUE){
      file.remove(newfiles[1])
      newfiles <- newfile[-1]
    }
    newdates <- as.character(dt[i]+(0:(length(newfiles)-1))/fps)
    for(j in 1:length(newfiles)){
      (cmd <- paste0('exiftool -createdate="', newdates[j-1], '" ',
                    newfiles[j], " ",
                    "-overwrite_original"))
      shell(cmd)
    }
  }
  setwd(wd)
}

inf <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna"
exf <- read.exif(inf)
times <- strptime(exf$CreateDate[1:34], "%Y:%m:%d %H:%M:%S", tz="UTC")
(fps <- 1 / as.numeric(mean(times[2*(1:17)] - times[2*(1:17)-1])))

inf <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test"
ouf <- "C:/Users/rowcliffe.m/Documents/CameraTrapping/REM/Calibration/Mytool/AndreLanna/test/stills"
extract.frames(inf, ouf, fps=0.1)
move.images(inf, ouf)
exf <- read.exif(ouf)
View(exf)
exf$CreateDate



  library(imager)
  library(stringr)
  origem_arquivos_video  <- choose.dir(, "choose folder with original files") 
  destino_arquivos_foto  <- choose.dir(, "choose folder to send jpgs")
  'from here it executes the code at the selected files'
  myFiles <- list.files(path = origem_arquivos_video, pattern="*.avi",
                        ignore.case=TRUE, recursive = FALSE, include.dirs = FALSE,
                        full.names = TRUE)
  myFiles <- sub(" ", "%20", myFiles)
  for(vd in myFiles){
    vdframes <- load.video(vd, maxSize=1, skip.to=0, frames=NULL,
                           fps=2, extra.args="", verbose=FALSE)
    'fps = 2 means that you will get 2 frames per second'
    total_frames <- depth(vdframes)
    for(n in 1:total_frames){
      nome_arquivo <- str_c(destino_arquivos_foto,"\\",
                            sub(".*/", "", sub(".AVI", "", paste(vd,n))),".jpeg")
      save.image(frame(vdframes, n), nome_arquivo)
    }
  }  

#######################################################################################################
#read.exif
#######################################################################################################
#Runs command line ExifTool to extract metadata of all image/video/audio files within a folder
#See for a list of supported formats https://www.sno.phy.queensu.ca/~phil/exiftool
#Requires standalone executable exiftool.exe to be present on your computer, available at above link
#Unzip and rename the exiftool(-k).exe file to exiftool.exe
#
#INPUT
# infolder: a character string giving the path of the folder containing files to process
# exifpath: a character string giving the path of the folder containing exiftool.exe
#Path and file names in infolder and outfile must be free of spaces
#Files withinin subfolders of infolder are also processed
#
#OUTPUT
#A dataframe of metadata. A csv file of the data called metadata.csv is also created (or 
#overwritten without warning) within infolder
read.exif <- function(infolder, outfolder=NULL, return=TRUE, write=FALSE, subdirs=TRUE,
                      exifpath="C:/Users/Rowcliffe.M/Documents/APPS/ExifTool"){
  wd <- getwd()
  setwd(exifpath)
  if(is.null(outfolder)) outfolder <- infolder
  outfile <- paste0(outfolder, "/metadata.csv")
  if(subdirs==TRUE) sbd<-"-r" else sbd <- ""
  cmd <- paste("exiftool", sbd, "-csv", infolder, ">", outfile)
  shell(cmd)
  setwd(wd)
  res <- read.csv(outfile, stringsAsFactors = FALSE)
  if(write==FALSE) file.remove(outfile)
  if(return==TRUE) return(res)
}

#######################################################################################################
#matrix.data.frame
#######################################################################################################
#Converts matrix to dataframe, converting columns to numeric when possible
matrix.data.frame <- function(mat){
  f <- function(x){
    y <- suppressWarnings(as.numeric(x))
    if(any(is.na(y))) x else y
  }
  df <- data.frame(mat, stringsAsFactors=FALSE)
  data.frame(lapply(df,f))
}

#######################################################################################################
#merge.csv
#######################################################################################################
#Merges all csv files within given directory and renumbers sequence_id field sequentially
#INPUT
# path: text string defining directory containing files to merge
# sitecol: text string giving name of column containing site identifier; if not "site_id" the column name is changed to this
#OUTPUT
#None, but creates a new file "./merged/merged.csv" within path 
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

#######################################################################################################
#decimal.time
#######################################################################################################
#Converts text time data to decimal time of day. Default format hh:mm:ss, but can handle 
#other separators and minutes and seconds can be missing.
decimal.time <- function(dat, sep=":"){
  f <- function(x){
    res <- as.numeric(x[1])
    if(length(x)>1) res <- res+as.numeric(x[2])/60
    if(length(x)>2) res <- res+as.numeric(x[3])/60^2
    res/24
  }
  tt <- strsplit(as.character(dat), sep)
  unlist(lapply(tt, f))
}

#######################################################################################################
#read.poledat
#######################################################################################################
#Reads pole digitisation data for either camera or site calibration
#INPUT
# file: character string giving name of tracker output csv file to read (including path if not in working directory)
# fields: single character string giving column headings for the sequence_annotation field, with names separated by sep
# sep: single character separating the column names in fields
#
#Use the following column names within fields when the relevant information is present:
# cam_id: camera identifier
# site_id: site identifier
# pole_id: pole identifier; if not provided in annotations, frame_number is taken to be the pole identifier
# distance: distance from camera; required for camera calibration
# length: length of pole digitised; required for camera calibration
# height: height of digitised point off the ground; required for site calibration
#OUTPUT
#A dataframe with the two ditisation points per pole arranged in single rows.
#Returns the input data minus x, y and sequence_annotation, plus columns:
# xb, yb, xt, yt: x and y co-ordinates of pole b(ases) and t(ops)
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
  
  if("site_id" %in% names(dat2))
    dat2$pole_id <- paste(dat2$site_id, dat2$frame_number, sep="_") else
  if("cam_id" %in% names(dat2))
    dat2$pole_id <- paste(dat2$cam_id, dat2$pole_id, sep="_") else
  if(!"pole_id" %in% names(dat2))
    dat2$pole_id <- dat2$frame_number
      
  tab <- table(dat2$pole_id)
  duff <- !tab==2
  if(any(duff)){
    dat2 <- droplevels(dat2[!dat2$pole_id %in% names(which(duff)), ])
    warning(paste("Some poles did not have exactly 2 points digitised and were removed:\n", 
                  paste(names(which(duff)), collapse=" ")))
  }
  if("distance" %in% col.names){
    duff <- with(dat2, tapply(distance, pole_id, min) != tapply(distance, pole_id, max))
    if(any(duff))
      stop(paste("Some poles did not have matching distance for top and base:\n",
                 paste(names(which(duff)), collapse=" ")))
  }
  
  dat2 <- dat2[order(dat2$pole_id, dat2$y), ]
  i <- 2*(1:(nrow(dat2)/2))
  xy <- cbind(dat2[i, c("x","y")], dat2[i-1, c("x","y")])
  names(xy) <- c("xb","yb","xt","yt")
  if("height" %in% col.names)
    xy <- cbind(xy, hb=dat2$height[i], ht=dat2$height[i-1])
  res <- cbind(dat2[i, !(names(dat2) %in% c("height","x","y"))], xy)
  
  if("height" %in% col.names){
    duff <- res$hb>=res$ht
    if(any(duff)){
      warning(paste("Some poles had base height >= top height and were removed:\n", 
                    paste(res$pole_id[duff], collapse=" ")))
      res <- droplevels(res[!duff, ])
    }
  }
  res
}

#######################################################################################################
#cal.cam
#######################################################################################################
#Creates a camera calibration model
#INPUT
# poledat: data frame of pole digitisation data with (at least) columns:
# distance: pole distances from camera
# length: pole lengths
# xt,yt,xb,yb: x,y pixel positions of pole tops (t) and bases (b) in image
# xdim, ydim: x and y dimensions of each image
# cam_id: camera ID code for each record (optional - see below)
#
#OUTPUT
#A named list of objects of class camcal (camera calibration), 
#describing relationship between pixel size and distance.
# model: quadratic model of FSratio against relative x position
# APratio: ratio of angle to *relative* x pixel position
#If cam_id is provided, one model is fitted for each unique camera ID.
#If data are for a single camera, cam_id can be omitted
cal.cam <- function(poledat){
  #Internal function fits a single camera calibration model
  cal <- function(dat){
    dim <- as.list(apply(dat[,c("xdim","ydim")], 2, unique))
    if(length(unlist(dim))>2) stop("There is more than one unique value per camera for xdim and/or ydim in poledat")
    names(dim) <- c("x","y")
    dat$pixlen <- with(dat, sqrt((xb-xt)^2 + (yb-yt)^2))
    dat$relx <- apply(dat[c("xb","xt")], 1, mean)/dim$x-0.5
    FSratio <- with(dat, distance*pixlen/(length*dim$y))
    APratio <- mean(with(dat, (acos(1-length^2/(2*(distance^2+(length/2)^2)))*dim$x/pixlen)))
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

#######################################################################################################
#plot.camcal
#######################################################################################################
#Show diagnostic plots for camera calibration model
#1. pole length / pixel length ratio against real distance
#2. Image view with poles
plot.camcal <- function(mod){
  dat <- mod$data
  cols <- grey.colors(11, start=0, end=0.8)

#PLOT POLE:PIXEL RATIO V DISTANCE RELATIONSHIP
  x <- abs(dat$relx)
  i <- round(1 + (x-min(x))*10/diff(range(x)))
  with(dat, plot(distance, length/pixlen, col=cols[i], pch=16, main=unique(dat$cam_id),
                 ylab="m/pixel", xlab="distance", 
                 sub="Shading from image centre (dark) to edge", cex.sub=0.7))
  FS <- predict(mod$mod, newdata=data.frame(relx=c(0,0.5)))
  dr <- range(dat$distance)
  lines(dr, dr/(FS[1]*mod$dim$y), col=cols[1])
  lines(dr, dr/(FS[2]*mod$dim$y), col=cols[11])

#PLOT POLE IMAGE
  d <- dat$distance
  i <- round(1 + (d-min(d))*10/diff(range(d)))
  plot(c(0,mod$dim$x), c(0,-mod$dim$y), type="n", asp=1, main=unique(dat$cam_id),
       xlab="x pixel", ylab="y pixel", 
       sub="Shading from near camera (dark) to far", cex.sub=0.7)
  for(p in 1:nrow(dat))
    lines(dat[p,c("xb","xt")], -dat[p,c("yb","yt")], type="l", lwd=2, col=cols[i[p]])
  lines(c(0,rep(c(mod$dim$x,0),each=2)), c(rep(c(0,-mod$dim$y),each=2),0), lty=2)
}

#######################################################################################################
#cal.site
#######################################################################################################
#Fits site calibration models
#INPUT
# cmod: a named list of camera calibration models created using cam.cal (can be a single model but still needs to be a named list)
# dat: a dataframe of pole digitisation data created using read.poledat
# lookup: a dataframe with (at least) columns site_id and cam_id matching camera models to site
#Values in lookup$cam_id must be matched in names(cmod)
#
#OUTPUT
#A named list of objects of type sitecal, each with components:
# cam.model: a camera calibriation model created using cal.cam
# site.model: a list with components:
#  model: a fitted nls object describing the relationship between real distance and imagepixel position
#  data: a dataframe containing the data used to fit the model
#  dim: the pixel dimensions of the site's images
cal.site <- function(cmod, dat, lookup){
  cal <- function(cmod, dat){
    dim <- as.list(apply(dat[,c("xdim","ydim")], 2, unique))
    if(length(unlist(dim))>2) stop("There is more than one unique value per site for xdim and/or ydim in dat")
    names(dim) <- c("x","y")
    xdiff <- (dat$xt-dat$xb)
    ydiff <- (dat$yt-dat$yb)
    dat$pixlen <- sqrt(xdiff^2 + ydiff^2)
    dat$xg <- with(dat, xb - xdiff*hb/(ht-hb))
    dat$yg <- with(dat, yb - ydiff*hb/(ht-hb))
    dat$xm <- with(dat, xt + xdiff*(1-ht)/(ht-hb))
    dat$ym <- with(dat, yt + ydiff*(1-ht)/(ht-hb))
    dat$rely <- dat$yg/dim$y
    dat$relx <- (dat$xb+dat$xt)/(2 * dim$x) - 0.5
    FSratio <- predict(cmod$model, newdata = data.frame(relx=dat$relx))
    dat$r <- FSratio * (dat$ht-dat$hb) * dim$y/dat$pixlen
    mod <- try(nls(r~b1/(rely-(b2+b3*relx)), data=dat, algorithm="port", 
                   start=list(b1=min(dat$r)/2, b2=min(dat$rely)*0.9, b3=0),
                   lower=c(b1=0,b2=0,b3=-Inf), 
                   upper=c(b1=Inf,b2=min(dat$rely),b3=Inf),
                   trace=F
                   )
               )
    res <- list(cam.model=cmod, site.model=list(model=mod, data=dat, dim=dim))
    class(res) <- "sitecal"
    res
  }
  
  sites <- unique(dat$site_id)
  if(!all(sites %in% lookup$site_id)) stop("Not all dat$site_id values have a matching value in lookup$site_id")
  if(any(!lookup$cam_id[match(sites, lookup$site_id)] %in% names(cmod))) stop("Can't find all the necessary camera models in cmod - check lookup table and names(cmod)")
  out <- lapply(sites, function(s) 
    cal(cmod[[lookup$cam_id[match(s, lookup$site_id)]]], subset(dat, site_id==s)))
  names(out) <- sites
  out
}

#######################################################################################################
#predict.r
#######################################################################################################
#Predict radial distance from camera given pixel positions
#INPUT
# mod: a sitecal objext (site calibration model, produced using cal.site(...))
# relx: x pixel position relative to the centre line
# rely: y pixel position relative to the top edge
#
#OUTPUT
#A vector numeric radii.
#Note, units depend on the units of pole height above ground used to calibrate the site model
predict.r <- function(mod, relx, rely){
  res <- predict(mod, newdata=data.frame(relx=relx, rely=rely))
  res[res<0] <- Inf
  res
}

#######################################################################################################
#plot.sitecal
#######################################################################################################
#Show diagnostic plots for site calibration model:
#1. Real distance against y pixel position
#2. Image view with poles
plot.sitecal <- function(mod){
  dim <- mod$site.model$dim
  dat <- mod$site.model$data
  colrange <- grey.colors(11, start=0, end=0.8)
  
#PLOT DISTANCE V Y-PIXEL RELATIONSHIP
  cols <- with(dat, colrange[1+round(10*((relx-min(relx))/diff(range(relx))))])
  with(dat, plot(rely, r, col=cols, pch=16, xlim=c(0,max(1.5,rely)), ylim=c(0, max(r)),
                 xlab="Relative y pixel position", ylab="Distance from camera",
                 main=unique(dat$site_id), 
                 sub="Shading from image left (dark) to right edge", cex.sub=0.7))
  if(class(mod$site.model$model)=="nls"){
    sq <- seq(0, max(1.5,dat$rely), len=100)
    lines(sq, predict.r(mod$site.model$model, -0.5, sq), col=colrange[1])
    lines(sq, predict.r(mod$site.model$model, 0, sq), col=colrange[6])
    lines(sq, predict.r(mod$site.model$model, 0.5, sq), col=colrange[11])
  }

#PLOT POLE IMAGE
  with(dat, plot(c(min(0,xg,xm), max(dim$x,xg,xm)), 
                   -c(max(dat$yg, dim$y), min(ym, 0)),
                 asp=1, xlab="x pixel", ylab="y pixel", type="n",
                 main=unique(dat$site_id), sub="Shading from near camera (dark) to far", cex.sub=0.7)
  )
  lines(c(0,rep(c(dim$x,0),each=2)), c(rep(c(0,-dim$y),each=2),0), lty=2)
  cols <- with(dat, colrange[1+round(10*((r-min(r))/diff(range(r))))])
  for(i in 1:nrow(mod$site.model$data)){
    with(dat, lines(c(xg[i],xm[i]), -c(yg[i],ym[i]), col=cols[i], lwd=2))
    with(dat, points(c(xb[i],xt[i]), -c(yb[i],yt[i]), pch=18, col=3, cex=0.7))
  }
}
#######################################################################################################
#predict.pos
#######################################################################################################
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
  
  required <- c("x","y","xdim","ydim","site_id","sequence_annotation","sequence_id")
  if(!all(required %in% names(dat))) 
    stop(paste("dat must contain all of these columns:", paste(required, collapse=" ")))

  col.names <- unlist(strsplit(fields, sep))
  if(gregexpr(sep,fields)[[1]][1]==-1) notes <- dat$sequence_annotation else
    notes <- strsplit(dat$sequence_annotation, sep)
  
  if(any(unlist(lapply(notes, length))!=length(col.names)))
    stop(paste("fields gives", length(col.names), "headings but some annotations do not have this many entries"))
  
  notes <- matrix(unlist(notes), nrow=nrow(dat), byrow=T, dimnames=list(NULL, col.names))
  dat2 <- cbind(matrix.data.frame(notes), subset(dat, select=-c(sequence_annotation)))
  dat2 <- droplevels(subset(dat2, is.na(suppressWarnings(as.numeric(as.character(dat2$species))))))
  
  sites <- unique(dat2$site_id)
  matched <- sites %in% names(mod)
  if(!all(matched)){
    warning(paste("Records for the following site(s) had no matching site calibration model and were discarded:\n"),
                  paste(sites[!matched], collapse=" "))
    dat2 <- subset(dat2, site_id %in% sites[matched])
    sites <- sites[matched]
  }

  xdimvals <- with(dat2, tapply(xdim, site_id, unique))
  ydimvals <- with(dat2, tapply(ydim, site_id, unique))
  if(length(unlist(xdimvals))>length(sites) | length(unlist(ydimvals))>length(sites)) 
    warning(paste("There is more than one unique value per site for xdim and/or ydim in", file))

  res <- lapply(sites, function(s){
    dt <- subset(dat2, site_id==s)
    cm <- mod[[s]]$cam.model
    sm <- mod[[s]]$site.model$model
    data.frame(dt, radius=predict.r(sm, dt$x/dt$xdim-0.5, dt$y/dt$ydim),
               angle=cm$APratio * (dt$x/dt$xdim-0.5))
  })
  res <- as.data.frame(rbindlist(res))
}

############################################################
#Data summary Functions
############################################################

#######################################################################################################
#seq.data
#######################################################################################################
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

#######################################################################################################
#seq.summary
#######################################################################################################
#Summarise sequences
#INPUT
#Dataframe dat produced by predict.pos with columns:
# ...
#interval: time between frames within sequences

#OUTPUT
#A list of dataframes:
# $trigdat, trigger data, containing original data for single frame sequences plus:
#   radius, angle: radius and angle of trigger position
# $movdat, movement data, containing original data for multi-frame sequences plus:
#   radius, angle: radius and angle of position in first frame of each image
#   pixdiff: total pixel distance traveled across image
#   dist: total distance travelled over ground (units depend on site calibration units)
#   time: time taken (diff[range{frame.number}]*interval)
#   speed: travel speed (dist/time)
#   frames: number of frames digitised
seq.summary <- function(dat, interval){
  calc.mov <- function(dat){
    n <- as.numeric(table(dat$sequence_id))
    dat2 <- seq.data(dat)
    pixdiff <- with(dat2, tapply(pixdiff, sequence_id, sum, na.rm=T) )
    mvdist <- with(dat2, tapply(displacement, sequence_id, sum, na.rm=T) )
    mvtime <- unlist(lapply(tapply(dat$frame_number, dat$sequence_id, range), diff)) * interval
    cbind(dat2[dat2$imgcount==1, !(names(dat2) %in% c("imgcount","pixdiff","displacement","d.angle"))],
          pixdiff=pixdiff,
          dist=mvdist,
          time=mvtime,
          speed=mvdist/mvtime,
          frames=n
    )
  }
  dat <- dat[order(dat$sequence_id), ]
  n <- table(dat$sequence_id)
  i <- dat$sequence_id %in% names(n)[n==1]
  list(trigdat=subset(dat, i), movdat=calc.mov(subset(dat, !i)))
}
