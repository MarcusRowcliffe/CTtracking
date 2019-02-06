require(magick)
require(dplyr)

#######################################################################################################
#extract.frames
#######################################################################################################
#Extracts frames at a given frame rate from video files, and optionally adds a timestamp to the
#metadata, reconstructed from original video time stamp and frame position.
#
#Optionally calls copy.images (see below), for cases where you also want to transfer images
#to the video frame directory (ignored if no image files exist in a folder).
#Optionally crops copied images (if any) to the same field of view as video frames.
#Optionally operates recursively on all subdirectories within inpath as well as the root. 

#If outpath folder doesn't exist it is created, otherwise the existing folder is over-written, with
#a warning and option to abort before this happens. If outpath is a folder name without path, this folder
#is created or over-written within the current working directory.

#NB Some file formats adjust their date metadata according to the time zone of the current computer,
#which can lead to incorrect timestamps on the resulting images. If this happens, you can either set 
#computer time zone to that in which the videos were taken while processing, or use the time.offset argument.

#The function calls two command line apps: ffmpeg and exiftool.
#For installation of exiftool, see read.exif function help.
#For installation of exifpro:
# 1. download the build here: https://ffmpeg.zeranoe.com/builds
# 2. extract the zip
# 3. rename resulting folder ffmpeg
# 4. move ffmpeg folder to C:/ (or other folder if preferred)

#INPUT
# fps: frame rate for extraction (frames per second)
# inpath: a character string giving the path of the folder containing video files to process
# outpath: a character string giving the path of the folder in which to place extracted images
# filetypes: character vector giving video file types to be processed (case insensitive)
# fpath: a character string giving the path of the folder containing the ffmpeg executable
# epath: a character string giving the path of the folder containing the exiftool executable
# copy.jpegs: whether to additioanlly copy images (JPEG files) from inpath to outpath
# stamp.time: whether to add timestamps to the video frame files; if TRUE, creates metadata field CreateDate
# time.offset: seconds to be added to timestamps in metadata of video frame files, if timestamp is TRUE
# suffix.length: number of digits to add to frame file names as suffix 
#               (eg VIDEO_FILE.MP4 -> VIDEO_FILE-001.jpg, VIDEO_FILE-002.jpg, ...)
# recursive: whether to process subdirectories recursively, or only the root inpath
# ...: additional arguments to pass to copy.images 
#      (eg image versus video resolution, see below; used if copy==TRUE)

#OUTPUT
# None: creates a set of image files, extracted from video files in inpath, mirroring the original directory structure.

extract.frames <- function(fps, inpath=NULL, outpath="frames", filetypes=c("MP4", "AVI"),
                              fpath="C:/ffmpeg/bin", epath="C:/exiftool",
                              copy.jpegs=FALSE, stamp.time=FALSE, time.offset=0, 
                              suffix.length=3, recursive=TRUE, ...){

  if(is.null(inpath)) inpath <- getwd()
  if(dirname(outpath)==".") outpath <- file.path(getwd(), outpath)
  if(dir.exists(outpath)){
    res <- readline("Outpath already exists and will be over-written. Do you want to proceed [y/n]? ")
    if(tolower(res)!="y") return()
    unlink(outpath, recursive = T)
  }
  message("Reading metadata...")
  vexf <- read.exif(inpath, toolpath=epath, recursive=recursive)
  dirs <- list.dirs(inpath)
  outdirs <- paste0(outpath, sub(inpath, "", dirs))
  for(path in outdirs) dir.create(path)
  outpaths <- paste0(outpath, sub(inpath, "", vexf$Directory))
  isvid <- grepl(paste(filetypes, collapse="|"), vexf$FileType)
  if(any(isvid)){
    for(i in 1:sum(isvid)){
      file <- vexf$SourceFile[isvid][i]
      message("Extracting frames from ", file, " (", i, " of ", sum(isvid), ")")
      extract(fps, file, outpaths[isvid][i], fpath, suffix.length)
    }
    if(stamp.time==TRUE){
      message("Updating video frame metadata...")
      stamptime(fps, outpath, vexf, epath, time.offset)
    }
    if(copy.jpegs==TRUE)
      for(d in 1:length(dirs)){
        message("Copying images from ", dirs[d], " (", d, " of ", length(dirs), ")")
        copy.images(dirs[d], outdirs[d], vexf, filetypes, ...)
      }
  }
}

#######################################################################################################
#get.min.metadate
#######################################################################################################
#Returns the minimum dates in each row of an exif dataframe containing character format
#date-times with ":" separators

#INPUT
# exf: a dataframe with at least one column containing character date-times and with 
#      those column names containing "Date"
#OUTPUT
# A charcter vector of date-times in format %Y:%m:%d %H:%M:%S

get.min.metadate <- function(exf){
  f <- function(dates){
    dates <- strptime(sub("\\s*\\+.*", "", dates), "%Y:%m:%d %H:%M:%S", tz="UTC")
    strftime(dates[which(dates==min(dates, na.rm=TRUE))][1], "%Y:%m:%d %H:%M:%S")
  }
  j <- grep("Date", names(exf))
  strptime(apply(exf[,j], 1, f), "%Y:%m:%d %H:%M:%S", tz="UTC")
}  

#######################################################################################################
#extract
#######################################################################################################
#Extracts frames from a single video file
#INPUT
# fps: frame rate for extraction (frames per second)
# file: a character string giving the name of the file to extract from, including path
# outpath: a character string giving the path in which to place extracted images;
# toolpath: a character string giving the path of the folder containing the ffmpeg executable
# suffix.length: the number of characters to add to the frame files as suffix to the video file name.
#                By default enerates a sequence -001, -002, ...

extract <- function(fps, file, outpath, toolpath="C:/FFmpeg/bin", suffix.length=2){
  wd <- getwd()
  setwd(toolpath)
  basefile <- tools::file_path_sans_ext(basename(file))
  outfile <- paste0("\"", file.path(outpath, paste0(basefile, "-%0", suffix.length, "d.jpg")), "\"")
  file <- paste0("\"", file, "\"")
  cmd <- paste0("ffmpeg -loglevel error -i ", file, " -vf fps=fps=", fps, ":start_time=-5 -vsync vfr ", outfile)
  shell(cmd)
  setwd(wd)
}

#######################################################################################################
#stamptime
#######################################################################################################
#Adds a time stamp to images extracted from video. Calculates stamp values from video start
#time and the frame rate used for extraction. Function requires .ExifTool_config file to be
#placed in the ExifTool folder to define bespoke metadata fields.

#INPUT
# fps: the frame rate used for extraction (frames per second)
# path: a character string giving the path containing the images to process
# vexf: a dataframe containing the exif data of video files from which frames were extracted
# toolpath: a character string giving the path of the folder containing the exiftool executable
# offset: seconds to be added to timestamps in file metadata
# recursive: whether to process subdirectories of path
 
#OUTPUT
#None: Adds the following metadata tags to the affected files:
# CreateDate: date and time created
# VideoCreateDate: date and time the originating video was created
# FrameNumber: the frame position within the sequence extracted from a given video
# CreateTimeOffset: time since the beginning of the sequence
# FrameExtractRate: the rate at which frames were extracted (frames per second)

stamptime <- function(fps, path, vexf, toolpath="C:/Exiftool", offset=0, recursive=TRUE){
  wd <- getwd()
  setwd(toolpath)
  ffls <- list.files.only(path, full.names=TRUE, recursive=recursive)
  ffls <- sub("\\(", "\\\\(", ffls)
  ffls <- sub("\\)", "\\\\)", ffls)
  dirs <- unique(dirname(ffls))
  fls <- basename(ffls)
  exf <- data.frame(SourceFile=ffls)
  splitfiles <- strsplit(tools::file_path_sans_ext(fls), "-")
  names <- unlist(lapply(splitfiles, function(x) x[1]))
  nums <- as.numeric(unlist(lapply(splitfiles, function(x) x[2])))-1
  i <- match(names, tools::file_path_sans_ext(vexf$FileName))
  vcd <- get.min.metadate(vexf)[i]
  exf$FrameNumber <- nums
  exf$VideoCreateDate <- strftime(vcd+offset, "%Y:%m:%d %H:%M:%S")
  exf$FrameExtractRate <- fps
  exf$CreateTimeOffset <- nums/fps
  exf$CreateDate <- strftime(vcd+exf$CreateTimeOffset+offset, "%Y:%m:%d %H:%M:%S")
  mfile <- paste(wd, "metadata.csv", sep="/")
  dfile <- paste(wd, "dirs.txt", sep="/")
  write.csv(exf, mfile, row.names=FALSE)
  write.table(dirs, dfile, row.names=FALSE, col.names = FALSE, quote=FALSE)

  cmd <- paste0("exiftool -csv=", paste0("\"", mfile, "\""),
                " -@ ", paste0("\"", dfile, "\""),
                " -overwrite_original")
  shell(cmd)
  file.remove(mfile)
  file.remove(dfile)
  setwd(wd)
}

#######################################################################################################
#copy.images
#######################################################################################################
#Copies all image (JPEG) files in inpath to outpath, for use when each camera trap trigger resulted
#in an image immediately followed by a video. Optionally crops the resulting images to give the same 
#field of view as the equivalent video frames; if not cropped, cropping information is instead
#added to the metadata of image copies (see OUTPUT for details).
#The cropping process assumes that video field of view is the same or smaller than image, 
#and that all images and videos in a given directory come from a single camera setting.
#If inpath does not contain both videos and images, or if either videos or images are of mixed 
#dimensions, the function does nothing.

#INPUT
# inpath: a character string giving the path of the folder containing files to process
# outpath: a character string giving the path of the folder into which files are copied
# exf: dataframe of exif data from files in inpath
# vidtypes: video file types to look for in association with images
# toolpath: a character string giving the path of the folder containing the exiftool executable
# suffix: text to be added to the copied file names before the extension
# crop: whether to crop images before copying 
# relative.res: resolution of image files relative to resolution of the equivalent video frames
# x.offset, y.offset: number of image pixels by which the video field of view is displaced

#OUTPUT
# Creates a copy of images, EITHER:
#  Cropped to conform field of view to that of the equivalent video frames,
# OR:
#  Uncropped but with the following metadata added decribing how image and video fields of view
#  map onto one another:
#   VideoWidth, VideoHeight: video pixel resolution
#   VideoWidthOnImage, VideoHeightOnImage: video frame size measured in image pixels
#   VideoXOrigin, VideoYOrigin: pixel position on image of video frame origin

copy.images <- function(inpath, outpath, exf=NULL, vidtypes=c("MP4", "AVI"), toolpath="C:/exiftool", 
                        suffix="", crop=FALSE, relative.res=1, x.offset=0, y.offset=0){
  if(is.null(exf)) exf <- read.exif(inpath)
  iexf <- subset(exf, FileType=="JPEG" & Directory==inpath)
  vexf <- subset(exf, FileType %in% vidtypes & Directory==inpath)
  if(nrow(iexf)>0 & nrow(vexf)>0){
    imgW <- unique(iexf$ImageWidth)/relative.res
    imgH <- unique(iexf$ImageHeight)/relative.res
    vidW <- unique(vexf$ImageWidth)
    vidH <- unique(vexf$ImageHeight)
    icongruent <- length(imgW)==1 & length(imgH)==1
    vcongruent <- length(vidW)==1 & length(vidH)==1
    if(!icongruent | !vcongruent){
      if(!icongruent & !vcongruent)
        warning("Images and videos have mixed dimensions - directory ignored: ", inpath) else
      if(!icongruent)
        warning("Images have mixed dimensions - directory ignored: ", inpath) else
      if(!vcongruent)
        warning("Videos have mixed dimensions - directory ignored: ", inpath)
      return()
    }
    H <- relative.res*vidH
    W <- relative.res*vidW
    Horigin <- relative.res*(imgH-vidH)/2+y.offset
    Worigin <- relative.res*(imgW-vidW)/2+x.offset
    if(crop){
      crop(inpath, outpath, iexf, dimensions=list(W=W, H=H, Worigin=Worigin, Horigin=Horigin), suffix)
    } else{
      wd <- getwd()
      setwd(toolpath)
      file.copy(iexf$SourceFile, outpath, copy.date=TRUE)
      outpath <- paste0("\"", outpath, "\"")
      cmd <- paste0("exiftool -m -videoxorigin=", Worigin, " -videoyorigin=", Horigin, 
                    " -videowidthonimage=", W, " ", " -videoheightonimage=", H, 
                    " -videowidth=", vidW, " ", " -videoheight=", vidH, 
                    " ", outpath, " -overwrite_original")
      shell(cmd)
      setwd(wd)
    }
  }
}

#######################################################################################################
#crop
#######################################################################################################
#Creates a cropped copy of image (JPEG) files to conform to the field of view of video frames from
#the same camera setting.

#INPUT
# inpath: a character string giving the path of the folder containing image file to process
# outpath: a character string giving the path of the folder into which cropped files are copied
# exf: dataframe of exif data from files in inpath; if NULL these data first extracted internally
# dimensions: a named list of parameters mapping images to video frames:
#               W, H: the x,y pixel dimensions of the video frame on the image
#               Worigin, Horigin: the x,y pixel position on image of the video frame origin
#             When NULL, dimensions are extracted from the image metadata (calamity if these don't exist).
#             All images in a directory must have the same dimension data.
# suffix: text to be added to the copied file names before the extension
#One of dimensions and exf must be provided.


#OUTPUT
# Cropped file copies.

crop <- function(inpath, outpath, exf=NULL, dimensions=NULL, suffix=""){
  if(!dir.exists(outpath)) dir.create(outpath)
  if(is.null(dimensions)){
    if(is.null(exf)) exf <- read.exif(inpath)
    exf <- subset(exf, FileType=="JPEG" & Directory==inpath & !is.na(VideoXorigin))
    images <- exf$SourceFile
    W <- unique(exf$VideoWidthOnImage)
    H <- unique(exf$VideoHeightOnImage)
    Worigin <- unique(exf$VideoXorigin)
    Horigin <- unique(exf$VideoYorigin)
    if(length(H)>1 | length(W)>1 | length(Horigin)>1 | length(Worigin)>1)
      stop("Image-video scaling metadata not unique - must be consistent for all files within a directory")
  } else{
    images <- list.files.only(inpath, pattern=".jpg|.JPG", full.names=TRUE)
    W <- dimensions$W
    H <- dimensions$H
    Worigin <- dimensions$Worigin
    Horigin <- dimensions$Horigin
  }
  if(Horigin<0){
    H <- H+Horigin
    Hmargin <- 0
  } else Hmargin <- Horigin
  if(Worigin<0){
    W <- W+Worigin
    Wmargin <- 0
  } else Wmargin <- Worigin
  imgs <- image_read(images)
  imgs <- image_crop(imgs, paste0(W,"x",H,"+",Wmargin,"+",Hmargin))
  suffix <- paste0(suffix,".")
  for(i in 1:length(imgs))
    image_write(imgs[i], paste0(outpath, "/", gsub("\\.", suffix, basename(images)[i])))
}

#######################################################################################################
#read.exif
#######################################################################################################
#Runs command line ExifTool to extract metadata of all image/video/audio files within a folder
#For a list of supported formats see https://www.sno.phy.queensu.ca/~phil/exiftool
#Requires standalone executable exiftool.exe to be present on your computer, available at above link
#Unzip and rename the exiftool(-k).exe file to exiftool.exe

#INPUT
# inpath: a character string giving the path of the folder containing files to process
# outpath: a character string giving the path of the folder in which to place results file (defaults to inpath)
# toolpath: a character string giving the path of the folder containing exiftool.exe
# return: should the function return the results as a dataframe
# write: should the function return the results as a new .csv file within outpath
# recursive: whether subdirectories of inpath should also be searched for images

#OUTPUT
#Optionally (depending on return input) a dataframe of metadata. 
# A csv file of the data called metadata.csv is also temporarily created 
# (or overwritten without warning) within outpath, and optionally preserved
# (depending on write input)

read.exif <- function(inpath, outpath=NULL, toolpath="C:/Exiftool", return=TRUE, write=FALSE, recursive=TRUE){
  wd <- getwd()
  setwd(toolpath)
  if(is.null(outpath)) outpath <- inpath
  outfile <- paste0(outpath, "/metadata.csv")
  outf <- paste0("\"", outfile, "\"")
  inpath <- paste0("\"", inpath, "\"")
  if(recursive==TRUE) sbd<-"-r" else sbd <- ""
  cmd <- paste("exiftool", sbd, "-csv", inpath, ">", outf)
  shell(cmd)
  setwd(wd)
  res <- read.csv(outfile, stringsAsFactors = FALSE)
  if(write==FALSE) file.remove(outfile)
  if(return==TRUE) return(res)
}

#######################################################################################################
#list.files.only
#######################################################################################################
#Wrapper for list.files that over-rides include.dirs argument to return only file names
list.files.only <- function(dir, ...){
  args <- c(path=dir, list(...))
  if("full.names" %in% names(args)) fn <- TRUE else fn <- FALSE
  if(fn) args$full.names <- TRUE else args <- c(args, full.names=TRUE)
  fls <-  do.call(list.files, args)
  res <- fls[!file.info(fls)$isdir]
  if(!fn) res <- basename(res)
  res
}

#######################################################################################################
#read.digidat
#######################################################################################################
#Reads and merges csv files of digitisation data from animaltracker tool.

#- Input path should point to a directory containing the digisation data csv files, and if EITHER
#  pixel translation OR exifcols are specified, the digitised images.
#- The root directory must contain ONLY those csv files to be processed.
#- The csv file names are assumed to be site IDs.
#- Optionally adds metadata from the original images. 
#- In cases where images are a mix of image and video frame, also optionally translates x,y pixel
#  positions from image to video scale or vice versa.
#- IMPORTANT: where image metadata are accessed, all images in the path must have unique names

#INPUT
# path: name of directory containing all required files (see above)
# exifcols: data columns from image metadata to add to the merged dataframe
# trans.xy: type of pixel translation to apply, with options:
#   "none": no translation
#   "img.to.vid": all image file pixel positions are translated to the video scale
#   "vid.to.img": all video file pixel positions are translated to the image scale

#OUTPUT
# A dataframe of the original digitisation data, with sequence_id column reassigned to give unique values
# to each sequence across the whole dataframe (with original IDs preserved as sequence_id_original), plus
# new column site_id holding values taken from input csv file names. Optionally, x,y values are translated from
# image to video scale or vice versa, with original x,y values are preserved x.original,y.original. Also
# optionally, columns specified by exifcols input are added from the image metadata.
read.digidat <- function(path,
                        exifcols=c("SourceFile", "Directory", "CreateDate", "ImageHeight", "ImageWidth"),
                        trans.xy=c("none", "img.to.vid", "vid.to.img")){
  renumber <- function(x){
    res <- diff(x)
    res[res!=0] <- 1
    c(0, cumsum(res))
  }
  
  trans.xy <- match.arg(trans.xy)

  message("Merging csv files... ", appendLF=FALSE)
  files <- list.files(path, pattern=".csv", full.names=TRUE, ignore.case=TRUE)
  df.list <- lapply(files, read.csv, stringsAsFactors=FALSE)
  
  colnames <- lapply(df.list, names)
  if(length(unique(unlist(lapply(colnames, length)))) > 1)
    stop("Not all files have the same number of columns")
  colnames <- matrix(unlist(colnames), ncol=length(colnames))
  if(any(apply(colnames, 1, function(x) length(unique(x))) != 1))
    stop("Not all files have the same column headings")
  
  df <- bind_rows(df.list)
  df$sequence_id_original <- df$sequence_id
  df$sequence_id <- renumber(df$sequence_id)
  df$site_id <- rep(sub(".csv", "", basename(files)), unlist(lapply(df.list, nrow)))
  message("DONE.")
  
  if(!is.null(exifcols) | trans.xy!="none"){
    message("Reading and appending metadata... ")
    exifdat <- read.exif(path)
    exifdat <- exifdat[match(df$filename, exifdat$FileName), ]
    df <- cbind(df, exifdat[, exifcols])
    rownames(df) <- 1:nrow(df)
  }

  if(trans.xy!="none"){
    message("Translating pixels... ", appendLF=FALSE)
    if(!"VideoHeight" %in% names(exifdat))
      stop("No video info found in image metadata - must be there if pixel translation is specified (trans.xy!=\"none\"")

    df$x.original <- df$x
    df$y.original <- df$y
    if(trans.xy=="img.to.vid"){
      j <- exifdat$FileSource!=""
      df$x[j] <- with(exifdat[j,], VideoHeight * (df$x[j]-VideoXorigin) / VideoHeightOnImage)
      df$y[j] <- with(exifdat[j,], VideoWidth * (df$y[j]-VideoYorigin) / VideoWidthOnImage)
    } else{
      j <- exifdat$FileSource==""
      df$x[j] <- with(exifdat[j,], VideoXorigin + VideoHeightOnImage*df$x[j] / VideoHeight)
      df$y[j] <- with(exifdat[j,], VideoYorigin + VideoWidthOnImage*df$y[j] / VideoWidth)
    }
    message("DONE.")
  }
  df
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
  res <- cbind(dat2[i, !(names(dat2) %in% c("height","x","y"))], xy)
  
  if("height" %in% col.names){
    duff <- res$hb>=res$ht
    if(any(duff)){
      warning(paste("Some poles had base height >= top height and were removed:", 
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
#poledat: data frame of pole digitisation data with (at least) columns:
# distance: pole distances from camera
# length: pole lengths
# xt,yt,xb,yb: x,y pixel positions of pole tops (t) and bases (b) in image
# xdim, ydim: x and y dimensions of each image
# cam_id: camera ID code for each record (optional - see below)

#OUTPUT
#An object of class camcal (camera calibration), 
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
#Show diagnostic plots for site calibration model
plot.sitecal <- function(mod){
  dim <- as.list(apply(smod$OCGSV09$site.model$data[,c("xdim","ydim")],2,unique))
  dat <- mod$site.model$data
  colrange <- grey.colors(11, start=0, end=0.8)
  
  #PLOT DISTANCE V Y-PIXEL RELATIONSHIP
  cols <- with(dat, colrange[1+round(10*((relx-min(relx))/diff(range(relx))))])
  with(dat, plot(rely, r, col=cols, pch=16, xlim=c(0,1.5), ylim=c(0, 1.5*max(r)),
                 xlab="Relative y pixel position", ylab="Distance from camera",
                 main=unique(dat$site_id), 
                 sub="Shading from image left (dark) to right edge", cex.sub=0.7))
  if(class(mod$site.model$model)=="nls"){
    sq <- seq(0, 1.5, len=100)
    lines(sq, predict.r(mod$site.model$model, -0.5, sq), col=colrange[1])
    lines(sq, predict.r(mod$site.model$model, 0, sq), col=colrange[6])
    lines(sq, predict.r(mod$site.model$model, 0.5, sq), col=colrange[11])
  }
  
  #PLOT POLE IMAGE
  plot(c(min(c(dat$xg, 0)), max(c(dat$xg, dim$xdim))),
       -c(min(c(dat$yg, 0)), max(c(dat$yg, dim$ydim))), 
       asp=1, xlab="x pixel", ylab="y pixel", type="n", 
       main=unique(dat$site_id), sub="Shading from near camera (dark) to far", cex.sub=0.7)
  lines(c(0,rep(c(dim$xdim,0),each=2)), c(rep(c(0,-dim$ydim),each=2),0), lty=2)
  cols <- with(dat, colrange[1+round(10*((r-min(r))/diff(range(r))))])
  for(i in 1:nrow(mod$site.model$data)){
    with(dat, lines(c(xg[i],xt[i]), -c(yg[i],yt[i]), col=cols[i], lwd=2))
    with(dat, points(c(xb[i],xt[i]), -c(yb[i],yt[i]), pch=18, cex=0.7))
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
  dat2 <- subset(dat2, is.na(suppressWarnings(as.numeric(as.character(dat2$species)))))
  
  sites <- unique(dat2$site_id)
  if(!any(sites %in% names(mod))) stop("Not all records have a matching site calibration model")
  
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
#Dataframe of image-to-image changes for each row in dat

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
