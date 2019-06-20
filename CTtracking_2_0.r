#NOTES ON PREPARING FILES####

#Before digitising, ensure that images are organised into placement_specific folders, 
#named with placement identifiers. Subfolders are allowed. Images must currently have 
#names that are unique across the whole dataset.

#Organise digitisation csv files by type - camera calibration (if used), site calibration
#and animal, with one folder per type. Name csv files with placement (or camera) identifiers
#that are consistent across types.

#When digitising with multiple fields in the sequence_annotation box, use a semicolon separator
#e.g. for a point digitised 0.5 m up a pole at 6 m from the camera, use annotation 0.5;6.

#If reading metadata from a previously prepared csv file, set strignsAsFactors=FALSE.
#ISSUE? fail if time used but in factor format, with mesasge suggesting above solution 
#(or just convert to character silently)


require(magick)
require(dplyr)

setClass("camcal", representation("list"))
setClass("sitecal", representation("list"))
setClass("calibration", representation("list"))

#GENERAL FUNCTIONS#############################################


#read.exif#

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

read.exif <- function(inpath, outpath=inpath, toolpath="C:/Exiftool", return=TRUE, write=FALSE, recursive=TRUE){
  wd <- getwd()
  setwd(toolpath)
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


#list.files.only#

#Wrapper for list.files that over-rides include.dirs argument to return only file names

#INPUT and OUTPUT
# As for list.files (... passes additional arguments to list.files)
list.files.only <- function(path, ...){
  args <- c(path=path, list(...))
  if("full.names" %in% names(args)) fn <- TRUE else fn <- FALSE
  if(fn) args$full.names <- TRUE else args <- c(args, full.names=TRUE)
  fls <-  do.call(list.files, args)
  res <- fls[!file.info(fls)$isdir]
  if(!fn) res <- basename(res)
  res
}


#VIDEO PROCESSING FUNCTIONS#############################################


#extract.frames#

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
# None: creates a set of image files, extracted from video files in inpath, mirroring the original 
#       directory structure.

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


#get.min.metadate#

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

#extract#

#Extracts frames from a single video file
#INPUT
# fps: frame rate for extraction (frames per second)
# file: a character string giving the name of the file to extract from, including path
# outpath: a character string giving the path in which to place extracted images;
# toolpath: a character string giving the path of the folder containing the ffmpeg executable
# suffix.length: the number of leading zeroes to add to the frame files as suffix to the 
#                video file name. By default generates a sequence -001, -002, ...
#OUTPUT
# None: creates a set of image files extracted from file in outpath.

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


#stamptime#

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


#copy.images#

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


#crop#

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
# None. Creates cropped file copies in outpath.

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


#DATA PREP FUNCTIONS#############################################


#split.annotations#

#Splits out multi-field annotations entered as a single field

#INPUT
# dat: a vector of data containing multiple field separated by a given character, sep
# colnames: a vector of the names to assign to output columns
# sep: the character defining field breaks within dat

#OUTPUT
# A dataframe with one column per field from the input data. The function fails if not all 
# entries in dat have the same number of sep characters (implying different numbers of columns),
# or if the number of colnames doesn't equal the number of columns in dat.
split.annotations <- function(dat, colnames=NULL, sep=";"){
  lst <- strsplit(as.character(dat), sep)
  seps <- unique(unlist(lapply(lst, length)))
  
  if(length(seps)>1) stop("Not all annotations have the same number of entries")
  if(is.null(colnames)) colnames <- paste0("X",1:seps) else
  if(seps!=length(colnames))
    stop("Number of column names is not equal to the number of annotations")
  
  d <- data.frame(Reduce(rbind, lst), stringsAsFactors=F)
  d <- type.convert(d, as.is=T)
  names(d) <- colnames
  rownames(d) <- NULL
  d
}


#read.digidat#

#Reads and merges csv files of digitisation data from animaltracker tool.

#- Input path should point to a directory containing the digisation data csv files in its root.
#- The root path must contain ONLY those csv files to be processed.
#- The csv file names are assumed to be site IDs.
#- The function optionally adds metadata from the original images. 
#- In cases where images are a mix of image and video frame, the function also optionally
#  translates x,y pixel positions from image to video scale or vice versa.
#- If EITHER pixel translation OR exifcols are specified (ie trans.xy is not "none" OR exifcols is
#  not NULL) image exif data are needed from the digitised images. In this case, EITHER exifdat must
#  be provided, OR the necessary images must be present within (sub-directories of) path and the
#  function will read exif data from there.
#- IMPORTANT: where image metadata are accessed, all images must have unique names.

#INPUT
# path: name of directory containing all required files (see above).
# exifdat: either a character path to a folder containing images from which to extract exif data, or
#    a dataframe containing the appropriate metadata
# annotations: a character vector of column names to give to annotations from the digitisation data
# pair: whether or not to pair up paired pole digitisation points for calibration (leave as FALSE for animal points)
# exifcols: data columns from exifdata to add to the merged dataframe, in addition
#           to defaults (Directory, CreateDate, ImageHeight and ImageWidth).
# trans.xy: type of pixel translation to apply, with options:
#   "none": no translation
#   "img.to.vid": all image file pixel positions are translated to the video scale
#   "vid.to.img": all video file pixel positions are translated to the image scale

#OUTPUT
# A dataframe of the original digitisation data, with sequence_id column reassigned to give
# unique values to each sequence across the whole dataframe (with original IDs preserved as
# sequence_id_original), plus new column group_id holding values taken from input csv file names.
# Optionally, x,y values are translated from image to video scale or vice versa, with original
# x,y values are preserved x.original,y.original. Also optionally, columns specified by exifcols
# input are added from the image metadata.
read.digidat <- function(path, exifdat=NULL, annotations=NULL, pair=FALSE,
                        exifcols=NULL,
                        trans.xy=c("none", "img.to.vid", "vid.to.img")){
  renumber <- function(x) c(0, cumsum(head(x, -1)!=tail(x, -1)))
  trans.xy <- match.arg(trans.xy)
  
  files <- list.files(path, pattern=".csv", full.names=TRUE, ignore.case=TRUE)
  if(length(files)==0) stop("No csv files found in path")
  df.list <- lapply(files, read.csv, stringsAsFactors=FALSE)
  
  colnames <- lapply(df.list, names)
  n_columns <- unlist(lapply(colnames, length))
  if(length(unique(n_columns)) > 1){
    message("Error: Not all files have the same number of columns - check output data")
    return(data.frame(file=basename(files), n_columns))
  }
  colnames <- matrix(unlist(colnames), ncol=length(colnames))
  if(any(apply(colnames, 1, function(x) length(unique(x))) != 1)){
    message("Error: Not all files have the same column headings - check output data")
    return(data.frame(file=basename(files), t(colnames)))
  }
  classes <- unlist(lapply(df.list, function(df) class(df$sequence_annotation)))
  if(length(unique(classes))>1){
    message("Error: Sequence annotation classes are not consistent across files - check output data")
    return(data.frame(file=basename(files), seq_ann_class=classes))
  }
  
  df <- bind_rows(df.list)
  df <- cbind(df, split.annotations(df$sequence_annotation, annotations))
  df$group_id <- rep(sub(".csv", "", basename(files)), unlist(lapply(df.list, nrow)))
  df$sequence_id_original <- df$sequence_id
  df$sequence_id <- renumber(paste0(df$group_id, df$sequence_id))
  if("pole_id" %in% names(df)) df$pole_id <- paste(df$pole_id, df$group_id, sep="_")

  if(!is.null(exifdat) | trans.xy!="none"){
    if(is.null(exifdat)) stop("exifdat must be provided if trans.xy (pixel translation) is specified")
    
    exifcols <- unique(c("Directory", "CreateDate", "ImageHeight", "ImageWidth", exifcols))
    if(is.character(exifdat)) exifdat <- read.exif(exifdat)
    dirs <- tools::file_path_sans_ext(basename(files))
    if(any(!dirs %in% basename(exifdat$Directory))) stop("Not all csv filenames have matching image directories")
    df$filename <- file.path(dirname(exifdat$Directory[1]),
                           rep(dirs, unlist(lapply(df.list, nrow))),
                           df$filename)

    i <- match(df$filename, exifdat$SourceFile)
    if(any(is.na(i))){
      cat(df$filename[is.na(i)], sep="\n")
      message("Warning: The above digitised files couldn't be found in exif data and were stripped out")
      notna <- !is.na(i)
      df <- df[notna,]
      i <- i[notna]
    }
    exifdat <- exifdat[i, ]
    addn <- data.frame(exifdat[, exifcols])
    names(addn) <- exifcols
    df <- cbind(df, addn)
    if("CreateDate" %in% names(df) & length(unique(nchar(df$CreateDate)))==1)
      df$TimeOfDay <- decimal.time(df$CreateDate)
    rownames(df) <- 1:nrow(df)
  }

  if(trans.xy=="none"){
    df$xdim <- exifdat$ImageWidth
    df$ydim <- exifdat$ImageHeight
  } else{
    if(!"VideoHeight" %in% names(exifdat))
      stop("No video dimension info found in image metadata - must be there if pixel translation is specified (trans.xy!=\"none\"")

    df$x.original <- df$x
    df$y.original <- df$y
    if(trans.xy=="img.to.vid"){
      j <- exifdat$FileSource!="" & !is.na(exifdat$VideoHeight)
      df$x[j] <- with(exifdat[j,], VideoWidth * (df$x[j]-VideoXorigin) / VideoWidthOnImage)
      df$y[j] <- with(exifdat[j,], VideoHeight * (df$y[j]-VideoYorigin) / VideoHeightOnImage)
      df$xdim <- with(exifdat, ifelse(is.na(VideoWidth), ImageWidth, VideoWidth))
      df$ydim <- with(exifdat, ifelse(is.na(VideoHeight), ImageHeight, VideoHeight))
    } else{
      j <- exifdat$FileSource=="" & !is.na(exifdat$VideoHeight)
      df$x[j] <- with(exifdat[j,], VideoXorigin + VideoWidthOnImage*df$x[j] / VideoWidth)
      df$y[j] <- with(exifdat[j,], VideoYorigin + VideoHeightOnImage*df$y[j] / VideoHeight)
      df$xdim <- unique(exifdat[!j,]$ImageWidth)
      df$ydim <- unique(exifdat[!j,]$ImageHeight)
    }
  }
  
  if(pair) df <- make.poledat(df)
  df
}


#decimal.time#

#Converts text time data to decimal time of day. Default format hh:mm:ss, but can handle 
#other separators and minutes and seconds can be missing.

#INPUT
# dat: an array of character times, with hours, minutes and seconds in that order separated by sep.
# sep: the character used to separate time components.

#OUTPUT
# An array of decimal times in hours.

decimal.time <- function(dat, sep=":"){
  dat <- as.character(dat)
  f <- function(x){
    res <- as.numeric(x[1])
    if(length(x)>1) res <- res+as.numeric(x[2])/60
    if(length(x)>2) res <- res+as.numeric(x[3])/60^2
    res/24
  }
  spaces <- unique(grepl(" " , dat))
  if(length(spaces)!=1) stop("Time formats don't seem to be consistent")
  if(spaces) dat <- unlist(lapply(strsplit(dat, " "), function(x) x[2]))
  tt <- strsplit(as.character(dat), sep)
  unlist(lapply(tt, f))
}


#make.poledat#

#Converts a dataframe of digitisation calibration pole data to a "paired up" format, 
#with  one row per pole. Input data must have at least fields x and y (pixel positions), 
#plus either a pole identifier field (pole_id) or an image file name field (filename). 
#If filename is provided but not pole_id, filename is taken to be the pole identifier,
#comined with Directory if this is present. Specific additional fields, when present,
#must use the following names:
# Directory: full path to the directory containing the digitised image
# distance: distance from camera; required for camera calibration
# length: length of pole digitised; required for camera calibration
# height: height of digitised point off the ground; required for site calibration

#INPUT
# dat: dataframe of digitisation data (ideally created by read.digidat).

#OUTPUT
# A dataframe with the two ditisation points per pole arranged in single rows.
# Returns the input data minus x, y and sequence_annotation, plus columns:
#  xb, yb, xt, yt: x and y co-ordinates of pole b(ottom) and t(op) positions digitised
#  hb, ht: actual heights above ground of the digitised pole positions
# A single point per pole can be digitise, but only for site calibration, and only if distance is
# also provided and height=0. Where a single pole is digitised at height 0, xt and yt values are imputed at ht=1.
# Pole records are discarded with a warning if:
#  1. they have non-numeric distance, length or height values;
#  2. they are digitised more than twice;
#  3. they are digitised only once at height > 0;
#  4. conflicting distance values are given for two digitisation points.

make.poledat <- function(dat){

  pairup <- function(dat){
    dat <- dat[order(dat$pole_id, dat$y), ]
    j <- 2*(1:(nrow(dat)/2))
    xy <- cbind(dat[j, c("x","y")], dat[j-1, c("x","y")])
    names(xy) <- c("xb","yb","xt","yt")
    if("height" %in% names(dat))
      xy <- cbind(xy, hb=dat$height[j], ht=dat$height[j-1], length=dat$height[j-1]-dat$height[j])
    dat <- cbind(dat[j,], xy)
    dat[, !names(dat) %in%  c("x","y","height")]
  }

  colnames <- names(dat)
  gotxy <- all(c("x","y") %in% colnames)
  gotpid <- any(c("pole_id", "filename") %in% colnames)
  if(!gotxy | !gotpid) 
    stop("Input dat must have at least columns x, y, and EITHER pole_id OR filename")

  if("height" %in% colnames){
    dat$height <- suppressWarnings(as.numeric(as.character(dat$height)))
    dat <- subset(dat, !is.na(height))
  }
  if("distance" %in% names(dat)){
    dat$distance <- suppressWarnings(as.numeric(as.character(dat$distance)))
    dat <- subset(dat, !is.na(distance))
  }
  if("length" %in% names(dat)){
    dat$length <- suppressWarnings(as.numeric(as.character(dat$length)))
    dat <- subset(dat, !is.na(length))
  }
  
  if(!"pole_id" %in% names(dat)) dat$pole_id <- with(dat, paste(group_id, filename, sep="_"))

  duff2 <- duff3 <- duff4 <- FALSE
  tab <- table(dat$pole_id)
  if("height" %in% names(dat)){
    minh <- tapply(dat$height, dat$pole_id, min)
    duff1 <- tab>2
    duff2 <- (tab==1 & minh>0)
  } else
    duff1 <- tab>2
  if("distance" %in% names(dat))
    duff3 <- with(dat, tapply(distance, pole_id, min) != tapply(distance, pole_id, max))
    
  if(any(duff1 | duff2 | duff3))
    dat <- droplevels(dat[!dat$pole_id %in% names(which(duff1 | duff2 | duff3)), ])
  
  tab <- table(dat$pole_id)
  i <- dat$pole_id %in% names(tab)[tab==1]
  res <- pairup(dat[!i, ])  
  if("height" %in% names(dat)){
    duff4 <- res$hb>=res$ht
    names(duff4) <- res$pole_id
    if(any(duff4, na.rm=TRUE)) res <- droplevels(res[!duff4, ])
  }
  
  solos <- dat[i, ]
  if(nrow(solos)>0 & "distance" %in% names (dat)){
    pxratio <- with(res,  sqrt((xb-xt)^2+(yb-yt)^2) / (ht-hb))
    invd <- 1/res$distance
    relx2 <- (res$xb / res$ImageWidth - 0.5)^2
    mod <- lm(pxratio~invd+relx2-1)
    nd <- data.frame(invd=1/solos$distance, relx2=(solos$x/solos$ImageWidth-0.5)^2)
    solos2 <- solos
    solos2$height <- 1
    solos2$y <- solos$y-predict(mod, newdata=nd)
    res <- rbind(res, pairup(rbind(solos,solos2)))
  }
  res <- res[order(res$pole_id), ]
  
  if(any(duff1 | duff2 | duff3) | any(duff4)){
    message("Warning:\n Some poles were discarded because they...")
    if(any(duff1)){
      message("...were digitised  more than twice:")
      cat(names(which(duff1)), sep="\n")
    }
    if(any(duff2)){
      message("...were digitised only once at height > 0:")
      cat(names(which(duff2)), sep="\n")
    }
    if(any(duff3)){
      message("...had different distances at top and base:")
      cat(names(which(duff3)), sep="\n")
    }
    if(any(duff4)){
      message("...had base height >= top height:")
      cat(names(which(duff4)), sep="\n")
    }
  }
  res
}


#CALIBRATION FUNCTIONS#############################################

#make.camtable#

#Make a lookup table matching sites to camera from a digitisation dataframe

#INPUT
# dat: a dataframe created using read.digidat containing camera information from image metadata
# camcolumns: a vector of column headings used to create camera categories

#OUTPUT
#A dataframe with columns site_id and cam_id, indicating which camera category 
#was deployed at each site
make.camtable <- function(dat, camcolumns=c("Make", "Model", "Megapixels")){
  if(!all(camcolumns %in% names(dat))) stop("Supplied columns do not all exist in dat column headings")
  cat <- caldat[, camcolumns]
  if(class(cat)=="data.frame") cat <- apply(cat, 1, paste, collapse="_")
  tab <- table(caldat$group_id, cat)
  i <- apply(tab, 1, function(x) which(x>0))
  if(class(i)!="integer") stop("Some deployments have more than one camera category")
  data.frame(site_id=unique(caldat$group_id), cam_id=colnames(tab)[i])
}


#cal.cam#

#Creates a camera calibration model

#INPUT
# poledat: data frame of pole digitisation data with (at least) columns:
# distance: pole distances from camera
# length: pole lengths
# xt,yt,xb,yb: x,y pixel positions of pole tops (t) and bases (b) in image
# xdim, ydim: x and y dimensions of each image
# group_id: camera ID code for each record (optional - see below)

#OUTPUT
# A list object of class camcal (ie camera calibration), describing relationship between
# pixel size and radial distance, and x-pixel position and angular distance, with elements:
#  model: quadratic model of FSratio against relative x position (ie focal_length:sensor_size)
#  APratio: ratio of angle to *relative* x pixel position
# If group_id is provided, one model is fitted for each unique camera ID.
# If data are for a single camera, group_id can be omitted

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
  
  if("group_id" %in% names(poledat)){
    cams <- unique(poledat$group_id)
    out <- lapply(cams, function(cam) cal(subset(poledat, group_id==cam)))
    names(out) <- cams
  } else
    out <- list(cam=cal(poledat))
  class(out) <- "calibration"
  out
}


#plot.camcal#

#Show diagnostic plots for camera calibration model

plot.camcal <- function(mod){
  cam <- unique(mod$data$group_id)

  dat <- mod$data
  cols <- grey.colors(11, start=0, end=0.8)
  
  #PLOT POLE:PIXEL RATIO V DISTANCE RELATIONSHIP
  x <- abs(dat$relx)
  i <- round(1 + (x-min(x))*10/diff(range(x)))
  with(dat, plot(distance, length/pixlen, col=cols[i], pch=16, main=cam,
                 ylab="m/pixel", xlab="distance", 
                 sub="Shading from image centre (dark) to edge", cex.sub=0.7))
  FS <- predict(mod$mod, newdata=data.frame(relx=c(0,0.5)))
  dr <- range(dat$distance)
  lines(dr, dr/(FS[1]*mod$dim$y), col=cols[1])
  lines(dr, dr/(FS[2]*mod$dim$y), col=cols[11])
  
  #PLOT POLE IMAGE
  d <- dat$distance
  i <- round(1 + (d-min(d))*10/diff(range(d)))
  plot(c(0,mod$dim$x), c(0,-mod$dim$y), type="n", asp=1, main=cam,
       xlab="x pixel", ylab="y pixel", 
       sub="Shading from near camera (dark) to far", cex.sub=0.7)
  for(p in 1:nrow(dat))
    lines(dat[p,c("xb","xt")], -dat[p,c("yb","yt")], type="l", lwd=2, col=cols[i[p]])
  lines(c(0,rep(c(mod$dim$x,0),each=2)), c(rep(c(0,-mod$dim$y),each=2),0), lty=2)
}

#cal.site#

#Create a site calibration model from data on pole distances from camera and positions within image.
#Pole distances can EITHER be predicted using provided camera calibration model(s), OR be provided as
#a field named distance in input data.

#INPUT
# dat: data frame of pole digitisation data with (at least) columns:
#  xb, yb, xt, yt: x and y co-ordinates of pole b(ottom) and t(op) positions digitised
#  hb, ht: actual heights above ground of the digitised pole positions
#  xdim, ydim: x and y dimensions of each image
#  distance: actual pole distances from camera (required if cmod not provided)
# cmod: a (list of) camera model(s); if multiple models, element names are used for matching
# lookup: a dataframe with (at least) columns cam_id and site_id, mapping cameras to sites
# flex: whether to include additional flexibility in the model (can be difficult to fit)
# minpoles: threshold minimum number of poles needed to fit a model; returns NULL sitecal if less

#OUTPUT
# A list object of class sitecal (ie site calibration), describing relationship between pixel position and distance, 
# with elements:
#  cam.model: the camera calibration model used to predict distances (if any)
#  site.model: non-linear least squares fit of distance (d) to *relative* x and y pixel positions:
#              d ~ b1 / (y-(b2+b3*x))
#  data: the data input to the model
#  dim: the x,y pixel dimensions of the images used for calibration

cal.site <- function(dat, cmod=NULL, lookup=NULL, flex=FALSE, minpoles=3){

  cal <- function(dat, cmod=NULL){
    if(nrow(dat)<minpoles){
      res <- list(cam.model=NULL, site.model=NULL)
      class(res) <- "sitecal"
    } else{
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
      if(!is.null(cmod)){
        FSratio <- predict(cmod$model, newdata = data.frame(relx=dat$relx))
        dat$distance <- FSratio * (dat$ht-dat$hb) * dim$y/dat$pixlen
      } else{
        poledat <- data.frame(dat[, c("distance", "xt","yt","xb","yb","xdim","ydim")])
        poledat$length <- dat$ht-dat$hb
        cmod <- cal.cam(poledat)
      }
      if(flex)
        mod <- try(nls(distance~b1/(rely^b4-(b2+b3*relx)), data=dat, algorithm="port", 
                     start=list(b1=min(dat$distance)/2, b2=min(dat$rely)*0.9, b3=0, b4=1),
                     lower=c(b1=0,b2=0,b3=-Inf,b4=0), 
                     upper=c(b1=Inf,b2=min(dat$rely),b3=Inf,b4=Inf),
                     trace=F )) else
        mod <- try(nls(distance~b1/(rely-(b2+b3*relx)), data=dat, algorithm="port", 
                     start=list(b1=min(dat$distance)/2, b2=min(dat$rely)*0.9, b3=0),
                     lower=c(b1=0,b2=0,b3=-Inf), 
                     upper=c(b1=Inf,b2=min(dat$rely),b3=Inf),
                     trace=F ))
      res <- list(cam.model=cmod, site.model=list(model=mod, data=dat, dim=dim))
      class(res) <- "sitecal"
    }
    res
  }
  
  sites <- unique(dat$group_id)
  if(is.null(cmod)){
    out <- lapply(sites, function(s) cal(subset(dat, group_id==s)))
    } else{
      if(is.null(lookup)) stop("Site-camera lookup table must be provided if camera models are specified")
      if(!all(sites %in% lookup$site_id)) stop("Not all dat$group_id values have a matching value in lookup$site_id")
      if(any(!lookup$cam_id[match(sites, lookup$site_id)] %in% names(cmod))) stop("Can't find all the necessary camera models in cmod - check lookup table and names(cmod)")
      out <- lapply(sites, function(s)
        cal(subset(dat, group_id==s), cmod[[lookup$cam_id[match(s, lookup$site_id)]]])
      )
    }
  names(out) <- sites
  nofits <- unlist(lapply(out, function(m) is.null(m$site.model)))
  if(any(nofits)){
    cat(sites[nofits], sep="\n")
    message("Warning: The above site(s) had too few poles to fit a model")
  }
  class(out) <- "calibration"
  out
}

#plot.sitecal#

#Show diagnostic plots for site calibration model

plot.sitecal <- function(mod){
  site <- unique(mod$site.model$data$group_id)

  if(is.null(mod$site.model)){
    message(paste("Model without a fit not plotted:", site, "\n"))
  } else{
    dim <- as.list(apply(mod$site.model$data[,c("xdim","ydim")],2,unique))
    dat <- mod$site.model$data
    colrange <- grey.colors(11, start=0, end=0.8)
    
    #PLOT DISTANCE V Y-PIXEL RELATIONSHIP
    cols <- with(dat, colrange[1+round(10*((relx-min(relx))/diff(range(relx))))])
    mxx <- max(max(dat$rely),1.5)
    with(dat, plot(rely, distance, col=cols, pch=16, xlim=c(0,mxx), ylim=c(0, 1.5*max(distance)),
                   xlab="Relative y pixel position", ylab="Distance from camera",
                   main=site, 
                   sub="Shading from image left (dark) to right edge", cex.sub=0.7))
    if(class(mod$site.model$model)=="nls"){
      sq <- seq(0, mxx, len=100)
      lines(sq, predict.r(mod$site.model$model, -0.5, sq), col=colrange[1])
      lines(sq, predict.r(mod$site.model$model, 0, sq), col=colrange[6])
      lines(sq, predict.r(mod$site.model$model, 0.5, sq), col=colrange[11])
    }
    
    #PLOT POLE IMAGE
    relht <- with(dat, (1-ht) / (ht-hb))
    xl <- with(dat, xt + relht*(xt-xb))
    yl <- with(dat, yt + relht*(yt-yb))
    plot(c(0, dim$xdim), -c(0, dim$ydim), 
         asp=1, xlab="x pixel", ylab="y pixel", type="n", 
         main=site, sub="Shading from near camera (dark) to far", cex.sub=0.7)
    lines(c(0,rep(c(dim$xdim,0),each=2)), c(rep(c(0,-dim$ydim),each=2),0), lty=2)
    cols <- with(dat, colrange[1+round(10*((distance-min(distance))/diff(range(distance))))])
    for(i in 1:nrow(mod$site.model$data)){
      with(dat, lines(c(xg[i],xl[i]), -c(yg[i],yl[i]), col=cols[i], lwd=2))
      with(dat, points(c(xb[i],xt[i]), -c(yb[i],yt[i]), pch=18, cex=0.7, col=2))
    }
  }
}

#plot.calibration#

#Show diagnostic plots for site calibration model

plot.calibration <- function(mods){
  lapply(mods, plot)
}

#DATA SUMMARY FUNCTIONS#############################################


#predict.r#

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


#predict.pos#

#Predicts position relative to camera given image pixel positions and site calibration models 

#INPUT
# dat: a dataframe of digitisation data containing (at least) columns:
#  x,y: x and y pixel positions for each digitised point
#  xdim,ydim: x and y pixel dimensions of each image; must be consistent for each group_id
#  group_id: site identifier
# mod: a named list of site calibration models; names must be matched by group_id column in dat

#OUTPUT
# A dataframe of original data with radial and angular distances from camera appended.

predict.pos <- function(dat, mod){

  required <- c("x","y","xdim","ydim","group_id")
  if(!all(required %in% names(dat))) 
    stop(paste("dat must contain all of these columns:", paste(required, collapse=" ")))

  sites <- unique(animdat$group_id)
  gotmodel <- sites %in% names(smods)
  nullmodel <- names(smods)[unlist(lapply(smods, function(m) is.null(m$site.model)))]
  gotmodel[match(nullmodel, sites)] <- FALSE
  if(!all(gotmodel)){
    cat(sites[!gotmodel], sep="\n")
    message("Warning: The above sites had no matching site calibration model and were stripped out")
    dat <- subset(dat, group_id %in% sites[gotmodel])
    sites <- sites[gotmodel]
  }

  multidim <- lapply(with(dat, tapply(xdim, group_id, unique)), length)>1 |
              lapply(with(dat, tapply(ydim, group_id, unique)), length)>1
  if(any(multidim)){
    message("Warning:\n There is more than one unique value per site for xdim and/or ydim in site(s):")
    cat(names(which(multidim)), sep="\n")
  }

  res <- lapply(sites, function(s){
    dt <- subset(dat, group_id==s)
    cm <- mod[[s]]$cam.model
    sm <- mod[[s]]$site.model$model
    data.frame(dt, radius=predict.r(sm, dt$x/dt$xdim-0.5, dt$y/dt$ydim),
               angle=cm$APratio * (dt$x/dt$xdim-0.5))
  })
  res <- bind_rows(res)
  tab <- table(res$sequence_id)
  res$frame_count <- sequence(tab)
  res
}


#seq.data#

#Creates a dataframe of image-to-image changes for each row in dat

#INPUT
# A dataframe dat produced by predict.pos with columns:
#   sequence_id: sequence identifier
#   x,y: x and y pixel positions of dixitised points
#   radius, angle: predicted radial and angular distances from camera

#OUTPUT
# A dataframe of:
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


#seq.summary#

#Summarise sequences to generate speed of movement estimates

#INPUT
# Dataframe dat produced by predict.pos with (at least) columns:
#  sequence_id: sequence identifiers
#  CreateDate: character date and time of image creation, formated as %Y:%m:%d %H:%M:%S
#  imgcount: the number of images in each sequence

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

seq.summary <- function(dat){
  calc.mov <- function(dat){
    n <- as.numeric(table(dat$sequence_id))
    dat <- seq.data(dat)
    pixdiff <- with(dat, tapply(pixdiff, sequence_id, sum, na.rm=T) )
    mvdist <- with(dat, tapply(displacement, sequence_id, sum, na.rm=T) )
    tm <- strptime(dat$CreateDate, format="%Y:%m:%d %H:%M:%S", tz="UTC")
    mvtime <- tapply(tm, dat$sequence_id, function(x) as.numeric(diff(range(x)), units="secs"))
    i <- n>1 & n<11
    mntime <- sum(mvtime[i]) / (sum(n) - length(n))
    time <- mvtime
    time[i] <- mntime * (n[i]-1)
    
    cbind(dat[dat$imgcount==1, !(names(dat) %in% c("imgcount","pixdiff","displacement","d.angle"))],
          pixdiff=pixdiff,
          dist=mvdist,
          timediff=mvtime,
          time=time,
          speed=mvdist/time,
          frames=n
    )
  }
  dat <- dat[order(dat$sequence_id), ]
  n <- table(dat$sequence_id)
  i <- dat$sequence_id %in% names(n)[n==1]
  calc.mov(subset(dat, !i))
}
