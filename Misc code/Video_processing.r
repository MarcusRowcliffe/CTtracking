#VIDEO PROCESSING FUNCTIONS#############################################
#install.exiftool#

#Downloads latest version of exiftool Windows executable from 
#exiftool.org and prepares it for use locally.

#INPUT
# dir: character string giving the directory in which to place exiftool

#DETAILS
#When dir is NULL (the default), exiftool is placed in a folder called exiftool
#in the current R library, indentified using .libPaths(). A folder named exiftool 
#is created if it doesn't already exist. An alternative to this function
#is to download manually. For this, download ExifTool from exiftool.org, unzip
#and rename the exiftool(-k).exe file to exiftool.exe, and place it in a relevant
#folder.

install.exiftool <- function(dir=NULL){
  if(is.null(dir)) dir <- file.path(.libPaths()[1], "exiftool")
  if(!dir.exists(dir)) dir.create(dir)
  zipout <- file.path(dir, "temp.zip")
  ver <- readLines("https://exiftool.org/ver.txt", warn=FALSE)
  website <- paste0("https://exiftool.org/exiftool-", ver, ".zip")
  utils::download.file(website, zipout)
  utils::unzip(zipout, exdir=dir)
  file.remove(zipout)
  filenm <- file.path(dir, "exiftool.exe")
  if(file.exists(filenm)) file.remove(filenm)
  renm <- file.rename(list.files(dir, full.names = TRUE), filenm)
  if(renm) paste("exiftool.exe successfully downloaded to", dir)
}

#read.exif#

#Extract metadata from image files (*.jpg)

#INPUT
# path: a single character string giving a path to folder or file
# fields: a character vector of field names to extract, optionally named (see details)
# tagfield: a single character string giving the name of a field containing user tags
# toolpath: a character string giving the path of the folder containing exiftool.exe
# ...: additional arguments passed to split.tags

#OUTPUT
#A dataframe of image metadata, unless tagfield is specified and split.tags throws
#up problems, in which case a dataframe is returned of the records identified as 
#problematic by split.tags.

#DETAILS
#Runs command line executable exiftool, which must be present locally (see 
#install.exiftool). Metadata is extracted from all jpeg images in the directory 
#defined by path and all its subdirectories.
#
#The default fields are required in subsequent processing functions, where these
#exact names are expected. If the necessary metadata exist under a different 
#field headings, the fields vector can be named, in which case element names are
#assigned to the resulting field names. To extract all available fields, set
#fields="". Specified fields that don't exist in the metadata are ignored with a
#warning. If no valid fields are found the function fails. 
#
#If tagfield is provided, this field will be separated into 
#multiple columns in the output using split.tags, with additional arguments 
#passed to this function (specifically tagsep and valsep, defining the tag and 
#value separation strings). If you only want separated tagfield data, fields can
#be set to NULL to suppress extraction of additional fields.
#
#By default (toolpath=NULL), the function expects to find the exiftool.exe file 
#in a folder named exiftool within the current R library, where the 
#install.exiftool function places it.

read.exif <- function(path, type="jpg",
                      fields=c("Directory", "FileName", "DateTimeOriginal", "ImageWidth", "ImageHeight"), 
                      tagfield=NULL, toolpath=NULL, ...){
  path <- normalizePath(path)
  if(length(path)>1) stop("path must be a string pointing to a single directory or file")
  if(is.null(toolpath)) toolpath <- file.path(.libPaths()[1], "exiftool")
  if(!file.exists(file.path(toolpath,"exiftool.exe"))) stop(paste("Can't find", file.path(toolpath,"exiftool.exe")))
  if(!file.exists(path)) stop("path not found")
  if(dir.exists(path)) nfiles <- length(list.files(path, recursive = TRUE)) else nfiles <- 1
  if(nfiles==0) stop(paste("No files found in", path))
  
  if(is.null(fields) & is.null(tagfield)) stop("No fields or tagfield defined")
  ff <- fields
  if(!"" %in% fields){
    if(!is.null(tagfield))
      if(!tagfield %in% fields) fields <- c(fields, tagfield)
    ff <- paste(paste0("-", fields), collapse=" ")
  }
  
  cmd <- paste("exiftool -ext", type, "-r -t -s", ff, paste0('"', path, '"'))
  wd <- getwd()
  setwd(toolpath)
  txtout <- system(cmd, intern=TRUE)
  setwd(wd)
  
  i <- grepl("\t", txtout)
  if(sum(i)==0) stop("No matching fields found in metadata")
  dflong <- read.table(text=txtout[i], stringsAsFactors = FALSE, sep="\t")
  if(nfiles==1) dflong$rowid <- 1 else
    dflong$rowid <- rep(1:(sum(!i)-2), head(diff(which(!i))-1, -1))
  dfout <- as.data.frame(tidyr::pivot_wider(dflong, rowid, names_from=V1, values_from=V2))[,-1, drop=FALSE]
  
  if(!"" %in% fields){
    notfound <- fields[!fields %in% names(dfout)]
    if(length(notfound)>0) 
      warning(paste("Some fields not found in metadata:", paste(notfound, collapse=", ")))
  }
  if(!is.null(names(fields))){
    nms <- names(fields)[match(names(dfout), fields)]
    names(dfout) <- ifelse(nms=="", names(dfout), nms)
  }
  
  if(!is.null(tagfield)){
    if(tagfield %in% names(dfout)){
      utags <- split.tags(dfout[,tagfield], ...)
      if(class(utags)=="integer"){
        return(dfout[utags, ])
      }
      if(nrow(utags)!=nrow(dfout))
        dfout <- utags else
          dfout <- cbind(dplyr::select(dfout, -any_of(tagfield)), utags)
    }
  }
  
  type.convert(dfout, as.is=TRUE)
}


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
#For installation of exiftool, see install.exif function help.
#For installation of ffmpeg:
# 1. download the latest master build here: https://ffmpeg.org/download.html
# 2. extract the zip

#INPUT
# fps: frame rate for extraction (frames per second)
# inpath: a character string giving the path of the folder containing video files to process
# outpath: a character string giving the path of the folder in which to place extracted images
# filetypes: character vector giving video file types to be processed (case insensitive)
# fpath: a character string giving the path of the folder containing the ffmpeg executable
# epath: a character string giving the path of the folder containing the exiftool executable
# copy.jpegs: whether to additionally copy images (JPEG files) from inpath to outpath
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

extract.frames <- function(fps, inpath, outpath=file.path(dirname(inpath), "frames"),
                           filetypes=c("MP4", "AVI"),
                           fpath="C:/ffmpeg/bin", epath=NULL,
                           copy.jpegs=FALSE, stamp.time=FALSE, time.offset=0, 
                           suffix.length=3, recursive=TRUE, ...){
  
  if(dirname(outpath)==".") outpath <- file.path(getwd(), outpath)
  if(dir.exists(outpath)){
    res <- readline("Outpath already exists and will be over-written. Do you want to proceed [y/n]? ")
    if(tolower(res)!="y") return()
    unlink(outpath, recursive = T)
  }
  message("Reading metadata...")
  vexf <- read.exif(inpath, type="avi", toolpath=epath, recursive=recursive, fields="")
  dirs <- list.dirs(inpath)
  outdirs <- paste0(outpath, sub(inpath, "", dirs))
  for(path in outdirs) dir.create(path)
  outpaths <- paste0(outpath, sub(inpath, "", vexf$Directory))
  isvid <- grepl(paste(filetypes, collapse="|"), vexf$FileType)
  if(any(isvid)){
    for(i in 1:sum(isvid)){
      file <- file.path(vexf$Directory, vexf$FileName)[isvid][i]
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
