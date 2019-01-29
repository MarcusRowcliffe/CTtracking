require(magick)

#######################################################################################################
#extract.frames
#######################################################################################################
#Extracts frames at a given frame rate from video files, and optionally adds a timestamp to the
#metadata, reconstructed from original video time stamp and frame position.

#Optionally calls copy.images (see below), for cases where you also want to transfer images
#to the video frame directory (ignored if no image files exist in a folder).
#Optionally operates recursively on all subdirectories within inpath as well as the root. 
#If ouptath is a folder name without path, this folder is created within the working directory.
#If outpach folder doesn't exist it is created, otherwise the existing folder is over-written, with
#a warning option to abort before this happens.
#NB Some file formats adjust their date metadata according to the time zone of the current computer,
#which can lead to incorrect timestamps on the resulting images. If this happens, set computer time
#zone to that in which the videos were taken while processing.
#
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
# filetypes: character vector giving file extensions of video files to be processed (case insensitive)
# fpath: a character string giving the path of the folder containing the ffmpeg executable
# epath: a character string giving the path of the folder containing the exiftool executable
# copy.jpegs: whether to copy (and crop) images from inpath to subdir
# stamp.time: whether to add timestamps to the video frame files; if TRUE, creates metadata field CreateDate
# time.offset: seconds to be added to timestamps in metadata of created images, if timestamp is TRUE
# suffix.length: number of digits to add to frame file names as suffix 
# recursive: whether to process subdirectories recursively, or only the root inpath
# ...: additional arguments to pass to copy.images (used if copy==TRUE)
#
#OUTPUT
# None: creates a set of time-stamped image files in each subdir, extracted from video files in inpath.
extract.frames <- function(fps, inpath=NULL, outpath="frames", filetypes=c(".mp4", ".avi"),
                              fpath="C:/ffmpeg/bin", epath="C:/exiftool",
                              copy.jpegs=FALSE, stamp.time=FALSE, 
                              time.offset=0, suffix.length=3, recursive=TRUE, ...){

  extract.dir <- function(inpath){
    outpath <- paste0(outpath, sub(paths[1], "", inpath))
    dir.create(outpath)
    fullfiles <- list.files.only(inpath)
    isvid <- grepl(paste(filetypes, collapse="|"), tolower(fullfiles))
    if(any(isvid)){
      fullfiles <- fullfiles[isvid]
      files <- tools::file_path_sans_ext(fullfiles)
      exf <- read.exif(inpath, toolpath=epath, subdirs=FALSE)
      for(i in 1:length(files)){
        extract(fps, file.path(inpath, fullfiles[i]), outpath, fpath, suffix.length)
        if(stamp.time==TRUE){
          stamptime(fps, outpath, files[i], get.min.metadate(exf[isvid,][i,]), epath)
        }
      }
      if(copy.jpegs==TRUE) 
        copy.images(inpath, outpath, epath, exf, ...)
    }
  }

  if(is.null(inpath)) inpath <- getwd()
  if(dirname(outpath)==".") outpath <- file.path(getwd(), outpath)
  if(dir.exists(outpath)){
    res <- readline(paste("Outpath already exists and will be over-written. Do you want to proceed [y/n]? \n",
                          outpath))
    if(tolower(res)!="y") return()
    unlink(outpath, recursive = T)
  }
  if(recursive==TRUE) paths <- list.dirs(inpath, recursive=TRUE) else paths <- inpath
  sapply(paths, extract.dir)
}

#######################################################################################################
#get.min.metadate
#######################################################################################################
#Returns the minimum date in a row of data containing character format date-times with ":" separators
#
#INPUT
# datarow: a row from a dataframe with at least one column containing character date-times and with 
#          those column names containing "Date"
#OUTPUT
# A POSIX class date-time
get.min.metadate <- function(datarow){
  j <- grep("Date", names(datarow))
  dates <- strptime(sub("\\s*\\+.*", "", datarow[j]), "%Y:%m:%d %H:%M:%S", tz="UTC")
  dates[which(dates==min(dates, na.rm=TRUE))][1]
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
#
#OUTPUT
extract <- function(fps, file, outpath, toolpath="C:/FFmpeg", suffix.length=3){
  wd <- getwd()
  setwd(toolpath)
  basefile <- tools::file_path_sans_ext(basename(file))
  cmd <- paste0("ffmpeg -i ", file, " -vf fps=fps=", fps, ":start_time=-5 -vsync vfr ",
               file.path(outpath, paste0(basefile, "-%0", suffix.length, "d.jpg")))
  shell(cmd)
  setwd(wd)
}

#######################################################################################################
#stamptime
#######################################################################################################
#Adds a time stamp to images all images in a folder containing a given string in their name;
#Bases stamp values on a starting time and the frame rate used for extraction.
#
#INPUT
# fps: the frame rate used for extraction (frames per second)
# path: a character string giving the path containing the images to process
# file: a character string used to select a subset of files to process
# basedate: a POSIX value to be used as the starting time for sequence
# toolpath: a character string giving the path of the folder containing the exiftool executable
# 
#OUTPUT
# None (only modifies existing files)
stamptime <- function(fps, path, file, basedate, toolpath="C:/Exiftool"){
  wd <- getwd()
  setwd(toolpath)
  file <- sub("\\(", "\\\\(", file)
  file <- sub("\\)", "\\\\)", file)
  files <- list.files.only(path, file, full.names=TRUE)
  newdates <- as.character(basedate+((1:length(files))-1)/fps)
  for(j in 1:length(files)){
    cmd <- paste0("Exiftool ", files[j], ' -createdate="', newdates[j], '"', " -overwrite_original")
    shell(cmd)
  }
  setwd(wd)
}

#######################################################################################################
#copy.images
#######################################################################################################
#Copies image files from inpath to outpath; where inpath contains a mix of images and 
#videos with different resolution, image files are first cropped the same resolution as 
#the videos. The cropping process assumes that video format is larger than image.
#
#INPUT
# inpath: a character string giving the path of the folder containing files to process
# outpath: a character string giving the path of the folder to which files are moved
# toolpath: a character string giving the path of the folder containing exiftool.exe
# exf: dataframe of exif data from files in inpath
# suffix: text to be added to the copied file names before the extension
# crop: whether to crop images before copying 
# imgtype: character string giving the file extension of the images to be copies
#
#OUTPUT
# Copies (optionally cropped) images files in inpath to outpath
copy.images <- function(inpath, outpath, toolpath="C:/Exiftool", exf=NULL, suffix="", crop=TRUE, imgtype=".jpg"){
  if(is.null(exf)) exf <- read.exif(inpath, toolpath, subdirs=FALSE) else
    if(is.null(toolpath)) stop("toolpath and exf arguments to copy.images cannot both be NULL")
  stills <- list.files.only(inpath, imgtype, ignore.case=TRUE)
  if(length(stills)>0){
    imgs <- image_read(paste0(inpath, "/", stills))
    if(crop){
      imgW <- unique(subset(exf, FileType=="JPEG")$ImageWidth)
      imgH <- unique(subset(exf, FileType=="JPEG")$ImageHeight)
      vidW <- unique(subset(exf, FileType=="MP4")$ImageWidth)
      vidH <- unique(subset(exf, FileType=="MP4")$ImageHeight)
      Hmargin <- (imgH-vidH)/2
      Wmargin <- (imgW-vidW)/2
      imgs <- image_crop(imgs, paste0(vidW,"x",vidH,"+",Wmargin,"+",Hmargin))
    }
    suffix <- paste0(suffix,".")
    for(i in 1:length(stills))
      image_write(imgs[i], paste0(outpath, "/", gsub("\\.", suffix, stills[i])))
  }
}

#######################################################################################################
#read.exif
#######################################################################################################
#Runs command line ExifTool to extract metadata of all image/video/audio files within a folder
#For a list of supported formats see https://www.sno.phy.queensu.ca/~phil/exiftool
#Requires standalone executable exiftool.exe to be present on your computer, available at above link
#Unzip and rename the exiftool(-k).exe file to exiftool.exe
#
#INPUT
# inpath: a character string giving the path of the folder containing files to process
# outpath: a character string giving the path of the folder in which to place results file (defaults to inpath)
# toolpath: a character string giving the path of the folder containing exiftool.exe
# return: should the function return the results as a dataframe
# write: should the function return the results as a new .csv file within outpath
# subdirs: should subdirectories of inpath also be searched for images
#Directory names must be free of spaces
#
#OUTPUT
#Optionally (depending on return input) a dataframe of metadata. 
# A csv file of the data called metadata.csv is also temporarily created 
# (or overwritten without warning) within outpath, and optionally preserved
# (depending on write input)
read.exif <- function(inpath, outpath=NULL, toolpath="C:/Exiftool", return=TRUE, write=FALSE, subdirs=TRUE){
  wd <- getwd()
  setwd(toolpath)
  if(is.null(outpath)) outpath <- inpath
  outfile <- paste0(outpath, "/metadata.csv")
  if(subdirs==TRUE) sbd<-"-r" else sbd <- ""
  cmd <- paste("exiftool", sbd, "-csv", inpath, ">", outfile)
  shell(cmd)
  setwd(wd)
  res <- read.csv(outfile, stringsAsFactors = FALSE)
  if(write==FALSE) file.remove(outfile)
  if(return==TRUE) return(res)
}

#######################################################################################################
#remove.file.spaces
#######################################################################################################
#Removes spaces from names of files within path, and optionally its subdirectories
#NB also removes spaces from the path names - don't use this if you need to preserve those spaces
#
#INPUT
# path: path within which to change file names, defaults to working directory
# subdirs: whither to look in subdirectories of path
remove.file.spaces <- function(path=".", subdirs=TRUE){
  files <- list.files(path, recursive=subdirs, full.names=TRUE)
  newnames <- gsub(" ", "", files)
  file.rename(files, newnames)
}

#######################################################################################################
#list.files.only
#######################################################################################################
#Wrapper for list.files that over-rides include.dirs argument to return only file names
list.files.only = function(dir, ...){
  args <- c(path=dir, list(...))
  if("full.names" %in% names(args)) full <- TRUE else full <- FALSE
  if(!full) args <- c(args, full.names=TRUE)
  fls <- do.call(list.files, args)
  res <- fls[!file.info(fls)$isdir]
  if(!full) res <- basename(res)
  res
}
