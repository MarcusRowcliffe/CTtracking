require(magick)
require(tidyr)

require(jpeg)
require(tidyverse)

camcal <- setClass("camcal", representation("list"))
depcal <- setClass("depcal", representation("list"))
calibs <- setClass("calibs", representation("list"))

#GENERAL FUNCTIONS#############################################

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

#peep.exif#

#Extract metadata data from a single image file for inspection prior to 
#extracting a whole folder. 

#INPUT
# path: a single character string giving a path to folder or file
# file.index: which file number to extract from (only used if path is a folder)

#OUTPUT
#A two-column dataframe of metadata (Tag and Value). 

#DETAILS
#Runs command line executable exiftool, which must be present locally (see 
#install.exiftool).

peep.exif <- function(path, file.index=1, toolpath=NULL){
  if(!file.exists(path)) stop("path not found")
  if(dir.exists(path)){
    allfls <- list.files(path, full.names=TRUE, recursive=TRUE)
    allfls <- allfls[grep(".jpg", allfls, ignore.case=TRUE)]
    n <- length(allfls)
    if(n==0) stop(paste("No files jpg found in", path))
    if(length(file.index)>1 | class(file.index)!="numeric") stop("file.index must be a single integer")
    if(file.index<1 | file.index>n) stop(paste0("file.index must be between 1 and ", length(allfls), " (the number of files in ", path, ")"))
    path <- allfls[file.index]
  }
  res <- read.exif(path, "", toolpath=toolpath)
  vals <- as.character(res[1,])
  data.frame(Field=names(res), Value=vals, stringsAsFactors=FALSE)
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
#A dataframe of image metadata. 

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

read.exif <- function(path, 
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

  cmd <- paste("exiftool -ext jpg -r -t -s", ff, paste0('"', path, '"'))
  wd <- getwd()
  setwd(toolpath)
  txtout <- shell(cmd, intern=TRUE)
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
      if(nrow(utags)!=nrow(dfout))
        dfout <- utags else
          dfout <- cbind(dplyr::select(dfout, -any_of(tagfield)), utags)
    }
  }
  
  type.convert(dfout, as.is=TRUE)
}

#image.copy#

#Copies images to a new location, optionally preserving the directory structure 
#from the #original location.

#INPUT
# to: a character string naming a folder to which to copy
# from: a character string naming a folder from which to copy
# exifdat: a dataframe of image metadata
# criterion: a logical expression in character form defining the images to select from exifdat
# structure: logical, whether to preserve folder structure (all files copied to root to dir if false)
# recursive: logical, whether to extract from subfolders of from (ignored if exifdat used)

#DETAILS
#Either from or exifdat must be provided, but not both. The criterion argument
#refers to data in exifdat, so is ignored if exifdat is not used. If used,
#the exifdat input must contain at least columns Directory and FileName.
#If the folder named by the to argument does not exist it will be created. The 
#default criterion (TRUE) copies all the images in exifdat. If a more complex 
#logical expression is provided, it should refer to one or more columns in exifdat. 
#Because criterion takes character form, any quote marks within the string need to 
#be escaped using \. For example, if you want to select only images where "fox" 
#is tagged in the species column, pass "species==\"fox\"" to the criterion
#argument. 

image.copy <- function(to, from=NULL, exifdat=NULL, criterion=TRUE, structure=TRUE, recursive=TRUE){
  if(is.null(exifdat) + is.null(from) != 1)
    stop("Either directory path (from=) or exif dataframe (exifdat=) must be provided, but not both")
  if(!is.null(from)){
    files <- list.files(from, pattern=".jpg", full.names=TRUE, 
                        recursive=recursive, ignore.case=TRUE)
  } else
  if(!is.null(exifdat)){
    if(!all(c("Directory", "FileName") %in% names(exifdat)))
      stop("exifdat must contain columns Directory and FileName")
    subdat <- try(subset(exifdat, eval(parse(text=criterion))), silent=TRUE)
    if(class(subdat)=="try-error"){
      tt <- unlist(strsplit(subdat[[1]], ":"))
      msg <- paste(c("There is a problem with the criterion", tail(tt,-1)), collapse=":")
      stop(msg)
    }
    files <- file.path(subdat$Directory, subdat$FileName)
  }

  if(structure){
    ff <- strsplit(files, "/", fixed=TRUE)
    mat <- suppressWarnings(do.call(rbind, ff))
    i <- sum(apply(mat, 2, function(x) length(unique(x)))==1)
    basefiles <- unlist(unique(lapply(ff, function(x)
      paste(x[(i+1):length(x)], collapse="/"))))
  } else{
    basefiles <- basename(files)
    if(length(unique(basefiles)) != length(basefiles))
      stop("Not all file names are unique, but you have chosen not to preserve the folder structure")
  }
    
  newfiles <- file.path(to, basefiles)
  nfound <- length(files)
  nexist <- sum(file.exists(newfiles))
  message(paste(nfound, "images found to copy...\n"),
          paste(nfound-nexist, "of which do not yet exist in destination folder."))

  if(nfound==nexist | nfound==0){
    message("Nothing to copy")
  } else{
    inpt <- ""
    inpt <- tolower(readline(prompt="Start copying (y/n)? "))
    while(!inpt %in% c("y","n"))
      inpt <- tolower(readline(prompt="Type y for yes or n for no: "))
    
    if(inpt=="y"){
      newdirs <- file.path(to, c("", unique(dirname(basefiles))))
      for(dir in newdirs) if(!dir.exists(dir)) dir.create(dir, recursive=TRUE)
      ncopied <- sum(file.copy(files, newfiles))
      message(paste(ncopied, "file(s) copied to:\n", normalizePath(to), "\n"))
      if(!is.null(exifdat)){
        subdat$Directory <- gsub("\\", "/", normalizePath(dirname(newfiles)), fixed=TRUE)
        return(subdat)
      }
    }
  }
}


#DATA PREP FUNCTIONS#############################################

#split.tags#

#Splits out multi-field tags which appear as a single field in image metadata,
#following image tagging in, for example, XnView, Photoshop, Digikam.

#INPUT
# dat: a vector of character strings (see details for expected form)
# tagsep: the character or string used to separate fields within strings
# valsep: the character or string used to separate field names from values within fields

#OUTPUT
#A data frame with a row per record in dat, and a column for each field name 
#found in dat. Where a field name is not given for a record, a missing value
#is assigned.

#DETAILS
#The default values for tagsep (", ") and valsep ("|") work for default 
#hierarchical tag output from XnView (www.xnview.com). In this case, records
#in dat must be strings taking the form: "Field1|value1, Field2|value2".

split.tags <- function(dat, tagsep=", ", valsep="|"){
  tagmatches <- unlist(lapply(gregexpr(tagsep, dat), function(x) sum(x>0)))
  valmatches <- unlist(lapply(gregexpr(paste0("\\", valsep), dat), function(x) sum(x>0)))
  i <- which((valmatches-tagmatches)!=1)
  if(length(i)>0){ 
    message("Error: There's a problem with the use of separators in dat - check output data")
    return(data.frame(Record=i, ProblemData=dat[i]))
  }
  
  lst <- strsplit(dat, tagsep)
  lst[unlist(lapply(lst, function(x) any(is.na(x))))] <- paste0("NA",valsep,"NA")
  longdf <- lst %>% 
    lapply(strsplit, paste0("\\", valsep)) %>% 
    unlist() %>% 
    matrix(ncol=2, byrow=TRUE) %>% 
    as.data.frame()
  longdf$rowid <- rep(1:length(dat), unlist(lapply(lst, length)))
  widedf <- longdf %>% 
    tidyr::pivot_wider(rowid, names_from=V1, values_from=V2) %>%
    as.data.frame() %>% 
    utils::type.convert(as.is=TRUE)
  dplyr::select(widedf, -any_of(c("NA", "rowid")))
}


#read.digidat#

#Reads and merges multiple csv files of digitisation data from animaltracker, 
#and optionally adds relevant image metadata.

#INPUT
# path: character path to folder containing all required files
# exifdat: a dataframe containing the relevant image metadata (see output)

#OUTPUT
# A dataframe of the original digitisation data plus:
#  - sequence_id: reassigned to give unique values to each sequence across the 
#    whole dataframe
#  - sequence_id_original: the original directory-specific sequence IDs
#  - dir: full path to the containing directory
#  - folder: name of the containing folder
#  - image metadata from exifdat (if provided), matched using directory/file 
#    combinations; columns Directory and FileName must be present in exifdat for this
#    purpose.

read.digidat <- function(path, exifdat=NULL){
  renumber <- function(x) c(0, cumsum(head(x, -1)!=tail(x, -1)))

  csvfiles <- list.files(path, pattern=".csv", full.names=TRUE, ignore.case=TRUE, recursive=TRUE)
  if(length(csvfiles)==0) stop("No csv files found in path")
  df.list <- lapply(csvfiles, read.csv, stringsAsFactors=FALSE)
  df.list <- lapply(df.list, dplyr::mutate_all, as.character)
  
  colnames <- lapply(df.list, names)
  n_columns <- unlist(lapply(colnames, length))
  if(length(unique(n_columns)) > 1){
    message("Error: Not all csv files have the same number of columns - check output data")
    return(data.frame(file=csvfiles, n_columns))
  }
  colnames <- matrix(unlist(colnames), ncol=length(colnames))
  if(any(apply(colnames, 1, function(x) length(unique(x))) != 1)){
    message("Error: Not all csv files have the same column headings - check output data")
    return(data.frame(file=csvfiles, t(colnames)))
  }
  
  pths <- rep(normalizePath(dirname(csvfiles), winslash="/"),
              unlist(lapply(df.list, nrow)))
  df <- cbind(dir = pths,
              folder = basename(pths),
              type.convert(dplyr::bind_rows(df.list), as.is=TRUE)
  )
  
  if("height" %in% names(df))
    df$height <- as.numeric(df$height)

  sicol <- which(names(df)=="sequence_id")
  df <- cbind(df[,1:sicol], sequence_id_original=df$sequence_id, df[,(sicol+1):ncol(df)])
  df$sequence_id <- renumber(paste0(df$dir, df$sequence_id))
  
  if(!is.null(exifdat)){
    if(!all(c("Directory", "FileName") %in% names(exifdat)))
      stop("exifdat must contain at least columns Directory and FileName for matching")
    dfsource <- file.path(df$dir, df$image_name)
    exifsource <- file.path(exifdat$Directory, exifdat$FileName)
    miss <- !dfsource %in% exifsource
    if(sum(miss)>0){
      cat(dfsource[miss], sep="\n")
      warning(paste(sum(miss), "out of", nrow(df), "digitised images not found in exifdat (named above) and were discarded"))
      df <- subset(df, !miss)
    }
    i <- match(file.path(df$dir, df$image_name), exifsource)
    j <- !names(exifdat) %in% c("Directory", "FileName")
    df <- cbind(df, exifdat[i,j])
  }
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


#pairup#

#Matches up points digitised in pairs

#INPUT
# dat: dataframe of digitisation data
# pairtag: a vector of one or more strings giving the name(s) of the column(s)
#          in dat the combination of which uniquely defines digitisation pairs

#OUTPUT
# A dataframe with the two digitisation points per object arranged in single rows.

#DETAILS
#Generates a dataframe of paired digitisation points for input to calc.distance, 
#either directly, or via cal.dep.

#Input dat must have at least columns x and y (pixel positions) and the columns
#defined by pairtag. There are three additional data columns that must have
#specific names if provided:
# height: height above ground of the digitised points
# distance: distance of the digitised points from camera
# length: length or size of the digitised object between the points of a pair

#The output dataframe contains the input data unchanged, except that 
#sequence_annotation is renumbered to give unique values across the whole 
#dataframe, columns x, y and (if present) height are removed, and the following
#columns are added:
#  sequence_annotation_original: the original sequence identifier unchanged
#  pair_id: the pair identifier, derived from column(s) given in the pairtag argument
#  xb, yb, xt, yt: pixel x and y co-ordinates of pole b(ottoms) and t(ops)
#  pixlen: length between digitised points in pixels
# Additionally, if a column named height is present, these columns are added:
#  xg, yg: extrapolated pixel x and y co-ordinates of pole ground-contact points)
#  hb, ht: real heights above ground of pole b(ottoms) and t(ops)
#  length: real lengths between points (the difference between ht and hb)

#Point groups are discarded with a warning if:
# base height is greater than or equal to top height (hb>=ht)
# the points have different distances at top and base
# the group was digitised only once or more than twice

pairup <- function(dat, pairtag){

  pair <- function(dat){
    dat <- dat[order(dat$pair_id, dat$y), ]
    j <- 2*(1:(nrow(dat)/2))
    xy <- cbind(dat[j, c("x","y")], dat[j-1, c("x","y")])
    names(xy) <- c("xb","yb","xt","yt")
    if("height" %in% names(dat)){
      xy <- cbind(hb=dat$height[j], ht=dat$height[j-1], length=dat$height[j-1]-dat$height[j], xy)
      relh <- with(xy, hb/length)
      xy <- cbind(xy, xg=with(xy, xb-(xt-xb)*relh), yg=with(xy, yb-(yt-yb)*relh))
    }
    dat <- cbind(dat[j,], xy)
    dat[, !names(dat) %in%  c("x","y","height")]
  }

  colnames <- names(dat)
  if(!all(c("x","y",pairtag) %in% colnames)) 
    stop(paste0("Input dat must have at least columns x, y, and those defined in pairtag argument (", 
                paste(pairtag, collapse=", "), ")"))
  
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
  dat$pair_id <- tidyr::unite(dplyr::select(dat, all_of(pairtag)), "pr", sep="/")$pr
  dat <- dat[order(dat$pair_id), ]
  
  duff2 <- duff3 <- duff4 <- FALSE
  tab <- table(dat$pair_id)
  i <- which(sequence(tab)==1)
  iduff1 <- !1:nrow(dat) %in% c(i, tail(i,-1)-1, nrow(dat)) #surplus points (>2)
  duff1 <- tapply(iduff1, dat$pair_id, any)
  if("height" %in% names(dat)) 
    duff2 <- tab==1 #Only one point digitised
  if("distance" %in% names(dat)) #Paired points at different distances
    duff3 <- with(dat, tapply(distance, dat$pair_id, min) != tapply(distance, dat$pair_id, max))

  dat <- pair(dat[!(dat$pair_id %in% names(which(duff2 | duff3)) | iduff1), ])
  
  if("hb" %in% names(dat)){ 
    duff4 <- with(dat, hb>=ht) #base height >= top height
  }

  if(any(duff1 | duff2 | duff3) | any(duff4)){
    message("Some rows were discarded because...")
    if(any(duff1)){
      message("...the pole had more than two points digitised:")
      cat(names(which(duff1)), sep="\n")
    }
    if(any(duff2)){
      message("...the pole was digitised only once:")
      cat(names(which(duff2)), sep="\n")
    }
    if(any(duff3)){
      message("...the pole had different distances at top and base:")
      cat(names(which(duff3)), sep="\n")
    }
    if(any(duff4)){
      message("...the pole base height was greater than or equal to top height:")
      cat(dat$pair_id[duff4], sep="\n")
    }
    message("Warning:\n Some rows were discarded, see above for details")
  }
  
  subset(dat, !duff4)
}

#CALIBRATION FUNCTIONS#############################################

#cal.cam#

#Creates a camera calibration model

#INPUT
# poledat: a dataframe of pole digitisation data (see details)
# camtag: character string naming a column within poledat tagging camera ID

#OUTPUT
# A list of class camcal (ie camera calibration), describing the relationship 
# between pixel size and radial distance, and x-pixel position and angular 
# distance, with elements:
#  model: quadratic model of FSratio (focal_length:sensor size) against relative x pixel position 
#  APratio: ratio of angle to relative x pixel position

#DETAILS
#Poledat input is most easily generated by read.digidat followed by pairup. It 
#must contain (at least) columns:
# distance: pole distances from camera
# length: length of pole between digitised points
#  xt,yt,xb,yb: x,y pixel positions of digitised pole t(ops) and b(ases) in image
#  ImageWidth, ImageHeight: x and y pixel dimensions of each image
#
#If camtag is provided, one model is fitted for each unique camera ID, but 
#camtag can be NULL if all data are for a single camera.

cal.cam <- function(poledat, camtag=NULL){
  #Internal function fits a single camera calibration model
  cal <- function(dat){
    dim <- as.list(apply(dat[,c("ImageWidth","ImageHeight")], 2, unique))
    if(length(unlist(dim))>2) 
      stop("There is more than one unique value per camera for ImageWidth and/or ImageHeight in poledat")
    names(dim) <- c("x","y")
    dat$pixlen <- with(dat, sqrt((xb-xt)^2 + (yb-yt)^2))
    dat$relx <- apply(dat[c("xb","xt")], 1, mean)/dim$x-0.5
    FSratio <- with(dat, distance*pixlen/(length*dim$y))
    APratio <- mean(with(dat, (acos(1-length^2/(2*(distance^2+(length/2)^2)))*dim$x/pixlen)))
    mod <- lm(FSratio~I(relx^2), data=dat)
    camcal(list(model=mod, APratio=APratio, dim=dim, data=dat, 
                id=if(is.null(camtag)) NULL else unique(dat[,camtag])))
  }
  
  if(class(poledat) != "data.frame") 
    stop("poledat must be a dataframes")
  if(!is.null(camtag)){
    if(!camtag %in% names(poledat))
      stop(paste0("Can't find camtag column (", camtag, ") in poledat"))
  }
  required <- c("xb", "yb", "xt", "yt", "ImageWidth", "ImageHeight", "distance", "length")
  if(!all(required %in% names(poledat))) 
    stop(paste("poledat must contain all of these columns:", paste(required, collapse=" ")))
  
  if(is.null(camtag))
    out <- list(cal(poledat)) else{
      cams <- unique(poledat[, camtag])
      out <- lapply(cams, function(cam) cal(poledat[poledat[,camtag]==cam, ]))
      names(out) <- cams
    } 
  calibs(out)
}


#plot.camcal#

#INPUT
# mod: a camera calibration object of class camcal, created using cal.cam

#Generates two diagnostic plots for a camera calibration model:
#1. Length per image pixel as a function of distance from camera and x pixel
#   position within the image
#2. A diagram of digitised pole sections as they appear in images.

plot.camcal <- function(mod){
  dat <- mod$data
  cols <- grey.colors(11, start=0, end=0.8)
  
  #PLOT POLE:PIXEL RATIO V DISTANCE RELATIONSHIP
  x <- abs(dat$relx)
  i <- round(1 + (x-min(x))*10/diff(range(x)))
  with(dat, plot(distance, length/pixlen, col=cols[i], pch=16, main=mod$id,
                 ylab="length/pixel", xlab="distance", 
                 sub="Shading from image centre (dark) to edge", cex.sub=0.7))
  FS <- predict(mod$mod, newdata=data.frame(relx=c(0,0.5)))
  dr <- range(dat$distance)
  lines(dr, dr/(FS[1]*mod$dim$y), col=cols[1])
  lines(dr, dr/(FS[2]*mod$dim$y), col=cols[11])
  
  #PLOT POLE IMAGE
  d <- dat$distance
  i <- round(1 + (d-min(d))*10/diff(range(d)))
  plot(c(0,mod$dim$x), c(0,-mod$dim$y), type="n", asp=1, main=mod$id,
       xlab="x pixel", ylab="y pixel", 
       sub="Shading from near camera (dark) to far", cex.sub=0.7)
  for(p in 1:nrow(dat))
    lines(dat[p,c("xb","xt")], -dat[p,c("yb","yt")], type="l", lwd=2, col=cols[i[p]])
  lines(c(0,rep(c(mod$dim$x,0),each=2)), c(rep(c(0,-mod$dim$y),each=2),0), lty=2)
}


#calc.distance

#Calculate distances from camera to an object of known length.

#INPUT
#dat: A dataframe of paired point digitisation data created by pairup
#cmods: A list of camera calibration models created by cal.cam, named if more than one 
#idtag: a string naming the column in dat used for matching in lookup
#lookup: a table used to match camera models to categories (typically deployments) in dat

#OUTPUT
#A dataframe identical to dat plus additional columns:
# distance: distance from camera to object in the same units as length in dat
# relx: relative x-pixel position in image (from -0.5 to 0.5 left to right edges)
# pixlen: the pixel length between points in image

#DETAILS
#The input dat must have (at least) columns named:
# xb,yb,xt,yt: paired x and y pixel positions of object extremities, defined by (xb,yb) and (xt,yt)
# length: real distances between the pixels in each pair

#If cmods holds a single calibration model, this is applied to all data in dat.
#If cmods holds multiple calibration models, both idtag and lookup must be 
#supplied, and both dat and lookup must contain this column. This enables the
#appropriate camera calibration model to be selected for each observation in dat.

calc.distance <- function(dat, cmods, idtag=NULL, lookup=NULL){
  
  calc <- function(dat, cmod){
    if(!"relx" %in% names(dat)) dat$relx <- (dat$xb+dat$xt)/(2 * cmod$dim$x) - 0.5
    FSratio <- predict(cmod$model, newdata = data.frame(relx=dat$relx))
    dat$pixlen <- with(dat, sqrt((xt-xb)^2 + (yt-yb)^2))
    dat$distance <- with(dat, FSratio*length*cmod$dim$y/pixlen)
    dat
  }

  if(!all(c("xb","yb","xt","yt","length") %in% names(dat))) 
    stop("dat must contain (at least) columns xb, yb, xt, yt and length")

  dat$rowid <- 1:nrow(dat)
  if(length(cmods)==1) 
    return(calc(dat, cmods[[1]])) else{
      if(is.null(idtag))
        stop("idtag cannot be missing if there is more than one model in cmods") else
          if(!idtag %in% names(dat))
            stop("Can't find idtag column in dat")
      if(is.null(lookup)){
        if(!all(dat[,idtag] %in% names(cmods)))
          stop("If lookup table is missing, idtag column must contain camera IDs matched with names(cmods)")
        camid <- dat[,idtag] 
      } else{
        if(!idtag %in% names(lookup))
          stop("idtag column must be present in lookup as well as dat")
        if(!all(dat[,idtag] %in% lookup[,idtag]))
          stop(paste0("Can't find all dat$", idtag, " values in lookup$", idtag))
        if(!all(lookup$camera %in% names(cmods)))
          stop("Can't find all lookup$camera values in names(cmods)")
        dat$cam_model <- lookup$camera[match(dat[,idtag], lookup[,idtag])]
      }

    cams <- unique(dat$cam_model)
    res <- lapply(cams, function(cam) calc(subset(dat, cam_model==cam), cmods[[cam]]))
    res <- dplyr::bind_rows(res)
    res[order(res$rowid), -which(names(res)=="rowid")]
  }
}

#cal.dep#

#Create a deployment calibration model

#INPUT
# dat: data frame of paired pole digitisation data (see details)
# cmods: A list of camera calibration models created by cal.cam, named if more than one 
# idtag: a string naming the column in dat used for matching in lookup
# lookup: a table used to match camera models to deployments in dat
# minpoles: threshold minimum number of poles needed to fit a model
# flex: whether to include additional flexibility in the model (can be difficult to fit)

#OUTPUT
#A list object of class depcal (ie deployment calibration), describing 
#relationship between pixel position and distance, with elements:
# cam.model: the camera calibration model used to predict distances (if any)
# model: non-linear least squares (nls) fit of distance as a function of pixel position (see details)
# data: the data input to the model
# dim: the x,y pixel dimensions of the images used for calibration
# id: the name of the deployment

#DETAILS
#Input dataframe dat is ideally created using pairup, and must contain at least
#columns:
#  xb, yb, xt, yt: x and y co-ordinates of pole b(ottom) and t(op) positions digitised
#  hb, ht: actual heights above ground of the digitised pole positions
#  ImageWidth, ImageHeight: x and y dimensions of each image
#If cmods (camera calibration models) are provided pole distances will be 
#predicted using these models. If not, dat must also contain distance data in a 
#column named distance.

#Distance (d) as a function of x and y pixel positions is modelled using non-linear
#least squares (nls function) with equation either (for flex=FALSE):
#   d ~ b1 / (y-(b2+b3*x))
#or (for flex=TRUE):
#   d ~ b1 / (y^b4-(b2+b3*x))

#If the number of poles available for a deployment is less than or equal to 
#minpoles, no model fitting is attempted and the output is NULL, with a warning.

cal.dep <- function(dat, cmods=NULL, deptag=NULL, lookup=NULL, 
                     minpoles=3, flex=FALSE){

  cal <- function(dat, id=NULL, cmod=NULL){
    if(nrow(dat)<minpoles){
      res <- list(cam.model=cmod, model=NULL, data=NULL, dim=NULL, id=id)
    } else{
      dim <- as.list(apply(dat[,c("ImageWidth","ImageHeight")], 2, unique))
      if(length(unlist(dim))>2) 
        stop("There is more than one unique value per deployment for ImageWidth and/or ImageHeight in dat")
      names(dim) <- c("x","y")
      
      dat$rely <- dat$yg/dim$y
      dat$relx <- (dat$xb+dat$xt)/(2 * dim$x) - 0.5
      repeat{
        b1.start <- runif(1,0,max(dat$distance))
        if(flex)
          mod <- try(nls(distance~b1/(rely^b4-(b2+b3*relx)), data=dat, algorithm="port", 
                       start=list(b1=b1.start, b2=min(dat$rely)*0.9, b3=0, b4=1),
                       lower=c(b1=0,b2=0,b3=-Inf,b4=0), 
                       upper=c(b1=Inf,b2=min(dat$rely),b3=Inf,b4=Inf),
                       trace=F )) else
          mod <- try(nls(distance~b1/(rely-(b2+b3*relx)), data=dat, algorithm="port", 
                       start=list(b1=b1.start, b2=min(dat$rely)*0.9, b3=0),
                       lower=c(b1=0,b2=0,b3=-Inf), 
                       upper=c(b1=Inf,b2=min(dat$rely),b3=Inf),
                       trace=F ))
          if(class(mod)=="nls") break
      }
      res <- list(cam.model=cmod, model=mod, data=dat, dim=dim, id=id)
    }
    depcal(res)
  }

  if(!is.null(deptag))
    if(!deptag %in% names(dat))
      stop(paste0("Can't find deptag column (", deptag, ")in dat"))
  if(length(cmods)==0){
    if(!"distance" %in% names(dat))
      stop("A distance column must be present in dat if cmods is NULL")
    } else
      if(length(cmods)==1){
        dat <- calc.distance(dat, cmods)
        } else{
          if(is.null(deptag) | is.null(lookup))
            stop("Both deptag and lookup must be defined if there are multiple models in cmods") else{
              if(!deptag %in% names(lookup))
                stop("deptag column must be present in lookup as well as dat")
              dat <- calc.distance(dat, cmods, deptag, lookup)
            }
        }

  if(length(cmods)==1) cmod <- cmods[[1]]
  if(is.null(deptag)){
    if(is.null(cmods)) cmod <- cal.cam(dat)[[1]]
    res <- list(cal(dat, NULL, cmod))
  } else{
    deps <- unique(dat[,deptag])
    res <- lapply(deps, function(d){
      subdat <- dat[dat[,deptag]==d, ]
      if(is.null(cmods)) cmod <- cal.cam(subdat)[[1]] else
      if(length(cmods)>1){
        cam <- lookup$camera[lookup[,deptag]==d]
        cmod <- cmods[[cam]]
      }
      cal(subdat, d, cmod)
    })
    names(res) <- deps
  }
  
  nofits <- unlist(lapply(res, function(m) is.null(m$model)))
  if(any(nofits)){
    message("Warning: One or more deployments had too few poles to fit a model:")
    cat(deps[nofits], sep="\n")
  }
  calibs(res)
}


#plot.depcal#

#Show diagnostic plots for deployment calibration model

#INPUT
# mod: a deployment calibration object of class depcal, created using dep.cam

#Generates two diagnostic plots for a deployment calibration model:
#1. Distance from camera as a function of pixel position within image with model
#   trend lines
#2. A diagram of digitised poles and the point's at which they were digitised
#   as they appear in images.

plot.depcal <- function(mod){
  dep <- mod$id
    
  if(is.null(mod$model)){
    message(paste("Model without a fit not plotted:", dep, "\n"))
  } else{
    dim <- as.list(apply(mod$data[,c("ImageWidth","ImageHeight")],2,unique))
    dat <- mod$data
    colrange <- grey.colors(11, start=0, end=0.8)
    
    #PLOT DISTANCE V Y-PIXEL RELATIONSHIP
    cols <- with(dat, colrange[1+round(10*((relx-min(relx))/diff(range(relx))))])
    mxx <- max(max(dat$rely),1.5)
    with(dat, plot(rely, distance, col=cols, pch=16, xlim=c(0,mxx), ylim=c(0, 1.5*max(distance)),
                   xlab="Relative y pixel position", ylab="Distance from camera",
                   main=dep, 
                   sub="Shading from image left (dark) to right edge", cex.sub=0.7))
    if(class(mod$model)=="nls"){
      sq <- seq(0, mxx, len=100)
      lines(sq, predict.r(mod$model, -0.5, sq), col=colrange[1])
      lines(sq, predict.r(mod$model, 0, sq), col=colrange[6])
      lines(sq, predict.r(mod$model, 0.5, sq), col=colrange[11])
    }
    
    #PLOT POLE IMAGE
    relht <- with(dat, (1-ht) / (ht-hb))
    plot(c(0, dim$ImageWidth), -c(0, dim$ImageHeight), 
         asp=1, xlab="x pixel", ylab="y pixel", type="n", 
         main=dep, cex.sub=0.7)
    lines(c(0,rep(c(dim$ImageWidth,0),each=2)), c(rep(c(0,-dim$ImageHeight),each=2),0), lty=2)
    cols <- colrange[with(dat, 1+round(10*((distance-min(distance))/diff(range(distance)))))]
    for(i in 1:nrow(dat)){
      with(dat, lines(c(xg[i],xt[i]), -c(yg[i],yt[i]), col=cols))
      with(dat, points(c(xb[i],xt[i]), -c(yb[i],yt[i]), pch=18, cex=0.7, col=2))
    }
  }
}

#plot.calibration#

#Show multiple diagnostic plots for a list of either camera or deployment 
#calibration calibration models

#INPUT
# mods: a list of either camera or deployment calibration objects

plot.calibs <- function(mods){
  lapply(mods, plot)
}

#show.image#

#Shows digitised images with digitisation points.

#INPUT
# dat: a dataframe image names and digitisation points
# dir: character string naming the directory containing the images
# type: whether dat contains pole or animal data
# 
#dat must contain the following columns:
# image_name
# x and y (if type is animal, giving digitisation positions)
# xb, xt, yb and yt (if type is pole, giving bottom and top digitisation positions)
#
#For animal data, typically applied to the $animal compenent of read.digidat() output.
#For pole data, typically applied to the $data component of cal.site() output.

show.image <- function(dat, dir, type=c("pole", "animal")){
  type <- match.arg(type)
  for(i in 1:nrow(dat)){
    imgpath <- file.path(dir, dat$image_name[i])
    img <- jpeg::readJPEG(imgpath, native=T)
    imdim <- dim(img)
    title <- dat$image_name[i]
    if(type=="pole") title <- paste0(title, " (", paste(dat[i, c("hb", "ht")], collapse=" / "), " m)")
    plot(1,1, xlim=c(1, imdim[2]), ylim=c(1, imdim[1]), type="n", asp=1,
         xaxt="n", yaxt="n", xlab="", ylab="", bty="n", main=title)
    rasterImage(img, 1, 1, imdim[2], imdim[1])
    if(type=="pole")
      points(dat[i,c("xt","xb","xg")], imdim[1]-dat[i,c("yt","yb","yg")]+1, 
             pch=16, col=c(2,2,5)) else
               points(dat$x[i], imdim[1]-dat$y[i]+1, pch=16, col=2)
  }
}

#DATA SUMMARY FUNCTIONS#############################################


#predict.r#

#Predict radial distance from camera given pixel positions

#INPUT
# mod: a depcal object (site calibration model, produced using cal.site(...))
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
# dat: a dataframe of animal position digitisation data (see details)
# mods: a named list of deployment calibration models
# deptag: a string naming the column within dat against which names of the 
#         elements can be matched to apply the right deployment calibration
#         models.

#OUTPUT
#A dataframe of original data with additional columns:
# radius: radial distance from camera
# angle: angular distance from camera
# frame_count: an indicator of the frame order within each sequence

#DETAILS
#Input dat must contain (at least) columns:
# x,y: x and y pixel positions for each digitised point
# ImageWidth,ImageHeight: x and y pixel dimensions of each image; must be consistent for each deployment

predict.pos <- function(dat, mods, deptag="deployment"){

  required <- c("x","y","ImageWidth","ImageHeight", deptag)
  if(!all(required %in% names(dat))) 
    stop(paste("dat must contain all of these columns:", paste(required, collapse=" ")))

  deps <- unique(dat[, deptag])
  gotmodel <- deps %in% names(mods)
  nullmodel <- names(mods)[unlist(lapply(mods, function(m) is.null(m$model)))]
  gotmodel[match(nullmodel, deps)] <- FALSE
  if(!all(gotmodel)){
    message("Warning: Some deployments had no matching calibration model and were stripped out:")
    cat(deps[!gotmodel], sep="\n")
    dat <- subset(dat, dat[,deptag] %in% deps[gotmodel])
    deps <- deps[gotmodel]
  }

  multidim <- lapply(tapply(dat$ImageWidth, dat[,deptag], unique), length)>1 |
              lapply(tapply(dat$ImageHeight, dat[,deptag], unique), length)>1
  if(any(multidim)){
    message("Warning:\n There is more than one unique value per deployment for ImageWidth and/or ImageHeight in deployment(s):")
    cat(names(which(multidim)), sep="\n")
  }

  res <- lapply(deps, function(d){
    dt <- subset(dat, dat[,deptag]==d)
    cm <- mods[[d]]$cam.model
    sm <- mods[[d]]$model
    data.frame(dt, radius=predict.r(sm, dt$x/dt$ImageWidth-0.5, dt$y/dt$ImageHeight),
               angle=cm$APratio * (dt$x/dt$ImageWidth-0.5))
  })
  res <- dplyr::bind_rows(res)
  tab <- table(res$sequence_id)
  res$frame_count <- sequence(tab)
  res
}


#seq.data#

#Creates a dataframe of image-to-image changes for each row in dat

#INPUT
# A dataframe dat produced by predict.pos with (at least) columns:
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
# dat: dataframe of position and time data grouped by sequence (see details)
# datetimetag: the name of a field in dat containing text date/time of images
# tformat: the format of the date/time records
# nframes: number of frames per sequence below which time taken is inferred (see details)

#OUTPUT
#A dataframes containing original data for only sequences with two or more images,
#plus additional columns:
# radius, angle: radius and angle of position in first frame of each image
# pixdiff: total pixel distance traveled across image
# dist: total distance travelled over ground (units depend on site calibration units)
# timediff: apparent time taken (timestamp differences between first and last frames) 
# time: inferred time taken (see details)
# speed: travel speed (dist/time)
# frames: number of images in the sequence

#DETAILS
#Input data produced by predict.pos works, with (at least) columns:
#  sequence_id: sequence identifiers
#  a column of character date time data with name matching the datetimetag argument
#
#For sequences with more than nframes images, time is taken directly from timediff.
#For shorter sequences, time is calculated as the number of image transitions
#(frames-1) times the average transition time for those shorter sequences. Defaults
#to using timediff for all.

seq.summary <- function(dat, datetimetag="DateTimeOriginal", tformat="%Y:%m:%d %H:%M:%S",
                        nframes=0){
  calc.mov <- function(dat){
    n <- as.numeric(table(dat$sequence_id))
    dat <- seq.data(dat)
    pixdiff <- with(dat, tapply(pixdiff, sequence_id, sum, na.rm=T) )
    mvdist <- with(dat, tapply(displacement, sequence_id, sum, na.rm=T) )
    tm <- as.POSIXct(dat[,datetimetag], format=tformat, tz="UTC")
    mvtime <- tapply(tm, dat$sequence_id, function(x) as.numeric(diff(range(x)), units="secs"))
    i <- n<=nframes
    mntime <- sum(mvtime[i]) / (sum(n[i]) - sum(i))
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
