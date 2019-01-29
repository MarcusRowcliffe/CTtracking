#Download standalone Exif executable from here: http://www.sno.phy.queensu.ca/~phil/exiftool/
#Unzip and rename the exiftool(-k).exe file to exiftool.exe
#Path and file names in infolder and outfile must be free of spaces
#Images in subfolders of infolder are also processed
read.exif <- function(infolder, outfile, exifpath="C:/Users/Rowcliffe.M/Documents/APPS/ExifTool"){
  wd <- getwd()
  setwd(exifpath)
  cmd <- paste("exiftool -r -csv", infolder, ">", outfile)
  shell(cmd)
  read.csv(outfile)
  setwd(wd)
}

#Processes metadata extracted from images by exiftool to produce a database of original and added tag data.
#INPUT
# data: data frame of metadata
# tagfields: integer, number of tag fields used (typically 4: species, contact, distance, angle)
# tagcolumn: text string naming the column holding tag data
# placecolumn: text string naming the column holding placement ID data
# filecolumn: text string naming the column holding file name data
# datecolumn: text string naming the column holding date/time data
# othercolumns: optional vector of text strings naming any other columns to extract from data
# date.format: text string defining the format of datecolumn, default "%Y:%m:%d %H:%M:%S" (see strptime function)

#OUTPUT
#A dataframe with one row per animal record, and columns:
# placeID: placement ID
# fileID: the name of the image file in which each record occurs
# datetime: POSIXct data for record date and time
# time: proportional time of day (12:00 AM = 0; 12:00 PM = 0.5)
# any other columns copied from data, as defined by othercolumns
# one column per tag field (eg species, distance...)

extract.records <- function(data, tagfields, tagcolumn, placecolumn, filecolumn, datecolumn, othercolumns=NULL, date.format="%Y:%m:%d %H:%M:%S"){
  keys <- strsplit(as.character(data[,tagcolumn]), ", ")
  splitkeys <- lapply(keys, function(x) strsplit(x, ": "))
  heading <- unlist(lapply(splitkeys, function(x) lapply(x, function(y) y[1])))
  value <- unlist(lapply(splitkeys, function(x) lapply(x, function(y) y[2])))
  imgnum <- rep(1:length(keys), unlist(lapply(keys, length)))
  headings<- unique(heading)
  eg <- expand.grid(1:length(keys), headings)
  allcombs <- paste(eg[,1], eg[,2])
  dat <- matrix(value[match(allcombs, paste(imgnum, heading))], nrow=length(keys))
  colnames(dat) <- headings
  pid <- which(headings==placecolumn)
  dt <- strptime(data[,datecolumn], date.format)
  leadcols <- data.frame(placeID=dat[,pid],
                         fileID=data[,filecolumn], 
                         datetime=dt,
                         time=(dt$hour + dt$min/60 + dt$sec/3600) / 24,
                         data[,othercolumns]
  )
  repcols <- dat[,-pid]
  maxind <- ncol(repcols)/tagfields
  colnames(repcols) <- rep(colnames(repcols)[1:tagfields], maxind)
  res <- NULL
  for(i in 0:(maxind-1))
    res <- rbind(res, cbind(leadcols, repcols[,(1:tagfields)+i*tagfields]))
  res <- subset(res, !is.na(species))
  res
}


#Takes a dataframe of animal records with relative angle category data in
# character format (e.g. "0-0.2"), and adds a column with the absolute angel equivalent
# in radians, matching therough placment and camera IDs in placement and camera dataframes

#INPUT
#records: dataframe of animal records with mandatory columns placeID and angle
#placements: dataframe of placement data with mandatory columns placeID and cameraID
#cameras: dataframe of camera data with mandatory columns cameraID and FieldOfView

#OUTPUT
#A reproduction of records with new column absangle giving the absolute angle of 
# relative angle from the input
add.abs.angle <- function(records, placements, cameras){
  fov <- cameras$FieldOfView[match(placements$cameraID[match(records$placeID, placements$placeID)], cameras$cameraID)]
  relangles <- strsplit(as.character(records$angle), "-")
  begin <- fov * as.numeric(unlist(lapply(relangles, function(x) x[1])))
  end <- fov * as.numeric(unlist(lapply(relangles, function(x) x[2])))
  data.frame(records, absangle=paste(begin,end,sep="-"))
}


convert.dates <- function(data, columns, format="%Y-%m-%d %H:%M:%S"){
  if(length(columns)==1){
    posdates <- data.frame(strptime(data[,columns], format))
    names(posdates) <- columns
  }else
    posdates <- as.data.frame(apply(data[,columns], 2, strptime, format))
  data <- data[,-match(columns, names(data))]
  data.frame(data, posdates)
}


#records must have columns placeID, distance, any used to create subset
#placements must have columns placeID, effort
make.ds.dat <- function(records, placements, RegLab=1, area=1, dconversion=1/1000, subset=NULL){
  if(!is.null(subset)) records <- subset(records, subset)
  i <- !(placements$placeID %in% records$placeID)
  dists <- strsplit(as.character(records$distance), "-")
  res <- data.frame(Region.Label=RegLab,
                    Area=area,
                    Sample.Label=c(as.character(records$placeID), as.character(placedat$placeID[i])),
                    distbegin=c(dconversion*as.numeric(unlist(lapply(dists, function(x) x[1]))), rep(NA, sum(i))),
                    distend=c(dconversion*as.numeric(unlist(lapply(dists, function(x) x[2]))), rep(NA, sum(i)))
  )
  res$distance <- (res$distbegin+res$distend)/2
  res$Effort <- placedat$effort[match(res$Sample.Label, placedat$placeID)]
  res
}


make.tr.dat <- function(records, placements){
  camdays <- as.numeric(placements$stop-placements$start)
  contacts <- table(records$placeID)
  contacts <- contacts[match(placements$placeID, names(contacts))]
  contacts[is.na(contacts)] <- 0
  data.frame(placeID=placements$placeID, camdays, contacts=as.numeric(contacts))
}
