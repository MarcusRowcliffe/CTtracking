source("C:/path/containing/CodeFile/CTtracking_2_0.r")

#####################
#Code to convert "," in sequence_annotations column to ";"
#This is a once-only process - in future use ; instead of , to separate annotation entries!

read1 <- function(file){
  dat <- read.csv(file, header=F, skip=1)
  hds <- read.csv(file, header=F, nrows=1, as.is=T)
  dat$V5 <- with(dat, paste(V5, V6, sep=";"))
  dat <- dat[, -6]
  names(dat) <- hds
  dat
}
path <- "C:/path/containing/TrackingCSVfiles/..."
ff <- list.files(pth, pattern=".csv", full.names=TRUE, ignore.case=TRUE)
flist <- lapply(ff, read1)
for(f in ff) write.csv(flist[[f]], f, row.names=F)

######################
#DATA PROCESSING STEPS

#Define paths to images, animal/calibration tracking data
imgpath <- "C:/path/containing/ImageFiles/..."
animpath <- "C:/path/containing/TrackingCSVfiles/..."
calpath <- "C:/path/containing/TrackingCSVfiles/..."

#Extract metadat from images
edat <- read.exif(imgpath)

#Create calibration pole digitisation data
cdat <- read.digidat(calpth, edat, annotations=c("height", "distance"))
cdat <- make.poledat(cdat)

#Fite site calibration models and check diagnostic plots
smods <- cal.site(cdat)
for(m in 1:length(smods)) plot.sitecal(smods[[m]])#

#Create animal digitisation data
posdat <- read.digidat(animpath, edat, annotations=c("species"), trans.xy="img.to.vid")

#Add actual radial and angular position estimates to animal digitisation data
#(raw needed data for distance analysis)
posdat <- predict.pos(posdat, smods)

#To extract only the first position of each sequence
#(needed for activity analysis)
cntctdat <- subset(posdat, frame_count==1)

#To summarise movement distances and speeds for each sequence
#(only needed for REM)
seqdat <- seq.summary(posdat)

View(edat)
View(cdat)
View(posdat)
View(cntctdat)
View(seqdat)
