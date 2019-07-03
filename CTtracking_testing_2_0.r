source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/CTtracking/CTtracking_2_0.r")

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
path <- "C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/Processing example data/A200"
ff <- list.files(path, pattern=".csv", full.names=TRUE, ignore.case=TRUE)
flist <- lapply(ff, read1)
names(flist) <- ff
for(f in ff) write.csv(flist[[f]], f, row.names=F)

######################
#DATA PROCESSING STEPS

#Define paths to images, animal/calibration tracking data
imgpath <- "C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/Processing example data/A200/A200/A200_2"
animpath <- "C:/path/containing/TrackingCSVfiles/..."
calpath <- "C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/Processing example data/test2"

#Extract metadat from images
edat <- read.exif(imgpath)
edat <- read.csv("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/Processing example data/test2/metadata/edat.csv")

#Create calibration pole digitisation data
cdat <- read.digidat(calpath, edat, flatten=TRUE, annotations=c("height", "distance"))
View(cdat)

#Fite site calibration models and check diagnostic plots
smods <- cal.site(cdat)
for(m in 1:length(smods)) plot.sitecal(smods[[m]])

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

