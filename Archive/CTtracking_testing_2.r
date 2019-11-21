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
#imgpath <- "C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/Processing example data/A200/A200/A200_2"
animpath <- "C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/FullData/1_species_csv"
calpath <- "C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/FullData/2_calibration_csv/modified"

#Extract metadat from images
#edat <- read.exif(imgpath)
#edat <- read.csv("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/Processing example data/test2/metadata/edat.csv")
edat.cal <- read.csv("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/FullData/calmetadata.csv",
                     stringsAsFactors=FALSE)
edat.img <- read.csv("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/CameraTrapping/REM/Calibration/Mytool/AndreLanna/FullData/metadata.csv",
                     stringsAsFactors=FALSE)

#Create calibration pole digitisation data
cdat <- read.digidat(calpath, edat.cal, flatten=TRUE, annotations=c("height", "distance"))

#Fit site calibration models and check diagnostic plots
smods <- cal.site(cdat)
smodsf <- cal.site(cdat, flex=T)
for(m in 1:length(smods)) plot.sitecal(smods[[m]])

#Create animal digitisation data
posdat <- read.digidat(animpath, edat.img, annotations=c("species"), trans.xy="img.to.vid")

#Add actual radial and angular position estimates to animal digitisation data
#(raw needed data for distance analysis)
posdat <- predict.pos(posdat, smods)

#To extract only the first position of each sequence
#(needed for activity analysis)
cntctdat <- subset(posdat, frame_count==1)

#To summarise movement distances and speeds for each sequence
#(only needed for REM)
seqdat <- seq.summary(posdat)

View(edat.img)
View(edat.cal)
View(cdat)
View(subset(posdat, site_id=="c1000f"))
View(cntctdat)
View(seqdat)

table(posdat$site_id)
table(posdat$xdim)
table(posdat$ydim)
tapply(posdat$ydim, posdat$site_id, unique)

hist(log10(posdat$radius))
mx <- 50
hist(subset(posdat, radius<mx)$radius, breaks=0:mx)



##########################################################################################
dat <- smods$b1000$site.model$data
mod <- try(nls(distance~b1/(rely^b3-b2), data=dat, algorithm="port", 
               start=list(b1=min(dat$distance)/2, b2=min(dat$rely)*0.9, b3=1),
               lower=c(b1=0,b2=0,b3=0), 
               upper=c(b1=Inf,b2=min(dat$rely),b3=Inf),
               fixed=c(b3=1),
               trace=F ))
nd <- data.frame(rely=seq(0,3,len=100), relx=0.5)
pdist <- predict(mod, newdata=nd)
plot(dat$rely, dat$distance, xlim=c(0,2.5))
lines(nd$rely, pdist)
coef(mod)
par(mfrow=c(2,2))
plot(smods$c1200f)
plot(smodsf$c1200f)

plot(smods$a400f)
plot(smodsf$a400f)

plot(smods$a1000f)
plot(smodsf$a1000f)
