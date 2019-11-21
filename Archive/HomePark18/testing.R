source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/CTtracking/CTtracking_2_0.r")

path <- "D:/Survey_xxx"

exifdat.cam <- read.exif(file.path(path, "CameraImages"))
camdat <- read.digidat(file.path(path, "CameraData"), 
                       exifdat=exifdat.cam, 
                       annotations=c("distance","height"), 
                       pair=TRUE)
View(camdat)
cmods <- cal.cam(camdat)
plot(cmods)

#exifdat <- read.exif(file.path(path, "DeploymentImages"))
#write.csv(exifdat, file.path(path, "exifdata.csv"), row.names = FALSE)
exifdat <- read.csv(file.path(path, "exifdata.csv"), stringsAsFactors = FALSE)
caldat <- read.digidat(path=file.path(path, "DeploymentData"), 
                       exifdat=exifdat,
                       annotations="height",
                       pair=TRUE,
                       exifcols=c("Make", "Model", "Megapixels"))
View(caldat)
camtable <- make.camtable(caldat, camcolumns=c("Make","Model", "Megapixels"))
View(camtable)
#TEMPORARY FIX
camtable$cam_id <- sample(names(cmods), nrow(camtable), replace = T)
smods <- cal.site(caldat, cmods, camtable)
plot(smods)

animdat <- read.digidat(file.path(path, "AnimalData"), 
                        exifdat, 
                        annotations="species")
table(animdat$species)
unique(subset(animdat, species=="new")$group_id)

posdat <- predict.pos(animdat, smods)
View(posdat)
hist(log10(posdat$radius))
log10(20)

sum(posdat$radius>20)
hist(subset(posdat, radius<20)$radius, breaks=40)
far.seqs <- unique(subset(posdat, radius>20)$sequence_id)
fardat <- subset(posdat, sequence_id %in% far.seqs)
View(fardat)
hist(posdat$radius)

seqdat <- seq.summary(posdat)
View(seqdat)
par(mfrow=c(3,1))
hist(log10(seqdat$dist))
hist(log10(seqdat$time))
hist(log10(subset(seqdat,speed>0.001 & speed<10)$speed))

longseqs <- subset(seqdat, time>120)$sequence_id
View(subset(posdat, sequence_id %in% longseqs))
sbst <- subset(seqdat, time<120 & time>0)
hist(log10(sbst$speed))
1/(mean(1/subset(sbst, species=="Fox")$speed))
1/(mean(1/subset(sbst, species=="Hedgehog")$speed))


#########################################
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/camtools/camtools.R")
#Get camtools.r from github.com/MarcusRowcliffe/camtools

tagdat <- extract.tags(exifdat)
View(tagdat)

#Some tidying up / temporary fixes
names(tagdat)[match(c("CreateDate", "placeID"), names(tagdat))] <- c("date", "station")
dr <- range(as.POSIXct(tagdat$date, format="%Y:%m:%d %H:%M:%S"))
i <- is.na(tagdat$station)
tagdat$station <- as.character(tagdat$station)
tagdat[i,"station"] <- basename(dirname(tagdat$SourceFile))[i] #infer missing station IDs
depdat <- data.frame(station=unique(tagdat$station), start=dr[1], stop=dr[2]) #create deployment data
View(depdat)

#Visual check and create trap rate data
contactdat <- subset(tagdat, contact==1)
plot.deployments(contactdat, depdat)
trdat <- event.count(contactdat, depdat)
chk <- check.dates(contactdat, depdat)
chk$bad.data
View(trdat)


#Define species to analyse
sp <- "Fox"

#Activity analysis
library(activity)
tms <- subset(contactdat, species==sp)$time
plot(fitact(tms))
actmod <- fitact(tms, 
                 bounds=c(18,8)*pi/12,
                 sample="data",
                 reps=100)
plot(actmod)
actmod@act

#Detection zone analysis
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/distanceDF/distancedf.r")
#Get ditancedf.r from github.com/MarcusRowcliffe/distanceDF

dzdat <- subset(posdat, frame_count==1 & species==sp)
dzdat$angle <- abs(dzdat$angle)
hist(dzdat$radius)
radmod <- fitdf(radius~1, dzdat, key="hr", transect="point", order=0)
plot(radmod$ddf, pdf=TRUE)
radmod$edd

angmod <- fitdf(angle~1, dzdat, order=0)
plot(angmod$ddf)
angmod$edd


#Speed analysis
source("C:/Users/rowcliffe.m/OneDrive - Zoological Society of London/GitHub/sbd/sbd_1_0.r")
#Get sbd_1_0.r from github.com/MarcusRowcliffe/sbd
spdat <- subset(seqdat, species==sp & speed>0.001 & speed<10)
(spdest <- hmean(spdat$speed))

#Density analysis
param <- list(v = spdest["mean"] * 14*60^2 / 1000,
              p = actmod@act["act"],
              r = radmod$edd$estimate / 1000,
              theta = angmod$edd$estimate * 2,
              g=1)
paramse <- list(v = spdest["se"] * 24*60^2 / 1000,
                p = actmod@act["se"],
                r = radmod$edd$se / 1000,
                theta = angmod$edd$se * 2,
                g=0)
bootTRD(trdat$Fox, trdat$effort.days, param, paramse)
