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


write.csv(posdat, file.path(path, "posdat.csv"), row.names = FALSE)
write.csv(seqdat, file.path(path, "seqdat.csv"), row.names = FALSE)

