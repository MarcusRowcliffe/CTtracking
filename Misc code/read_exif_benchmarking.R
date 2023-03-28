devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/master/CTtracking.r")

dir <- "F:/mid Cornwall camera traps"
subdirs <- c("B02", "B04", "B09", "B10", "B13")

f <- function(d){
  fldr <- file.path(dir, d, "DCIM/100_BTCF")
  start <- Sys.time()
  res <- read.exif(fldr)
  end <- Sys.time()
  c(nrow(res), as.numeric(end-start, units="mins"))
}

tds <- sapply(subdirs, f)
tds <- data.frame(n=tds[1,], mins=tds[2,])
plot(tds)
mod <- lm(mins~I(n/1000), data=tds)
abline(mod)
summary(mod)
coef(mod)[1]*60 #12 sec startup overhead
coef(mod)[2]    #c. 0.5 minute per thousand photos
predict(mod, newdata=data.frame(n=1e6))/60 #7.6 hours per million photos
#speeds could be slower on a slower machine or with more fields extracted