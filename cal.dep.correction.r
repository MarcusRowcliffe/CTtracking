library(tidyverse)

rotate <- function(xy, theta, cent){
  X <- xy[,1] - cent[1]
  Y <- xy[,2] - cent[2]
  r <- sqrt(X^2 + Y^2)
  dir <- atan(X/Y) + ifelse(Y<0, pi, ifelse(X<0, 2*pi, 0))
  data.frame(x = cent[1] + r*sin(dir+theta),
             y = cent[2] + r*cos(dir+theta))
}
shift <- function(xy){
  xy[,1] <- xy[,1] + mean(dat$x1) - mean(dat$x2)
  xy[,2] <- xy[,2] + mean(dat$y1) - mean(dat$y2)
  xy
}
rotateOF <- function(theta, xy){
  centre <- optim(c(x=1512, y=2016), centreOF, xy)$par
  rxy <- rotate(xy, theta, centre)
  sum(sqrt((rxy[,1]-dat$x1)^2 + (rxy[,2]-dat$y1)^2))
}
centreOF <- function(par, xy){
  cdist1 <- sqrt((1512-dat$x1)^2 + (2016-dat$y1)^2)
  cdist2 <- sqrt((par["x"]-dat$x2)^2 + (par["y"]-dat$y2)^2)
  sum((cdist1-cdist2)^2)
}
expandOF <- function(b, xy){
  n <- nrow(dat)
  i <- sequence(1:(n-1))
  j <- rep(2:n, 1:(n-1))
  xy <- xy * b
  d1 <- with(dat, sqrt((x1[i]-x1[j])^2 + (y1[i]-y1[j])^2))
  d2 <- sqrt((xy[i,1]-xy[j,1])^2 + (xy[i,2]-xy[j,2])^2)
  sum((d1-d2)^2)
}

dat <- read.csv("C:/Users/rowcliffe.m/Desktop/New folder (2)/coordinates.csv")
par(mfrow=c(1,1))
i <- sample(1:9, 3)
xy1 <- dat[i, c("x1","y1")]
xy2 <- dat[i, c("x2","y2")]
strt <- c(cx=mean(xy1[,1]-xy2[,1]), cy=mean(xy1[,2]-xy2[,2]), bx=1, by=1, theta=0)
par <- optim(strt, OF, xy1=xy1, xy2=xy2)
plot(xy1, asp=1, xlim=c(0,3024), ylim=c(0,4032))
points(xy2, col=2)
points(correct(par$par, xy2), pch=2, col=2)


rotate <- function(xy, theta, cent){
  X <- xy[,1] - cent[1]
  Y <- xy[,2] - cent[2]
  r <- sqrt(X^2 + Y^2)
  dir <- atan(X/Y) + ifelse(Y<0, pi, ifelse(X<0, 2*pi, 0))
  data.frame(x = cent[1] + r*sin(dir+theta),
             y = cent[2] + r*cos(dir+theta))
}
correct <- function(par, xy){
  xy <- rotate(xy, par["theta"], c(x=mean(xy[,1]), y=mean(xy[,2])))
  xy[,1] <- par["cx"] + xy[,1] * par["bx"]
  xy[,2] <- par["cy"] + xy[,2] * par["by"]
  xy
}
OF <- function(par, xy1, xy2){
  xy <- correct(par, xy2)
  sum(sqrt((xy1[,1]-xy[,1])^2 + (xy1[,2]-xy[,2])^2))
}

