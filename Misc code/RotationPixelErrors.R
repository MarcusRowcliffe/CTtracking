#Relationship between pole rotation and relative perceived size / proportional error
sq <- seq(0,90,len=100)
plot(sq, 1-cos(sq*pi/180), type="l")
r <- 1
plot(sq, r*sin(sq*pi/180), type="l")


#Impact of rotation and pixel error on distance estimation

#nn: number of pole distances to estimate
#rot: SD +- degrees rotation from vertical
#pixerr: SD +- pixel error
Dhatfunc <- function(nn, rot, pixerr){
  #camcal
  D <- rep(c(1,2,4,8), each=5) #camcal pole distances
  n <- length(D)
  Y <- 1560 #image height
  L <- 1 #camcal pole length
  FS <- 1.5 #true foclen:sensize ratio
  ptrue <- FS * L * Y / D #true / observed pixel lengths
  pobs <- ptrue*cos(rnorm(n, sd=rot*pi/180)) + rnorm(n, sd=pixerr)
  FShat <- mean(D * pobs / (L * Y))
  #D estimation
  Dtrue <- runif(nn, 1, 12)
  LL <- round((Dtrue+rnorm(nn, 1.2, 0.5))/15, 1)
  LL[LL<=0] <- 0.1
  ptr <- FS * LL * Y / Dtrue
  ro <- rnorm(nn, sd=rot*pi/180)
  pe <- rnorm(nn, sd=pixerr)
  pob <- ptr*cos(ro) + pe
  Dhat <- LL * FShat * Y / pob
  Derr <- 1-Dhat/Dtrue
  data.frame(Dtrue, Dhat, Derr, rotation=ro, pixerror=pe)
}
res <- Dhatfunc(1000, 5, 2)
with(res, plot(Dtrue, Dhat))
lines(c(0,20), c(0,20), col=2)
with(res, plot(rotation, Derr))
with(res, plot(pixerror, Derr))
hist(res$Derr)
median(res$Derr)
mean(res$Derr)
q <- (1-0.9)/2
quantile(res$Derr, c(q, 1-q))

sqn <- 21
rosq <- seq(0,20,len=sqn)
pesq <- seq(0,10,len=sqn)
errs <- expand.grid(ro=rosq, pe=pesq)
res <- sapply(1:nrow(errs), function(i){
  res <- Dhatfunc(10000, errs[i,1], errs[i,2])
  mean(quantile(abs(res$Derr), c(0.05,0.95)))
})
contour(rosq, pesq, matrix(res, nrow=sqn), xlab="Rotation deg", ylab="Pixel error")
