
#Function parameter definitions:
#P: vector of photo number per location
#T: vector of time per location (same length as P)
#param: a named list of model parameters containing list(v, p, g, a, theta):
#paramSE: a named list of model parameters SEs containing list(v, p, g, a, theta):
#	v: speed of movement (either day range or speed while active)
#	p: proportion of time active (set to 1 if v is day range)
#	g: group size
#	a: camera detection radius
#	theta: camera detection arc (in radians)
#	NB - ensure that units are consistent (i.e. distance in v and a, and time in v and T)
#strata: a factor the same length as P defining which stratum each location is in
#areas: a named list giving the area of each stratum (names must correspond to the levels in strata)
#its: the number of resamples for bootstrapping


#Single stratum density estimation
#For use in TRD

STRD <- function(i, P, T, param, strata)
{	x <- strata==levels(strata)[i]
	res <- pi * param$g * sum(P[x]) / (sum(T[x]) * param$v * param$p * param$r * (2+param$theta))
	names(res) <- NULL
	res
}


#Trap rate density estimator, with or without stratification
TRD <- function(P, T, param, strata=NULL, areas=NULL)
{	if(length(P)!=length(T)) stop("P and T have unequal lengths")
	if(!("g" %in% names(param))) param <- c(param,g=1)
	if(!("p" %in% names(param))) param <- c(param,p=1)
	if(is.null(strata))
	{	res <- pi * param$g * sum(P) / (sum(T) * param$v * param$p * param$r * (2+param$theta))
		names(res) <- NULL
	} else
	{	if(is.null(areas)) stop("areas are missing")
		if(length(strata)!=length(P)) stop("strata vector is a different length to P/T")
		if(sum(names(areas) %in% levels(strata)) != length(names(areas)) |
			sum(levels(strata) %in% names(areas)) != length(levels(strata)))
			stop("strata levels do not match areas names")
		nstrata <- length(areas)
		areas <- unlist(areas[order(names(areas))])
		locdens <- sapply(1:nstrata, STRD, P, T, param, strata)
		res <- sum(locdens * areas) / sum(areas)
	}
	res
}

#Single resampled traprate density estimate
#For use in bootTRD

TRDsample <- function(i, P, T, param, strata=NULL, areas=NULL)
{	if(is.null(strata))
	{	x <- sample(1:length(T), replace=TRUE)
	}
	else
	{	nstrata <- length(areas)
		x <- NULL
		for (i in 1:nstrata) x <- c(x, sample(which(strata==levels(strata)[i]), replace=TRUE))
		strata <- sort(strata)
	}
	TRD(P[x], T[x], param, strata, areas)
}


#Dixon test for extremity of highest outlier
dixon <- function(x)
{	x <- sort(x, TRUE)
	(x[1]-x[2]) / (x[1]-x[length(x)])
}
#Simulate Dixon outlier test value from neg binomial distribution with offset T
DvalSim <- function (i, T, size, mu) 
	dixon(rnbinom(length(T),size=size,mu=mu*T))

#Produce Dixon test p value(s) for one or more alpha values 
pdixon <- function(Dval, T, size, mu, its=1000)
{	res <- sort(sapply(1:its, DvalSim, T, size, mu))
	i <- findInterval(Dval,res)
	(its-i)/its
}
nbNLL <- function(param, P, T)
{	size <- exp(param[1])
	mu <- exp(param[2])
	-sum(dnbinom(P, size=size, mu=mu*T, log=TRUE))
}

#Trap rate density estimate with bootstrapped confidence intervals and variance
bootTRD <- function(P, T, param, paramSE, strata=NULL, areas=NULL, its=1000)
{	BSdens <- sapply(1:its, TRDsample, P, T, param, strata, areas)
	BSse <- sd(BSdens)
	Dens <- TRD(P,T,param,strata,areas)
	prms <- ifelse("g" %in% names(param), 5, 4)
	addn <- rep(0,prms)
	addn[which(names(param)=="theta")] <- 2
	Es <- c(Dens,addn+unlist(param))
	SEs <- c(BSse,unlist(paramSE))
	SE <- Dens * sqrt(sum((SEs/Es)^2))
	cbind(Density=Dens, SE=SE)
}

#Trap rate density estimate with bootstrapped confidence intervals and variance,
#both with and without the most extreme trap rate observation removed.
#Returns the probability of the observation being a true outlier based on 
#bootstrapped probability given observed distribution

robustTRD <- function(P, T, param, paramSE, strata=NULL, areas=NULL, boots=1000, outits=1000)
{	rate <- P/T
	OI1 <- which(rate==max(rate))
	OI2 <- which(rate==sort(rate,TRUE)[2])
	mn <- mean(rate)
	vr <- var(rate)
	initsize <- log(ifelse(vr>mn, mn^2/(vr-mn), 100))
	dispar <- exp(optim(c(initsize,log(mn)), nbNLL, P=P, T=T)$par)

	Dval1 <- dixon(rate)
	Dval2 <- dixon(rate[-OI1])
	p <- pdixon(c(Dval1,Dval2), T, dispar[1], dispar[2], outits)

	res <- cbind(bootTRD(P, T, param, paramSE, strata, areas, boots), NA, NA)
	res1 <- cbind(bootTRD(P[-OI1], T[-OI1], param, paramSE, strata, areas, boots), OI1, p[1])
	res2 <- cbind(bootTRD(P[-c(OI1,OI2)], T[-c(OI1,OI2)], param, paramSE, strata, areas, boots), OI2, p[2])

	res <- rbind(res, res1, res2)
	dimnames(res)[[2]][3:4] <- c("Obs#Cut", "OutlierP")
	dimnames(res)[[1]] <- c("All data", "Cut 1", "Cut 2")
	res
}
	


multiTRD <- function(plt, sp,P,T,param) 
{	bootTRD(nPhoto[species==sp | plotID==plt], tDeploy[species==sp | plotID==plt], param)
}

deltaCV <- function(x, se) sqrt(sum( (se/x)^2 ))