library(Distance)
library(MASS)

integrate2 <- function(f,lower,upper,...,subs=100){
  h <- (upper-lower)/subs
  sq <- lower+(1:(subs-1))*h
  (upper-lower) * (f(lower,...)+2*sum(f(sq,...))+f(upper,...)) / (2*subs)
}

dfpdf <- function(x, prm, ispoint, key, series, order, exp, w){
  res <- switch(key,
                "unif" = rep(1/w, length(x)),
                "hn" = mrds:::keyfct.hn(x, prm[1]),
                "hr" = mrds:::keyfct.hz(x, prm[2], prm[1])
                )
  aprm <- switch(key,
                 "unif" = prm,
                 "hn" = prm[-1],
                 "hr" = prm[-(1:2)]
                 )
  if(length(aprm)>0){
    adj <- switch(series,
                  "herm" = mrds:::adjfct.herm(x, scaling=w, order, aprm, exp),
                  "cos" = mrds:::adjfct.cos(x, scaling=w, order, aprm, exp),
                  "poly" = mrds:::adjfct.poly(x, scaling=w, order, aprm, exp)
                  )
    res <- res*(1+adj)
  }
  if(ispoint) res <- x*res
  res
} 

esw <- function(prm, ispoint, key, series, order, exp, w){
  res <- integrate2(dfpdf, 0, w, prm, ispoint, key, series, order, exp, w)
  if(ispoint) res <- sqrt(2*res)
  res
}

fitdf <- function(formula, data, newdata=NULL, reps=999, ...){
  args <- list(...)
  covnames <- all.vars(formula)[-1]
  depname <- all.vars(formula)[1]
  if(is.numeric(data[,depname])) names(data)[depname==names(data)] <- "distance" else{
    cats <- strsplit(as.character(data[,depname]), "-")
    data$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    data$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    data$distance <- (data$distbegin + data$distend) / 2
  }
  if("quiet" %in% names(args))
    args <- list(data=data, formula=formula[-2], ...) else
      args <- list(data=data, formula=formula[-2], quiet=TRUE, ...)
  mod <- do.call(ds, args)

  key <- mod$ddf$ds$aux$ddfobj$type
  series <- mod$ddf$ds$aux$ddfobj$adjustment$series
  order <- mod$ddf$ds$aux$ddfobj$adjustment$order
  exp <- mod$ddf$ds$aux$ddfobj$adjustment$exp
  w <- mod$ddf$meta.data$width
  ispoint <- mod$ddf$ds$aux$point
  cfs <- mod$ddf$par
  vcov <- abs(solve(-mod$ddf$hessian))
  if(is.na(vcov[1])){
    message("Model failed to converge, no edd calculated")
    return(list(ddf=mod, edd=NULL))
  }
  scfs <- mvrnorm(reps, cfs, vcov)

  if(length(covnames)==0){
    ESW <- esw(exp(cfs), ispoint, key, series, order, exp, w)
    SE <- sd(apply(exp(scfs), 1, esw, ispoint, key, series, order, exp, w))
    prdn <- data.frame(estimate=ESW, se=SE)
  } else

  { if(is.null(newdata)){
      f <- function(txt){
        lbrkt <- regexpr("\\(", txt)
        rbrkt <- regexpr("\\)", txt)
        if(lbrkt == -1) txt else substr(txt, lbrkt[1]+1, rbrkt[1]-1)
      }
      covnames <- lapply(covnames, f)
      univals <- lapply(covnames, function(cov) unique(data[cov])[,1])
      newdata <- expand.grid(univals)
      names(newdata) <- unlist(covnames)
    } else
      for(nm in names(newdata)) levels(newdata[[nm]]) <- levels(data[[nm]])

    ff <- strsplit(mod$ddf$dsmodel[2], "formula = ")[[1]][2]
    ff <- formula(substr(ff, 1, nchar(ff)-1))
    m <- model.frame(ff, newdata)
    mat <- model.matrix(ff, m)

    sc.ind <- match(names(mod$ddf$ds$aux$ddfobj$scale$parameters), c(names(cfs)))
    excl <- 1:max(sc.ind)
    prmat <- cbind(mat %*% cfs[sc.ind], cfs[-excl])
    if(key=="hr") prmat <- cbind(cfs[1], prmat)
    ESW <- apply(exp(prmat), 1, esw, ispoint, key, series, order, exp, w)

    prmat <- matrix(scfs[, sc.ind] %*% t(mat), ncol=1)
    if(key=="hr") prmat <- cbind(scfs[, 1], prmat)
    esws <- matrix(apply(prmat, 1, esw, ispoint, key, series, order, exp, w), ncol=nrow(newdata))
    SE <- apply(esws, 2, sd, na.rm=TRUE)
    prdn <- data.frame(newdata, estimate=ESW, se=SE)
  }
  list(ddf=mod$ddf, edd=prdn)
}

AICdf <- function(mods){
  getf <- function(m){
    ff <- strsplit(m$ddf$dsmodel[2], "formula = ")[[1]][2]
    substr(ff, 1, nchar(ff)-1)
  }
  ff <- unlist(lapply(mods, getf))
  AIC <- unlist(lapply(mods, function(m) m$ddf$criterion))
  dAIC <- round(AIC-min(AIC), 2)
  AICw <- exp(-0.5*dAIC)
  AICw <- round(AICw/sum(AICw), 3)
  data.frame(ff,AIC, dAIC, AICw)[order(AIC), ]
}
