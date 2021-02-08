#' Define a function that:
#'
#' * takes the time series of cases starting at the 1st day of infections at that location,
#' * fit [exponential smoothing state space model (ETS)](http://www.exponentialsmoothing.net/) to the confirmed cases time series,
#' * forecast the model for 14 days, mean prediction and lower/upper confidence intervals (95% nominal coverage),
#' * we fit models on log transformed data where the predictions based on untransformed data were constant,
#' * return the raw data, observed time series, and predictions as a list.
predict_covid <- function(k, m, use_log=FALSE) {
    z <- blob[[k]]
    out <- list(
        region=as.list(x[k,c(6,5,4,3)]),
        rawdata=as.list(blob[[k]]),
        observed=NULL, predicted=NULL, transformation=NULL)
    if (sum(z$confirmed, na.rm=TRUE) == 0)
        return(out)
    day1 <- min(which(diff(z$confirmed) > 0)) + 1L
    z <- z[day1:nrow(z),,drop=FALSE]
    y <- z$confirmed
    n <- length(y)
    if (use_log) {
        tr <- function(x) log(x + 1)
        itr <- function(x) exp(x) - 1
    } else {
        tr <- function(x) x
        itr <- tr
    }
    f <- try(ets(tr(y)))
    if (inherits(f, "try-error")) {
        pm <- matrix(NA, m, 3)
    } else {
        p <- forecast(f, m)
        pm <- itr(cbind(p$mean, p$lower[,2], p$upper[,2]))
    }
    d <- z$date
    out$observed <- list(
        date=as.Date(z$date),
        confirmed=y)
    out$predicted <- list(
        date=as.Date(
            seq(d[length(d)]+1, d[length(d)]+m, 1)),
        mean=unclass(pm[,1]),
        lower=unclass(pm[,2]),
        upper=unclass(pm[,3]))
    out$transformation <- if (use_log)
        "logarithmic" else "identity"
    out
}

fit_ts <- function(y, i, j, method="ets", log=FALSE, k=1) {
  method <- match.arg(method, c("ets", "arima", "lm", "sq"))
  yy <- y[i:j]
  x <- (j+1):(j+k)
  if (log)
    yy <- log(yy)
  if (method %in% c("lm", "sq")) {
    xx <- i:j
    xx2 <- xx^2
    m <- if (method == "lm")
      lm(yy ~ xx) else lm(yy ~ xx + xx2)
    p <- predict(m, data.frame(xx=x, xx2=x^2), interval="prediction")
    fit <- p[,"fit"]
    lwr <- p[,"lwr"]
    upr <- p[,"upr"]
  } else {
    m <- if (method == "ets")
      ets(yy) else auto.arima(yy)
    p <- forecast(m, k)
    fit <- p$mean
    lwr <- p$lower[,"95%"]
    upr <- p$upper[,"95%"]
  }
  if (log) {
    fit <- exp(fit)
    lwr <- exp(lwr)
    upr <- exp(upr)
  }
  if (method == "lm") {
    r <- if (log)
      exp(coef(m)[2L]) else sum(coef(m)) / coef(m)[1L]
  } else {
    r <- fit[1L] / y[j]
  }
  out <- list(i=i, j=j, k=k, method=method, y=y, x=x, obs=y[x],
    rate=as.numeric(r),
    fit=as.numeric(fit), lwr=as.numeric(lwr), upr=as.numeric(upr))
  class(out) <- "fit_ts"
  out
}

straight_ts <- function(zz) {
    zz <- zz[order(zz$date),]
    xx <- seq(min(zz$date), max(zz$date), 1)
    zz <- zz[match(xx, zz$date),]
    zz$date <- xx
    zz
}

rate_ts <- function(y, w=4, ...) {
  res <- list()
  for (j in w:(length(y)-1L)) {
    i <- max(1L, j-w-1L)
    f <- try(fit_ts(y, i, j, method="lm", log=TRUE, k=1), silent=TRUE)
    if (!inherits(f, "try-error"))
        res[[length(res)+1L]] <- f
  }
  t(sapply(res, function(z) c(x=z$x, r=z$rate)))
}
## http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
movingAverage <- function(x, n=1, centered=FALSE) {

    if (centered) {
        before <- floor  ((n-1)/2)
        after  <- ceiling((n-1)/2)
    } else {
        before <- n-1
        after  <- 0
    }

    # Track the sum and count of number of non-NA items
    s     <- rep(0, length(x))
    count <- rep(0, length(x))

    # Add the centered data
    new <- x
    # Add to count list wherever there isn't a
    count <- count + !is.na(new)
    # Now replace NA_s with 0_s and add to total
    new[is.na(new)] <- 0
    s <- s + new

    # Add the data from before
    i <- 1
    while (i <= before) {
        # This is the vector with offset values to add
        new   <- c(rep(NA, i), x[1:(length(x)-i)])

        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new

        i <- i+1
    }

    # Add the data from after
    i <- 1
    while (i <= after) {
        # This is the vector with offset values to add
        new   <- c(x[(i+1):length(x)], rep(NA, i))

        count <- count + !is.na(new)
        new[is.na(new)] <- 0
        s <- s + new

        i <- i+1
    }

    # return sum divided by count
    s/count
}

f1 <- function(zz) {
  zz <- gsub("<br/ >", "<br/>", zz)
  zz <- strsplit(zz, "<br/>")
  zz <- lapply(zz, trimws)
  zz <- lapply(zz, function(z) gsub("<strong>", "", gsub("</strong>", "", z)))
  zz
}
f2 <- function(zzz) {
  out <- data.frame(Area=zzz[1], Cases=0, Active=0, Recovered=0, Deaths=0)
  u <- grepl("Cases", zzz)
  if (any(u))
    out$Cases <- as.integer(strsplit(zzz[which(u)], " ")[[1]][1])
  u <- grepl("Active", zzz)
  if (any(u))
    out$Active <- as.integer(strsplit(zzz[which(u)], " ")[[1]][1])
  u <- grepl("Recovered", zzz)
  if (any(u))
    out$Recovered <- as.integer(strsplit(zzz[which(u)], " ")[[1]][1])
  u <- grepl("Death", zzz)
  if (any(u))
    out$Deaths <- as.integer(strsplit(zzz[which(u)], " ")[[1]][1])
  out
}

