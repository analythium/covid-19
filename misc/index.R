## grab latest data
baseurl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
x_c <- read.csv(paste0(baseurl, "time_series_19-covid-Confirmed.csv"),
    stringsAsFactors = FALSE)
x_d <- read.csv(paste0(baseurl, "time_series_19-covid-Deaths.csv"),
    stringsAsFactors = FALSE)
x_r <- read.csv(paste0(baseurl, "time_series_19-covid-Recovered.csv"),
    stringsAsFactors = FALSE)

## check if columns and attributes match
rownames(x_c) <- apply(x_c[,1:4], 1, paste, collapse="_")
rownames(x_d) <- apply(x_c[,1:4], 1, paste, collapse="_")
rownames(x_r) <- apply(x_c[,1:4], 1, paste, collapse="_")
stopifnot(all(colnames(x_d)==colnames(x_r)))
stopifnot(all(colnames(x_d)==colnames(x_c)))
stopifnot(all(colnames(x_r)==colnames(x_c)))
stopifnot(all(rownames(x_d)==rownames(x_r)))
stopifnot(all(rownames(x_d)==rownames(x_c)))
stopifnot(all(rownames(x_r)==rownames(x_c)))

## pull out attributes
x <- x_c[,1:4]
stopifnot(all(
    colnames(x)==c("Province.State", "Country.Region", "Lat", "Long")))

## parsing dates
cn <- colnames(x_c)[-(1:4)]
d <- strptime(gsub("\\.", "/", gsub("X", "", cn)), "%m/%e/%y", "UTC")
names(d) <- cn
## check if it is sorted
stopifnot(all(d == sort(d)))
## check if no days are missing
dexp <- seq(d[1], d[length(d)], 24*60*60)
stopifnot(all(d == dexp))

xy <- x[,c("Long", "Lat")]

loc <- paste0(x$Country.Region,
    ifelse(x$Province.State == "",
        "",
        paste0("/", x$Province.State)))
slug <- paste0(tolower(gsub(" ", "-",
        gsub("[^[:alnum:] ]", "", x$Country.Region))),
    ifelse(x$Province.State == "",
        "",
        paste0("-", tolower(gsub(" ", "-",
            gsub("[^[:alnum:] ]", "", x$Province.State))))))
colnames(x) <- c("province-state", "country-region", "latitude", "logitude")
x$slug <- slug
x$location <- loc

x_c <- as.matrix(x_c[,cn])
x_d <- as.matrix(x_d[,cn])
x_r <- as.matrix(x_r[,cn])

rownames(x) <- rownames(x_c) <- rownames(x_d) <- rownames(x_r) <- slug
colnames(x_c) <- colnames(x_d) <- colnames(x_r) <- as.character(d)

#rs <- cbind(rowSums(x_c), rowSums(x_d), rowSums(x_r))

blob <- list()
for (i in slug) {
    z <- data.frame(date=d,
        confirmed=x_c[i,],
        deaths=x_d[i,],
        recovered=x_r[i,])
    rownames(z) <- NULL
    blob[[i]] <- z
}

biggies <- unique(x[["country-region"]][duplicated(x[["country-region"]])])
for (j in biggies) {
    xx <- x[x[["country-region"]]==j,,drop=FALSE]
    z <- data.frame(prov="", country=paste0(j, ", Combined"),
        latitude=mean(xx$latitude),
        longitude=mean(xx$logitude),
        slug=paste0(tolower(j), "-combined"),
        location=paste0(j, ", Combined"),
        stringsAsFactors = FALSE)
    colnames(z) <- colnames(x)
    rownames(z) <- z$slug
    x <- rbind(x, z)
    zz <- blob[[rownames(xx)[1]]]
    for (k in rownames(xx)[-1]) {
        zz[,2:4] <- zz[,2:4] + blob[[k]][,2:4]
    }
    blob[[z$slug]] <- zz
}

library(forecast)

j <- "canada-alberta"
j <- "Hungary"
j <- "Italy"
j <- "Canada/British Columbia"


predict_covid <- function(k, m=14) {
    z <- blob[[k]]
    if (sum(z$confirmed, na.rm=TRUE) == 0)
        return(NULL)
    day1 <- min(which(diff(z$confirmed) > 0)) + 1L
    z <- z[day1:nrow(z),,drop=FALSE]
    y <- z$confirmed
    n <- length(y)

    f <- ets(log(y))
    p <- forecast(f, m)
    pm <- exp(cbind(p$mean, p$lower[,2], p$upper[,2]))
    d <- z$date
    out <- list(
        region=as.list(x[k,c(6,5,4,3)]),
        observed=list(
            date=as.Date(z$date),
            confirmed=z$confirmed
        ),
        predicted=list(
            date=as.Date(
                seq(d[length(d)]+24*60*60, d[length(d)]+14*24*60*60, 24*60*60)),
            mean=unclass(pm[,1]),
            lower=unclass(pm[,2]),
            upper=unclass(pm[,3])
        )
    )
    class(out) <- c("covid")
    out
}

plot.covid <- function(x, ...) {
    plot(x$observed$date,
        x$observed$confirmed,
        type="l", lwd=2,
        xlim=range(c(x$observed$date,
        x$predicted$date)), ylim=c(1, max(x$predicted$upper)),
        log="y", las=1,
        main=x$region$location, xlab="Date", ylab="Confirmed")
    polygon(c(x$predicted$date, rev(x$predicted$date)),
            c(x$predicted$lower, rev(x$predicted$upper)),
            border="grey", col="grey")
    lines(x$predicted$date, x$predicted$mean, lty=2)
    invisible(x)
}

library(jsonlite)


f <- lm(log(y) ~ I(1:n)-1)
plot(x$observed$date,
    x$observed$confirmed,
    type="l", lwd=2,
    xlim=range(c(x$observed$date,
        x$predicted$date)), ylim=c(1, max(x$predicted$upper)),
    las=1,
    main=j, xlab="Date", ylab="Confirmed")
lines(0:(n+m), exp(0:(n+m) * coef(f)), lty=2)


# COVID-19 cases in Alberta, Canada
# cases over time and some analyses
library(PVAClone)

dat <- data.frame(
  date=c("2020-03-05", "2020-03-06", "2020-03-07", "2020-03-08", "2020-03-09", "2020-03-10", "2020-03-11",
         "2020-03-12", "2020-03-13", "2020-03-14", "2020-03-15", "2020-03-16", "2020-03-17", "2020-03-18"),
  cases=c(1, 2, 2, 4, 7, 14, 19,
          23, 29, 39, 56, 74, 97, 119))

m <- pva(dat$cases, ricker("none"), 10)

K <- -coef(m)["a"]/coef(m)["b"]

prx <- 0:(nrow(dat)*3)
pr <- numeric(length(prx))
pr[1] <- log(dat$cases[1])
for (i in 2:length(pr))
    pr[i] <- pr[i-1] + coef(m)["a"] + coef(m)["b"]*exp(pr[i-1])
pr <- exp(pr)

plot(prx, pr, type="l",
    ylab="# of cases", xlab=paste("Days since", dat$date[1]))
lines(seq_len(nrow(dat))-1, dat$cases, col=2, type="b", lwd=2, pch=19)
abline(h=K, col=4, lty=3)

diff(dat$cases)/dat$cases[-length(dat$cases)]

library(forecast)
f <- ets(log(dat$cases[-length(dat$cases)]))
plot(forecast(f, 14))

## clustering

nmin <- 14
nn <- sapply(clean, function(z) length(z$observed$date))
nn <- nn[nn >= nmin]
N <- length(nn)
D <- matrix(Inf, length(clean), length(clean))
rownames(D) <- colnames(D) <- names(clean)
K <- D
K[] <- 0
diag(D) <- 0

fun <- function(i, j, last_only=TRUE) {
  si <- clean[[i]]
  sj <- clean[[j]]
  yi <- si$observed$confirmed
  names(yi) <- si$observed$date
  yj <- sj$observed$confirmed
  names(yj) <- sj$observed$date
  n <- min(length(yi), length(yj))
  if (n >= nmin) {
    ## we want to match the end of the time series
    yi <- rev(rev(yi)[seq_len(n)])
    yi <- yi / yi[1L]

    if (last_only) {
      yj <- rev(rev(yj)[seq_len(n)])
      yj <- yj / yj[1L]
    }

    m <- length(yj) - length(yi)
    lyi <- log(yi)
    if (m == 0) {
      dij <- sum((lyi - log(yj))^2)
    } else {
      d <- numeric(m)
      for (k in seq_len(m)) {
        z <- yj[k:(k+n-1L)]
        z <- z / z[1L]
        d[k] <- sum((lyi - log(z))^2)
      }
      k <- which.min(d)
      dij <- d[k]
      z <- yj[k:(k+n-1L)]
      z <- z / z[1L]
    }
    out <- list(d=dij, k=k)
  } else {
    out <- list(d=Inf, k=0)
  }
  out
}

for (iii in seq_len(N)) {
  for (jjj in seq_len(iii-1L)) {
    ui <- names(nn)[iii]
    uj <- names(nn)[jjj]
    v <- fun(ui, uj)
    D[ui,uj] <- D[uj,ui] <- v$d
    K[ui,uj] <- K[uj,ui] <- v$k
  }
}
range(K)
range(D)
D[is.infinite(D)] <- max(D[is.finite(D)])


i <- "canada-alberta"
dd <- D[i, colnames(D) != i]
dd[which.min(dd)]

dm <- as.dist(D)
h <- hclust(dm)

library(vegan)

mds <- monoMDS(dm)
plot(mds)

i <- "canada-alberta"

si <- clean[[i]]
jj <- names(clean)[names(clean) != i]


#j <- "canada-british-columbia"
for (j in jj) {
  sj <- clean[[j]]

  yi <- si$observed$confirmed
  names(yi) <- si$observed$date
  yj <- sj$observed$confirmed
  names(yj) <- sj$observed$date
  n <- min(length(yi), length(yj))
  if (n >= nmin) {
    ## we want to match the end of the time series
    yi <- rev(rev(yi)[seq_len(n)])
    yi <- yi / yi[1L]

    m <- length(yj) - length(yi)
    lyi <- log(yi)
    if (m == 0) {
      dij <- sum((lyi - log(yj))^2)
      #d1 <- names(lyi)[1L]
    } else {
      d <- numeric(m)
      for (k in seq_len(m)) {
        z <- yj[k:(k+n-1L)]
        z <- z / z[1L]
        d[k] <- sum((lyi - log(z))^2)
      }
      k <- which.min(d)
      dij <- d[k]
      #d1 <- names(yj)[k]
      z <- yj[k:(k+n-1L)]
      z <- z / z[1L]
    }
    D[j,"d"] <- dij
    D[j,"k"] <- k
  }
}

logyi <- log(yi[seq_len(n)])
logyj <- log(yj[seq_len(n)])



dat <- data.frame(
  date=c("2020-03-05", "2020-03-06", "2020-03-07", "2020-03-08", "2020-03-09", "2020-03-10", "2020-03-11",
         "2020-03-12", "2020-03-13", "2020-03-14", "2020-03-15", "2020-03-16", "2020-03-17", "2020-03-18"),
  cases=c(1, 2, 2, 4, 7, 14, 19,
          23, 29, 39, 56, 74, 97, 119))
write.csv(dat, row.names=FALSE, file="covid-19-canada-alberta.csv")


x <- read.csv("covid-19-canada-alberta.csv", stringsAsFactors = FALSE)
x$Date <- as.Date(x$Date)
n <- nrow(x)
plot(Total_confirmed ~ Date, x, log="y", type="l")
plot(100*diff(Total_confirmed)/Total_confirmed[-n] ~ Date[-1], x,
    ylab="Daily % change", type="l")


## ----- AB data
library(forecast)
s <- read.csv("./data/covid-19-canada-alberta.csv")
#y <- s$Total_confirmed - s$Total_deaths - s$Total_recovered
y <- s$Total_confirmed
i <- 1 # start date
j <- 14 # end date
k <- 14 # forecast

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
  out <- list(i=i, j=j, k=k, method=method, y=y, x=x, obs=y[x],
    fit=as.numeric(fit), lwr=as.numeric(lwr), upr=as.numeric(upr))
  class(out) <- "fit_ts"
  out
}
plot.fit_ts <- function(x, m=1.25, ...) {
  plot(x$y, ylim=c(0, m*max(x$y)), xlim=c(1, m*length(x$y)),
    type="l", col="grey", lty=2, lwd=2,
    xlab="days", ylab="cases", ...)
  if (x$k < 2) {
    lines(c(x$x, x$x), c(x$lwr, x$upr), col="#ff000044")
    points(x$x, x$obs, col=1, pch=19)
    points(x$x, x$fit, col=2, pch=19)
  } else {
    polygon(c(x$x, rev(x$x)),
      c(x$lwr, rev(x$upr)),
      col="#ff000044", border="#ff000044")
    lines(x$x, x$obs, col=1, lwd=2)
    lines(x$x, x$fit, col=2, lwd=2)
  }
  invisible(x)
}

fit_ts(y, 1, 20, "ets", log=FALSE)
fit_ts(y, 1, 20, "ets", log=TRUE)
fit_ts(y, 15, 20, "lm", log=FALSE)
fit_ts(y, 15, 20, "lm", log=TRUE)
fit_ts(y, 1, 20, "sq", log=FALSE)
fit_ts(y, 1, 20, "sq", log=TRUE)
fit_ts(y, 1, 20, "arima", log=FALSE)
fit_ts(y, 1, 20, "arima", log=TRUE)

plot(fit_ts(y, 1, 20, "ar", log=FALSE, k=3), 2)

eval_ts1 <- function(y, ...) {
  x <- fit_ts(y, ..., k=1)
  c(obs=x$obs, fit=x$fit, lwr=x$lwr, upr=x$upr,
    bias=(x$obs - x$fit), sq=(x$obs - x$fit)^2,
    int=(x$upr-x$lwr),
    inside=as.integer(x$upr >= x$obs && x$lwr <= x$obs))
}
eval_ts1(y, 1, 20, "ets", log=FALSE)
eval_ts <- function(y, ..., w=NULL) {
  res <- list()
  for (j in 2:(length(y)-1L)) {
    i <- if (is.null(w))
      1L else max(1L, j-w-1L)
    res[[length(res)+1L]] <- c(i=i, j=j, eval_ts1(y, ..., i=i, j=j))
  }
  data.frame(do.call(rbind, res))
}
eval_ts(y, method="sq", w=4)
eval_all <- function(y) {
  suppressWarnings({
    res <- list(
      ets=eval_ts(y, method="ets", log=FALSE, w=NULL)[-(1:2),],
      ar=eval_ts(y, method="arima", log=FALSE, w=NULL)[-(1:2),],
      lm3=eval_ts(y, method="lm", log=FALSE, w=3)[-(1:2),],
      lm4=eval_ts(y, method="lm", log=FALSE, w=4)[-(1:2),],
      lm5=eval_ts(y, method="lm", log=FALSE, w=5)[-(1:2),],
      lm6=eval_ts(y, method="lm", log=FALSE, w=6)[-(1:2),],
      lm7=eval_ts(y, method="lm", log=FALSE, w=7)[-(1:2),],
      sq4=eval_ts(y, method="sq", log=FALSE, w=4)[-(1:2),],
      sq5=eval_ts(y, method="sq", log=FALSE, w=5)[-(1:2),],
      sq6=eval_ts(y, method="sq", log=FALSE, w=6)[-(1:2),],
      sq7=eval_ts(y, method="sq", log=FALSE, w=7)[-(1:2),],
      sq8=eval_ts(y, method="sq", log=FALSE, w=8)[-(1:2),],
      sq9=eval_ts(y, method="sq", log=FALSE, w=9)[-(1:2),],
      sq10=eval_ts(y, method="sq", log=FALSE, w=10)[-(1:2),],
      logets=eval_ts(y, method="ets", log=TRUE, w=NULL)[-(1:2),],
      logar=eval_ts(y, method="arima", log=TRUE, w=NULL)[-(1:2),],
      loglm3=eval_ts(y, method="lm", log=TRUE, w=3)[-(1:2),],
      loglm4=eval_ts(y, method="lm", log=TRUE, w=4)[-(1:2),],
      loglm5=eval_ts(y, method="lm", log=TRUE, w=5)[-(1:2),],
      loglm6=eval_ts(y, method="lm", log=TRUE, w=6)[-(1:2),],
      loglm7=eval_ts(y, method="lm", log=TRUE, w=7)[-(1:2),],
      logsq4=eval_ts(y, method="sq", log=TRUE, w=4)[-(1:2),],
      logsq5=eval_ts(y, method="sq", log=TRUE, w=5)[-(1:2),],
      logsq6=eval_ts(y, method="sq", log=TRUE, w=6)[-(1:2),],
      logsq7=eval_ts(y, method="sq", log=TRUE, w=7)[-(1:2),],
      logsq8=eval_ts(y, method="sq", log=TRUE, w=8)[-(1:2),],
      logsq9=eval_ts(y, method="sq", log=TRUE, w=9)[-(1:2),],
      logsq10=eval_ts(y, method="sq", log=TRUE, w=10)[-(1:2),]
    )
  })
  list(results=res, summary=t(sapply(res, colMeans))[,-(1:6)])
}
e <- eval_all(y)
e$summary

v <- e$results$ar
v <- e$results$logsq6

plot(v$obs, v$fit, type="n", xlab="observed", ylab="fitted")
polygon(c(v$obs, rev(v$obs)), c(v$lwr, rev(v$upr)), border="grey", col="grey")
abline(0,1,lty=2)
lines(v$obs, v$fit)


library(jsonlite)

baseurl <- "https://analythium.github.io/covid-19"
load(url(file.path(baseurl, "data", "covid-19.RData")))
load(url(file.path(baseurl, "data", "covid-19-canada-alberta.RData")))

i <- "canada-combined"
i <- "hungary"
e <- eval_all(clean[[i]]$observed$confirmed)
e$summary

v <- e$results$sq4
plot(v$obs, v$fit, type="n", xlab="observed", ylab="fitted")
polygon(c(v$obs, rev(v$obs)), c(v$lwr, rev(v$upr)), border="grey", col="grey")
abline(0,1,lty=2)
lines(v$obs, v$fit)

zz <- list()
for (i in names(clean)) {
  cat(i, "\n")
  flush.console()
  z <- try(eval_all(clean[[i]]$observed$confirmed))
  if (!inherits(z, "try-error"))
    zz[[i]] <- z$summary
}

rn <- rownames(zz[[1]])
rn <- c("ets", "ar", "lm4", "loglm4", "sq6", "logsq6")
table(unlist(sapply(zz, function(z) rn[which.min(abs(z[rn,"bias"]))])))
table(unlist(sapply(zz, function(z) rn[which.min(z[rn,"sq"])])))
table(unlist(sapply(zz, function(z) rn[which.min(abs(0.95-z[rn,"inside"]))])))

## lm can be slightly biased
## sq is least biased
## ets and loglm coverage are best

## i=1, change j


## compare lin/log, ets/lm different window sizes (coverage + bias)
## automate selection
## run on multiple countries to see how it works
## plot 1-day forecasts based on multiple models to see how that looks


