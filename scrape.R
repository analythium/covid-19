## scrape Alberta stats reports

cat("Loading packages ... ")
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(rvest))

cat("OK\nGetting & parsing html ... ")
url <- "https://covid19stats.alberta.ca/"
h <- read_html(url)
n <- html_nodes(h, 'script')
n <- n[grep("htmlwidget-", n)]
n <- n[!startsWith(as.character(n), "<script>(function()")]
txt <- html_text(n)
json <- lapply(txt, fromJSON)
n2 <- html_nodes(h, 'table')
tab <- list()
for (i in seq_along(n2)) {
    tmp <- try(html_table(n2[i]), silent=TRUE)
    if (!inherits(tmp, "try-error"))
        tab[[length(tab)+1L]] <- tmp[[1L]]
}

names(json) <- paste0("node", seq_along(json))
names(tab) <- paste0("node", seq_along(tab)+length(json))

out <- list()
out$source <- list(url=url, time=Sys.time())

out <- c(out, json, tab)

if (FALSE) {

## Figure 1
## Cumulative COVID-19 cases in Alberta by day
cat("OK\nScraping Fig 1 ... ")
x <- json[[1L]]
out$cumulative <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y)

## Figure 2
## Cumulative COVID-19 cases in Alberta by route of suspected acquisition
## Only includes COVID-19 cases where case report forms have been received
cat("OK\nScraping Fig 2 ... ")
x <- json[[2L]]
out$routes <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y,
    name=x$x$data$name)

## Figure 3
## COVID-19 cases in Alberta by day
cat("OK\nScraping Fig 3 ... ")
x <- json[[3L]]
out$reported <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y)

## Figure 4
## this is static image

## Figure 5
## COVID-19 cases in Alberta by date reported to Alberta Health
cat("OK\nScraping Fig 5 ... ")
x <- json[[4L]]
out$zones <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y,
    name=x$x$data$name)

## Map
cat("OK\nScraping map ... ")
x <- json[[5L]]$x$calls$args[[2L]][[7L]]
x <- gsub("<strong>", "", x)
x <- gsub("</strong>", "", x)
x <- gsub(" case(s)", "", x, fixed=TRUE)
x <- strsplit(x, "<br/>")
out$areas <- list(area=sapply(x, "[[", 1L), cases=as.integer(sapply(x, "[[", 2L)))
## Geo JSON: CRS EPSG3857
if (FALSE) {
    library(sp)
    library(rgdal)
    library(leafletR)
    z <- json[[5L]]$x$calls$args[[2L]][[1L]]
    u <- list()
    for (i in 1:length(z)) {
        m <- cbind(X=z[[i]][[1]]$lng[[1]], Y=z[[i]][[1]]$lat[[1]])
        u[[i]] <- Polygons(list(Polygon(m)), x[[i]][1])
    }
    l <- SpatialPolygons(u, 1:length(z))
    proj4string(l) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    #g@data <- data.frame(area=sapply(x, "[[", 1L))
    toGeoJSON(l, "data/alberta-areas")
    tmp <- readOGR("data/alberta-areas.geojson")
}
## Figure 6
## People tested for COVID-19 in Alberta by day
cat("OK\nScraping Fig 6 ... ")
x <- json[[6L]]
out$tested <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y,
    name=x$x$data$name)

## Table 1
## COVID-19 cases in Alberta by zone
cat("OK\nScraping Table 1 ... ")
out$tablebyzone <- tab[[1L]]

## Table 2
## COVID-19 testing in Alberta
cat("OK\nScraping Table 2 ... ")
x <- tab[[2L]]
colnames(x)[1L] <- "Variable"
x[,2L] <- gsub(",", "", x[,2L])
x[,2L] <- as.integer(x[,2L])
out$testingtotal <- x

## Table 3
## Number of people tested for COVID-19 in Alberta by zone
cat("OK\nScraping Table 3 ... ")
x <- tab[[3L]]
x[,2L] <- gsub(",", "", x[,2L])
x[,2L] <- as.integer(x[,2L])
x[,3L] <- gsub(",", "", x[,3L])
x[,3L] <- as.integer(x[,3L])
out$testingbyzone <- x

}

## write json
cat("OK\nWriting results for Alberta ... ")
dir.create("_stats/api/v1/data")
dir.create("_stats/api/v1/data/alberta")
dir.create("_stats/api/v1/data/alberta/latest")
writeLines(toJSON(out, auto_unbox = TRUE),
    "_stats/api/v1/data/alberta/latest/index.json")
writeLines(toJSON(out, auto_unbox = TRUE),
    paste0("_stats/api/v1/data/alberta/", as.Date(Sys.time()), ".json"))

## Canada data

cat("OK\nNormalizing CSV ... ")

fn <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
h <- read.csv(fn, nrows=1)
ccl <- c("pruid"="integer", "prname"="character", "prnameFR"="character",
    "date"="character", "numconf"="integer", "numprob"="integer",
    "numdeaths"="integer", "numtotal"="integer", "numtested"="integer",
    "numrecover"="integer", "percentrecover"="numeric",
    "ratetested"="numeric", "numtoday"="integer", "percentoday"="numeric")
ccl <- structure(ccl[match(colnames(h), names(ccl))], names=colnames(h))
ccl[is.na(ccl)] <- "character"
x <- read.csv(fn, colClasses=ccl, na.strings = c("NA", "N/A"))
x$date <- as.Date(x$date, "%d-%m-%Y")
x$numtoday[!is.na(x$numtoday) & x$numtoday < 0] <- 0
x$percentoday[!is.na(x$percentoday) & x$percentoday < 0] <- 0
#summary(x)
#str(x)

z <- lapply(sort(unique(x$prname)), function(i) x[x$prname==i,])
names(z) <- sort(unique(x$prname))

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

cat("OK\nEstimating rates ... ")
for (i in names(z)) {
    s <- straight_ts(z[[i]])
    if (sum(s$numtotal, na.rm=TRUE) > 0) {
        r <- suppressWarnings(rate_ts(s$numtotal))
        s$rate <- r[match(seq_len(nrow(s)), r[,"x"]),"r"]
        s$double <- ifelse(!is.na(s$rate) & s$rate > 1.00001,
            log(2)/log(s$rate), NA)
    } else {
        s$rate <- NA
        s$double <- NA
    }
    z[[i]] <- s
}

pr <- sort(unique(x$prname))
tmp <- data.frame(Date=seq(min(x$date), max(x$date), 1))
cn <- colnames(z[[1]])[5:ncol(z[[1]])]
all <- list()
for (i in cn) {
  all[[i]] <- tmp
  for (j in pr) {
    v <- z[[j]][[i]]
    all[[i]][[j]] <- z[[j]][[i]][match(tmp$Date, z[[j]]$date)]
  }
}

cat("OK\nWriting results for Canada... ")
dir.create("_stats/api/v1/data/canada")
dir.create("_stats/api/v1/data/canada/regions")
writeLines(toJSON(z),
    "_stats/api/v1/data/canada/regions/index.json")
writeLines(toJSON(all),
    "_stats/api/v1/data/canada/index.json")
#Canada <- z
#Alberta <- out
#save(Canada, Alberta, file="_stats/data/covid-19-canada.RData")
cat("OK\nDONE\n\n")



