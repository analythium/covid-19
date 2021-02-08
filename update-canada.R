## Canada data

suppressPackageStartupMessages(library(jsonlite))
source("functions.R")

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
#x <- read.csv(fn, colClasses=ccl, na.strings = c("NA", "N/A"))
x <- read.csv(fn, na.strings = c("NA", "N/A"), stringsAsFactors=FALSE)
for (i in names(ccl)) {
  if (ccl[i] == "character") {
    if (!is.character(x[[i]]))
      x[[i]] <- as.character(x[[i]])
  } else {
    if (!is.numeric(x[[i]]))
      x[[i]] <- as.numeric(x[[i]])
  }
}
x$date <- as.Date(x$date, "%d-%m-%Y")
x$numtoday[!is.na(x$numtoday) & x$numtoday < 0] <- 0
x$percentoday[!is.na(x$percentoday) & x$percentoday < 0] <- 0
#summary(x)
#str(x)

z <- lapply(sort(unique(x$prname)), function(i) x[x$prname==i,])
names(z) <- sort(unique(x$prname))


cat("OK\nEstimating rates ... ")
## rate is 3-day moving average of 100 * (N[t]/N[t-1] - 1)
for (i in names(z)) {
  s <- straight_ts(z[[i]])
    #if (sum(s$numtotal, na.rm=TRUE) > 0) {
    #  r <- suppressWarnings(rate_ts(s$numtotal))
    #  s$rate <- r[match(seq_len(nrow(s)), r[,"x"]),"r"]
    #  s$double <- ifelse(!is.na(s$rate) & s$rate > 1.00001,
    #    log(2)/log(s$rate), NA)
    #  s$rate <- 100 * (s$rate - 1)
    #} else {
    #    s$rate <- NA
    #    s$double <- NA
    #}
  ntot <- s$numtotal
  rt <- c(NA, ntot[-1] / ntot[-length(ntot)])
  rt[is.infinite(rt)] <- NA
  rt[ntot < 10] <- NA
  s$rate <- movingAverage(100 * (rt - 1), 3)
  s$double <- movingAverage(log(2)/log(rt), 3)
  s$double[is.infinite(s$double)] <- NA
  s$double[s$double <= 0] <- NA
  z[[i]] <- s
}


pr <- sort(unique(x$prname))
pr <- pr[!(pr %in% c("Canada", "Repatriated travellers"))]
tmp <- data.frame(Date=seq(min(x$date), max(x$date), 1))
cn <- colnames(z[[1]])[5:ncol(z[[1]])]
all <- list()
for (i in cn) {
  all[[i]] <- tmp
  for (j in pr) {
    v <- z[[j]][[i]]
    for (ii in 2:length(v))
      if (is.na(v[ii]))
        v[ii] <- v[ii-1]
    all[[i]][[j]] <- v[match(tmp$Date, z[[j]]$date)]
  }
}

cat("OK\nWriting results for Canada ... ")
dir.create("output/api/v1/data/canada")
dir.create("output/api/v1/data/canada/regions")
writeLines(toJSON(z, na="null"),
    "output/api/v1/data/canada/regions/index.json")
writeLines(toJSON(all, dataframe="columns", na="null"),
    "output/api/v1/data/canada/index.json")
