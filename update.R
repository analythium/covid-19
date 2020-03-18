#!/usr/bin/env Rscript

library(jsonlite)
library(forecast)

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

## location and slug
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

## formatting
x_c <- as.matrix(x_c[,cn])
x_d <- as.matrix(x_d[,cn])
x_r <- as.matrix(x_r[,cn])
rownames(x) <- rownames(x_c) <- rownames(x_d) <- rownames(x_r) <- slug
colnames(x_c) <- colnames(x_d) <- colnames(x_r) <- as.character(d)

## making data sets
blob <- list()
for (i in slug) {
    z <- data.frame(date=as.Date(d),
        confirmed=x_c[i,],
        deaths=x_d[i,],
        recovered=x_r[i,])
    rownames(z) <- NULL
    blob[[i]] <- z
}

## combined data
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

## forecasting
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
        rawdata=as.list(blob[[k]]),
        observed=list(
            date=as.Date(z$date),
            confirmed=z$confirmed
        ),
        predicted=list(
            date=as.Date(
                seq(d[length(d)]+1, d[length(d)]+m, 1)),
            mean=unclass(pm[,1]),
            lower=unclass(pm[,2]),
            upper=unclass(pm[,3])
        )
    )
    out
}

## write output
dir.create("_stats")
dir.create("_stats/api")
OK <- rep(TRUE, nrow(x))
names(OK) <- rownames(x)
for (i in rownames(x)) {
    out <- try(predict_covid(i), silent = TRUE)
    if (inherits(out, "try-error"))
        out <- NULL
    if (is.null(out)) {
        OK[i] <- FALSE
    } else {
        dir.create(paste0("_stats/", i))
        writeLines(toJSON(out), paste0("_stats/api/", i, "/index.json"))
    }
}
writeLines(toJSON(x[OK,,drop=FALSE]), "_stats/api/index.json")
