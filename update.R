#!/usr/bin/env Rscript
#' ---
#' title: 'COVID-19 Analysis Methods'
#' author: ''
#' output: 'html_document'
#' ---
#'
#' ## Terms of use
#'
#' This GitHub repo and its contents herein, including all data, and analysis,
#' copyright 2020 [Analythium Solutions](https://www.analythium.io/),
#' all rights reserved, is provided to the public strictly for
#' educational and academic research purposes.
#' The Website relies upon publicly available data from multiple sources,
#' that do not always agree. Analythium Solutions hereby disclaims any
#' and all representations and warranties with respect to the Website,
#' including accuracy, fitness for use, and merchantability.
#' Reliance on the Website for medical guidance or use of the
#' Website in commerce is strictly prohibited.
#'
#' ## Preamble
#'
#' We need a few [R](https://www.r-project.org/) packages:
#' [**jsonlite**](https://CRAN.R-project.org/package=jsonlite)
#' for writing [JSON](https://www.json.org/) data,
#' [**forecast**](https://CRAN.R-project.org/package=forecast)
#' for forecasting time series
cat("Loading packages ... ")
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(forecast))
#' ## Data sources
#'
#' The data we use is updated daily in the
#' [github.com/CSSEGISandData/COVID-19](https://github.com/CSSEGISandData/COVID-19#readme)
#' repository. These data is for the 2019 Novel Coronavirus Visual Dashboard operated by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Also, Supported by ESRI Living Atlas Team and the Johns Hopkins University Applied Physics Lab (JHU APL).
#' The data is copyright 2020 Johns Hopkins University,
#' all rights reserved, is provided to the public strictly for educational
#' and academic research purposes.
#' See data sources and terms of use on the project page.
#'
#' We load three tables:
#'
#' * daily confirmed cases (cumulative) by region,
#' * daily deaths (cumulative) by region,
#' * daily recovered cases (cumulative) by region.
cat("OK\nPulling data ... ")
baseurl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
x_c <- read.csv(paste0(baseurl, "time_series_covid19_confirmed_global.csv"),
    stringsAsFactors = FALSE)
x_d <- read.csv(paste0(baseurl, "time_series_covid19_deaths_global.csv"),
    stringsAsFactors = FALSE)
#' ## Data processing
#'
#' We check consistency across the three tables
cat("OK\nChecking data ... ")
rownames(x_c) <- apply(x_c[,1:4], 1, paste, collapse="_")
rownames(x_d) <- apply(x_c[,1:4], 1, paste, collapse="_")
stopifnot(all(colnames(x_d)==colnames(x_c)))
stopifnot(all(rownames(x_d)==rownames(x_c)))
#' Create a data frame describing the region attributes
cat("OK\nCreating lookup ... ")
x <- x_c[,1:4]
stopifnot(all(
    colnames(x)==c("Province.State", "Country.Region", "Lat", "Long")))
#' Convert dates from the heading to
#' [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' YYYY-MM-DD date format
cn <- colnames(x_c)[-(1:4)]
d <- strptime(gsub("\\.", "/", gsub("X", "", cn)), "%m/%e/%y", "UTC")
names(d) <- cn
#' Check if date are sorted
stopifnot(all(d == sort(d)))
#' Check for missing date
dexp <- seq(d[1], d[length(d)], 24*60*60)
stopifnot(all(d == dexp))
#' Combine Country/Region and Province/State columns
loc <- paste0(x$Country.Region,
    ifelse(x$Province.State == "",
        "",
        paste0("/", x$Province.State)))
#' Create a slogified version of the pasted location
slug <- paste0(tolower(gsub(" ", "-",
        gsub("[^[:alnum:] ]", "", x$Country.Region))),
    ifelse(x$Province.State == "",
        "",
        paste0("-", tolower(gsub(" ", "-",
            gsub("[^[:alnum:] ]", "", x$Province.State))))))
colnames(x) <- c("province-state", "country-region", "latitude", "longitude")
x$slug <- slug
x$location <- loc
#' Format the three matrices
cat("OK\nCreating regional data sets ... ")
x_c <- as.matrix(x_c[,cn])
x_d <- as.matrix(x_d[,cn])
rownames(x) <- rownames(x_c) <- rownames(x_d) <- slug
colnames(x_c) <- colnames(x_d) <- as.character(d)
#' Make a list of region specific data sets combining the 3 matrices
blob <- list()
for (i in slug) {
    z <- data.frame(date=as.Date(d),
        confirmed=x_c[i,],
        deaths=x_d[i,])
    rownames(z) <- NULL
    blob[[i]] <- z
}
#' Tally up all the regions to make a Global combined data
cat("OK\nMaking combined data sets ... ")
z <- data.frame(prov="", country="Global, Combined",
    latitude=0,
    longitude=0,
    slug="global-combined",
    location="Global, Combined",
    stringsAsFactors = FALSE)
colnames(z) <- colnames(x)
rownames(z) <- z$slug
x <- rbind(x, z)
zz <- data.frame(date=as.Date(d),
    confirmed=colSums(x_c),
    deaths=colSums(x_d))
rownames(zz) <- NULL
blob[[z$slug]] <- zz
#' Combine Countries/Regions with multiple Province/State entries
biggies <- unique(x[["country-region"]][duplicated(x[["country-region"]])])
for (j in biggies) {
    xx <- x[x[["country-region"]]==j,,drop=FALSE]
    z <- data.frame(prov="", country=paste0(j, ", Combined"),
        latitude=mean(xx$latitude),
        longitude=mean(xx$longitude),
        slug=paste0(tolower(gsub(" ", "-", j)), "-combined"),
        location=paste0(j, ", Combined"),
        stringsAsFactors = FALSE)
    colnames(z) <- colnames(x)
    rownames(z) <- z$slug
    x <- rbind(x, z)
    zz <- blob[[rownames(xx)[1]]]
    for (k in rownames(xx)[-1]) {
        zz[,2:3] <- zz[,2:3] + blob[[k]][,2:3]
    }
    blob[[z$slug]] <- zz
}
#' ## Time series analysis and forecasting
#'
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
cat("OK\nRunning analyses ... ")
clean <- list()
for (k in names(blob)) {
    out <- predict_covid(k=k, m=7, use_log=TRUE)
    if (!is.null(out$predicted$mean)) {
        Diff <- diff(out$predicted$mean)
        if (max(Diff) == 0)
            out <- predict_covid(k=k, m=7, use_log=FALSE)
    }
    clean[[k]] <- out
}
cat(sprintf("OK\n\t* Results processed successfully for %s regions of %s total",
    sum(names(blob) %in% names(clean)), length(blob)))
cat(sprintf("\n\t* Time series model successfully fitted for %s regions of %s total",
    sum(sapply(clean, function(z) !is.null(z$observed))), length(blob)))
cat(sprintf("\n\t* Logarithmic scale used for %s regions",
    sum(sapply(clean, function(z) !is.null(z$transformation) && z$transformation == "logarithmic"))))
#' ## Saving results
#'
#' Write JSON output into text files: this makes up the API:
#' `_stats` is a temporary folder that contains the API to be deployed
cat("\nWriting restults ... ")
dir.create("_stats")
dir.create("_stats/api")
dir.create("_stats/api/v1")
dir.create("_stats/api/v1/regions")
#' Catch possible problems during model fitting (i.e. exclude
#' regions with a single observation)
for (k in names(clean)) {
    dir.create(paste0("_stats/api/v1/regions/", k))
    writeLines(toJSON(clean[[k]]),
        paste0("_stats/api/v1/regions/", k, "/index.json"))
}
writeLines(toJSON(x), "_stats/api/v1/regions/index.json")
#' Save session info and last update date
info <- list(date=Sys.time(), session=unclass(sessionInfo()))
info$session <- lapply(info$session, function(z) lapply(z, unclass))
writeLines(toJSON(info), "_stats/api/v1/index.json")
#' Save R output for possible reuse by other scripts as needed
dir.create("_stats/data")
save(x, blob, clean, file="_stats/data/covid-19.RData")
#'
#' The API is based on the slugified region names that are listed here:
#' https://analythium.github.io/covid-19/api/v1/regions/.
#' For example the slug `canada-combined` takes us to
#' https://analythium.github.io/covid-19/api/v1/regions/canada-combined/.
#' The date of last update and R session info can be found at
#' https://analythium.github.io/covid-19/api/v1/.
#' [Travis CI](https://travis-ci.org/github/analythium/covid-19)
#' is updating the API using a daily cron job.
cat("OK\nDONE\n\n")
