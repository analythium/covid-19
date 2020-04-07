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
tab <- html_table(n2[-1L])

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
cat("OK\nWriting results ... ")
dir.create("_stats/api/v1/data")
dir.create("_stats/api/v1/data/canada")
dir.create("_stats/api/v1/data/canada/alberta")
dir.create("_stats/api/v1/data/canada/alberta/latest")
writeLines(toJSON(out, auto_unbox = TRUE),
    "_stats/api/v1/data/canada/alberta/latest/index.json")
writeLines(toJSON(out, auto_unbox = TRUE),
    paste0("_stats/api/v1/data/canada/alberta/", as.Date(Sys.time()), ".json"))
save(out, file="_stats/data/covid-19-canada-alberta.RData")
cat("OK\nDONE\n\n")
