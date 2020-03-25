## scrape Alberta stats reports

library(xml2)
library(rvest)
library(jsonlite)

url <- "https://covid19stats.alberta.ca/"

h <- read_html(url)
n <- html_nodes(h, 'script')
n <- n[grep("htmlwidget-", n)]
txt <- html_text(n)
json <- lapply(txt, fromJSON)
out <- list()

## Figure 1
## Cumulative COVID-19 cases in Alberta by day
x <- json[[1L]]
out$cumulative <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y)

## Figure 2
## Cumulative COVID-19 cases in Alberta by route of suspected acquisition
## Only includes COVID-19 cases where case report forms have been received
x <- json[[2L]]
out$routes <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y,
    name=x$x$data$name)

## Figure 3
## COVID-19 cases in Alberta by day
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
x <- json[[4L]]
out$zones <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y,
    name=x$x$data$name)

## Map
## COVID-19 cases in Alberta by date reported to Alberta Health
x <- json[[5L]]$x$calls$args[[2L]][[7L]]
x <- gsub("<strong>", "", x)
x <- gsub("</strong>", "", x)
x <- gsub(" case(s)", "", x, fixed=TRUE)
x <- strsplit(x, "<br/>")
out$areas <- list(area=sapply(x, "[[", 1L), cases=as.integer(sapply(x, "[[", 2L)))

## Figure 5
## COVID-19 cases in Alberta by date reported to Alberta Health
x <- json[[6L]]
out$tested <- list(
    xlab=x$x$layout$xaxis$title,
    ylab=x$x$layout$yaxis$title,
    x=x$x$data$x,
    y=x$x$data$y,
    name=x$x$data$name)
out$source <- list(url=url, time=Sys.time())

## write json
dir.create("_stats/api/v1/data")
dir.create("_stats/api/v1/data/canada")
dir.create("_stats/api/v1/data/canada/alberta")
dir.create("_stats/api/v1/data/canada/alberta/latest")
writeLines(toJSON(out), "_stats/api/v1/data/canada/alberta/latest/index.json")
writeLines(toJSON(out), paste0("_stats/api/v1/data/canada/alberta/",
    as.Date(Sys.time()), ".json"))
save(out, file="_stats/data/covid-19-canada-alberta.RData")
