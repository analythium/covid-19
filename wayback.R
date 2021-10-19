## wayback machine backfill

suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(rvest))
source("functions.R")

OBJ <- list(
list(TODAY = "2021-10-10",
url = "https://web.archive.org/web/20211011125745/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-11",
url = "https://web.archive.org/web/20211011125745/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-12",
url = "https://web.archive.org/web/20211012134745/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-13",
url = "https://web.archive.org/web/20211013125506/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-14",
url = "https://web.archive.org/web/20211014145508/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-15",
url = "https://web.archive.org/web/20211015142418/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-16",
url = "https://web.archive.org/web/20211016132318/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-17",
url = "https://web.archive.org/web/20211017224527/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"),

list(TODAY = "2021-10-18",
url = "https://web.archive.org/web/20211019034526/https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"))


for (iiii in 1:length(OBJ)) {

TODAY <- OBJ[[iiii]]$TODAY
url <- OBJ[[iiii]]$url

cat(TODAY, "\n")
flush.console()

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
out$source <- list(url="https://covid19stats.alberta.ca/",
                   time=paste(TODAY, "00:00:00"))
out <- c(out, json, tab)

writeLines(toJSON(out, auto_unbox = TRUE),
    paste0("wayback/", TODAY, ".json"))
TODAY
}

# upload this to api/v1/data/alberta
