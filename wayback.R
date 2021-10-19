## wayback machine backfill

TODAY <- "2021-10-10"
url <- "https://covid19stats.alberta.ca/"

suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(rvest))
source("functions.R")

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

writeLines(toJSON(out, auto_unbox = TRUE),
    paste0("wayback/", TODAY, ".json"))

