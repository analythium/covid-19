## scrape Alberta stats reports

cat("Loading packages ... ")
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(xml2))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(htmlwidgets))
source("functions.R")

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
TODAY <- as.Date(Sys.time())

## write json
cat("OK\nWriting results for Alberta ... ")
dir.create("output/api/v1/data", recursive=TRUE)
dir.create("output/api/v1/data/alberta")
dir.create("output/api/v1/data/alberta/latest")
writeLines(toJSON(out, auto_unbox = TRUE),
    "output/api/v1/data/alberta/latest/index.json")
writeLines(toJSON(out, auto_unbox = TRUE),
    paste0("output/api/v1/data/alberta/", TODAY, ".json"))

#tab1n <- tab[[1]][-(1:2),c(1,2,4,6,8)]
#colnames(tab1n) <- c("Age", "Female", "Male", "Unknown", "All")
#tab1p <- tab[[1]][-(1:2),c(1,3,5,7,9)]
#colnames(tab1p) <- c("Age", "Female", "Male", "Unknown", "All")
#for (i in 2:5) {
#  tab1n[,i] <- as.numeric(gsub(",", "", tab1n[,i]))
#  tab1p[,i] <- as.numeric(gsub(",", "", tab1p[,i]))
#}


abi <- 6
for (iii in 1:length(json)) {
  tmp <- try(sort(json[[iii]]$x$data$name))
  cat(tmp, "\n")
  if (!is.null(tmp) && !inherits(tmp, "try-error"))
    if (all(tmp[1:3] == c("Calgary Zone", "Central Zone","Edmonton Zone")))
    abi <- iii
}
ab <- data.frame(Date=json[[abi]]$x$data$x[[1]])
for (i in 1:length(json[[abi]]$x$data$name)) {
  nam <- gsub(" Zone", "", json[[abi]]$x$data$name[i])
  ab[[nam]] <- json[[abi]]$x$data$y[[i]]
}
abr <- abd <- ab
for (i in 2:ncol(ab)) {
#  r <- suppressWarnings(rate_ts(ab[,i]))
#  r <- r[match(seq_len(nrow(ab)), r[,"x"]),"r"]
#  abd[,i] <- ifelse(!is.na(r) & r > 1.00001, log(2)/log(r), NA)
#  abr[,i] <- 100 * (r - 1)
  rt <- c(NA, ab[-1,i] / ab[-nrow(ab),i])
  rt[is.infinite(rt)] <- NA
  rt[ab[,i] < 10] <- NA
  abr[,i] <- movingAverage(100 * (rt - 1), 3)
  abd[,i] <- movingAverage(log(2)/log(rt), 3)
  abd[is.infinite(abd[,i]),i] <- NA
}

writeLines(toJSON(list(numtotal=ab, rate=abr, double=abd),
    dataframe="columns", na="null"),
    "output/api/v1/data/alberta/index.json")

cat("OK\nGetting global data ... ")
f0 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

d1c <- read.csv(paste0(f0, "time_series_covid19_confirmed_US.csv"),
                stringsAsFactors = FALSE, check.names=FALSE)
d1d <- read.csv(paste0(f0, "time_series_covid19_deaths_US.csv"),
                stringsAsFactors = FALSE, check.names=FALSE)
d2c <- read.csv(paste0(f0, "time_series_covid19_confirmed_global.csv"),
                stringsAsFactors = FALSE, check.names=FALSE)
d2d <- read.csv(paste0(f0, "time_series_covid19_deaths_global.csv"),
                stringsAsFactors = FALSE, check.names=FALSE)
#d2r <- read.csv(paste0(f0, "time_series_covid19_recovered_global.csv"),
#                stringsAsFactors = FALSE, check.names=FALSE)

d1c <- d1c[!is.na(d1c$UID),]
d1d <- d1d[!is.na(d1d$UID),]
d1c <- d1c[order(d1c$UID),]
d1d <- d1d[order(d1d$UID),]

st <- sort(unique(d1c$Province_State))
cold <- grepl("/", colnames(d1c))
d3c <- d3d <- matrix(NA, length(st), sum(cold))
dimnames(d3c) <- dimnames(d3d) <- list(st, colnames(d1c)[cold])
xy3 <- matrix(NA, length(st), 2)
dimnames(xy3) <- list(st, c("Long", "Lat"))
for (i in st) {
  ii <- d1c$Province_State==i
  d3c[i,] <- colSums(d1c[ii, cold], na.rm=TRUE)
  d3d[i,] <- colSums(d1d[ii, cold], na.rm=TRUE)
  xy3[i,"Long"] <- median(d1c[ii, "Long_"])
  xy3[i,"Lat"] <- median(d1c[ii, "Lat"])
}

clast1 <- colnames(d1c)[ncol(d1c)]
l1c <- d1c[,!cold]
l1d <- d1d[,!cold]
stopifnot(all(l1c$UID==l1d$UID))

l1c$Confirmed <- d1c[,clast1]
l1c$Deaths <- d1d[,clast1]

clast2 <- colnames(d2c)[ncol(d2c)]
l2c <- d2c[,1:4]
l2d <- d2d[,1:4]
#l2r <- d2r[,1:4]
l2c$Combined_Key <- ifelse(l2c[["Province/State"]] == "",
  l2c[["Country/Region"]],
  paste0(l2c[["Country/Region"]], ", ", l2c[["Province/State"]]))
l2d$Combined_Key <- ifelse(l2d[["Province/State"]] == "",
  l2d[["Country/Region"]],
  paste0(l2d[["Country/Region"]], ", ", l2d[["Province/State"]]))
l2c <- l2c[order(l2c$Combined_Key),]
l2d <- l2d[order(l2d$Combined_Key),]
stopifnot(all(l2c$Combined_Key==l2d$Combined_Key))
l2c$Confirmed <- d2c[,clast2]
l2c$Deaths <- d2d[,clast2]
#l2c$Recovered <- d2r[,clast2]
l3c <- data.frame(Combined_Key=paste0(rownames(xy3), ", US"),
  xy3, Confirmed=d3c[,clast1], Deaths=d3d[,clast1],
  stringsAsFactors = FALSE)
l1 <- l1c[,c("Combined_Key","Long_", "Lat", "Confirmed", "Deaths")]
l2 <- l2c[,c("Combined_Key","Long", "Lat", "Confirmed", "Deaths")]
colnames(l1) <- colnames(l2)

l12 <- rbind(l3c, l2[l2$Combined_Key != "US",])
l12$Confirmed[l12$Confirmed < 0] <- 0
l12$Deaths[l12$Deaths < 0] <- 0
l12 <- l12[l12$Long != 0 & l12$Lat != 0,]

q <- l12
q <- q[!is.na(q$Long) & !is.na(q$Lat),]
q$ConfirmedScale <- sqrt(q$Confirmed/max(q$Confirmed))
q$ConfirmedScale[is.na(q$ConfirmedScale)] <- 0
q$DeathsScale <- sqrt(q$Deaths/max(q$Deaths))
q$DeathsScale[is.na(q$DeathsScale)] <- 0

dt1 <- as.Date(strptime(colnames(d3c), "%m/%e/%y", "UTC"))
dt2 <- as.Date(strptime(colnames(d2c)[-(1:4)], "%m/%e/%y", "UTC"))
stopifnot(all(dt1==dt2))

rownames(d3c) <- paste0("US, ", rownames(d3c))
rownames(d3d) <- paste0("US, ", rownames(d3d))

biggies <- d2c[[2]][d2c[[1]]!=""]
zzz <- d2c[[2]][d2c[[2]] %in% biggies & d2c[[1]]==""]
biggies <- unique(biggies[!(biggies %in% zzz)])

for (b in biggies) {
  tmpc <- colSums(d2c[d2c[[2]]==b,-(1:4)], na.rm=TRUE)
  newc <- data.frame("Province/State"="",
                     "Country/Region"=b,
                     Lat=NA, Long=NA,
                     t(data.matrix(tmpc)),
                     check.names = FALSE)
  tmpd <- colSums(d2d[d2d[[2]]==b,-(1:4)], na.rm=TRUE)
  newd <- data.frame("Province/State"="",
                     "Country/Region"=b,
                     Lat=NA, Long=NA,
                     t(data.matrix(tmpd)),
                     check.names = FALSE)
  d2c <- rbind(d2c, newc)
  d2d <- rbind(d2d, newd)
}

tmp1 <- as.matrix(d2c[,-(1:4)])
rownames(tmp1) <- ifelse(d2c[["Province/State"]] == "",
  d2c[["Country/Region"]],
  paste0(d2c[["Country/Region"]], ", ", d2c[["Province/State"]]))
dc <- data.frame(Date=dt1,
  cbind(t(as.matrix(d3c)), t(tmp1)),
  check.names = FALSE)
tmp2 <- as.matrix(d2d[,-(1:4)])
rownames(tmp2) <- ifelse(d2d[["Province/State"]] == "",
  d2d[["Country/Region"]],
  paste0(d2d[["Country/Region"]], ", ", d2d[["Province/State"]]))
dd <- data.frame(Date=dt1,
  cbind(t(as.matrix(d3d)), t(tmp2)),
  check.names = FALSE)
rownames(q) <- rownames(dc) <- rownames(dd) <- NULL

stopifnot(all(colnames(dc)==colnames(dd)))
stopifnot(length(colnames(dc)[duplicated(colnames(dc))]) == 0)

cat("OK\nWriting global data ... ")
dir.create("output/api/v1/data/world")
dir.create("output/api/v1/data/world/latest")
writeLines(toJSON(q),
  "output/api/v1/data/world/latest/index.json")
dir.create("output/api/v1/data/world/confirmed")
writeLines(toJSON(dc),
  "output/api/v1/data/world/confirmed/index.json")
dir.create("output/api/v1/data/world/deaths")
writeLines(toJSON(dd),
  "output/api/v1/data/world/deaths/index.json")

dir.create("output/data")

cat("OK\nUpdating AB area level data ... ")

## locally checked out branch or api
#baseurl <- "output/api/v1/data/alberta/"
baseurl <- "https://analythium.github.io/covid-19/api/v1/data/alberta/"

SEQ <- as.character(seq(as.Date("2020-03-20"), TODAY, 1))
Map <- list()
for (i in SEQ) {
  cat(i, "\n")
  tmp <- try({
    if (i == SEQ[length(SEQ)]) {
      fromJSON(paste0("output/api/v1/data/alberta/", i, ".json"))
    } else {
      fromJSON(paste0(baseurl, i, ".json"))
    }
  })
  if (!inherits(tmp, "try-error")) {
    if ("areas" %in% names(tmp)) {
      Map[[i]] <- tmp[["areas"]]
    } else {
      for (j in names(tmp)) {

        Check1 <- !is.null(tmp[[j]]$x$options$crs$crsClass)

        if (Check1) { # node15
          #break
          z <- tmp[[j]]
          if (i %in% c("2020-03-21", "2020-03-23")) {
            zz <- f1(z$x$calls[[2]][[2]][[7]])
            Map[[i]] <- list(area=sapply(zz, "[[", 1),
              cases=as.integer(gsub(" case(s)", "", sapply(zz, "[[", 2), fixed=TRUE)))
          } else {
            if (as.Date(i) > as.Date("2021-04-23")) {
              # Municipality: 116
              zzM <- do.call(rbind, lapply(f1(z$x$calls[[2]][[3]][[5]]), f2))
              # Local geographic area: 133
              zzG <- do.call(rbind, lapply(f1(z$x$calls[[2]][[4]][[5]]), f2))
            } else {
              # Municipality: 116
              #zzM <- do.call(rbind, lapply(f1(z$x$calls[[2]][[2]][[7]]), f2))
              zzM <- if (as.Date(i) > as.Date("2020-11-24")) {
                do.call(rbind, lapply(f1(z$x$calls[[2]][[3]][[7]]), f2))
              } else {
                do.call(rbind, lapply(f1(z$x$calls[[2]][[2]][[7]]), f2))
              }
              # Local geographic area: 133
              zzG <- if (as.Date(i) > as.Date("2020-11-24")) {
                do.call(rbind, lapply(f1(z$x$calls[[2]][[4]][[7]]), f2))
              } else {
                do.call(rbind, lapply(f1(z$x$calls[[2]][[3]][[7]]), f2))
              }
            }
            if (nrow(zzM) > 5 && sum(zzM[,-1]) != 0) {
              cat(j, "\n")
              #print(dim(zzM))
              Map[[i]] <- list(municipalities=zzM, local=zzG)
              if (nrow(Map[[i]]$local)<=1)
                Map[[i]]$local <- NULL
            }
          }
        }
      }
    }
  } else {
    cat("Error loading JSON blob: used blob from previous day\n")
    Map[[i]] <- Map[[length(Map)]]
  }
}

SEQ <- names(Map)
n <- length(Map)
Areas <- as.character(Map[[n]]$local[,1])
tmp <- sapply(strsplit(Areas, " (", fixed=TRUE), "[[", 1L)
tmp2 <- gsub(" ", "", tolower(tmp))
Ar <- data.frame(
  new=Areas,
  new_short=tmp,
  new_lower=tmp2,
  stringsAsFactors = FALSE)
Munic <- as.character(Map[[n]]$municipalities[,1])

## cases
AA <- matrix(NA, length(Areas), n)
dimnames(AA) <- list(Ar$new_lower, SEQ)
MM <- matrix(NA, length(Munic), n)
dimnames(MM) <- list(Munic, SEQ)
## arrays
d3 <- c("cases", "active", "recovered", "deaths")
AAA <- array(NA, c(length(Areas), n, 4L))
dimnames(AAA) <- list(Ar$new_lower, SEQ, d3)
MMM <- array(NA, c(length(Munic), n, 4L))
dimnames(MMM) <- list(Munic, SEQ, d3)

for (i in 1:n) {
  if ("area" %in% names(Map[[i]])) {
    ii <- match(rownames(AA), gsub(" ", "", tolower(Map[[i]]$area)))
    AA[,i] <- Map[[i]]$cases[ii]
    AAA[,i,"cases"] <- Map[[i]]$cases[ii]
  }
  if ("local" %in% names(Map[[i]])) {
    if (i <= 145) {
      ii <- match(rownames(AA), gsub(" ", "", tolower(Map[[i]]$local$Area)))
    } else {
      nm <- Map[[i]]$local$Area
      tmp <- sapply(strsplit(nm, " (", fixed=TRUE), "[[", 1L)
      tmp2 <- gsub(" ", "", tolower(tmp))
      ii <- match(rownames(AA), tmp2)
    }
    AA[,i] <- Map[[i]]$local$Cases[ii]
    AAA[,i,"cases"] <- Map[[i]]$local$Cases[ii]
    AAA[,i,"active"] <- Map[[i]]$local$Active[ii]
    AAA[,i,"recovered"] <- Map[[i]]$local$Recovered[ii]
    AAA[,i,"deaths"] <- Map[[i]]$local$Deaths[ii]
  }
  if ("municipalities" %in% names(Map[[i]])) {
    ii <- match(rownames(MM), Map[[i]]$municipalities$Area)
    MM[,i] <- Map[[i]]$municipalities$Cases[ii]
    MMM[,i,"cases"] <- Map[[i]]$municipalities$Cases[ii]
    MMM[,i,"active"] <- Map[[i]]$municipalities$Active[ii]
    MMM[,i,"recovered"] <- Map[[i]]$municipalities$Recovered[ii]
    MMM[,i,"deaths"] <- Map[[i]]$municipalities$Deaths[ii]
  }
}
## fixing some data archiving issues
ss <- AA[,"2020-04-06"] == AA[,"2020-04-10"]
ss[is.na(ss)] <- FALSE
AA[ss,c("2020-04-07", "2020-04-08", "2020-04-09")] <- AA[ss,"2020-04-06"]
AAA[ss,c("2020-04-07", "2020-04-08", "2020-04-09"),] <- AAA[ss,"2020-04-06",]

cat("OK\nSaving RData ... ")
save(out, z, all, ab, abr, abd, q, dc, dd, AA, MM, Ar, Map,
  file="output/data/covid-19.RData")

## saving Alberta space-time data array
cat("OK\nSaving AB spatal data ... ")
save(AAA, MMM, Ar,
  file="output/data/covid-19-abmap.RData")


cat("OK\nSaving world map ... ")

l <- leaflet(q) %>% addTiles() %>%
  addCircleMarkers(
    lng=~Long,
    lat=~Lat,
    color="#F30",
    radius=~10*sqrt(ConfirmedScale)+5,
    stroke = FALSE, fillOpacity = 0.5,
    label=~paste0(Combined_Key, ": ", q$Confirmed)) %>%
  setView(lng = 10, lat = 25, zoom = 03) %>%
      addProviderTiles("Esri.OceanBasemap", group = "Esri.OceanBasemap") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("OpenStreetmap","Esri.OceanBasemap",
        "DarkMatter (CartoDB)", "Esri.WorldImagery"),
        options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE))
od <- setwd("output")
saveWidget(l, "map.html")
setwd(od)
cat("OK\nDONE\n\n")

