library(jsonlite)

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
stopifnot(all(l2c$UID==l2d$UID))
stopifnot(all(l2c$UID==l2r$UID))
l2c$Combined_Key <- ifelse(l2c[["Province/State"]] == "",
  l2c[["Country/Region"]],
  paste0(l2c[["Country/Region"]], ", ", l2c[["Province/State"]]))
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

q=l12#[1:10,]
q$ConfirmedScale <- sqrt(q$Confirmed/max(q$Confirmed))
q$ConfirmedScale[is.na(q$ConfirmedScale)] <- 0
q$DeathsScale <- sqrt(q$Deaths/max(q$Deaths))
q$DeathsScale[is.na(q$DeathsScale)] <- 0

xy <- lapply(1:nrow(q), function(i) c(q[i,"Long"], q[i,"Lat"]))
names(xy) <- q$Combined_Key

toJSON(q)
toJSON(xy)

library(leaflet)

qq <- l12
qq$Confirmed[qq$Confirmed < 0] <- 0
qq$Deaths[qq$Deaths < 0] <- 0
qq$vc <- qq$Confirmed/max(qq$Confirmed)
qq$vd <- qq$Deaths/max(qq$Deaths)
l <- leaflet(qq) %>% addTiles() %>%
  addCircleMarkers(
    color="#F30",
    radius=~20*sqrt(vd)+5,
    stroke = FALSE, fillOpacity = 0.5,
    label=~paste0(Combined_Key, ": ", qq$Confirmed)) %>%
  setView(lng = 10, lat = 25, zoom = 03) %>%
    addProviderTiles("Stamen.TonerLite",
                     group = "Toner",
                     options = providerTileOptions(minZoom = 8, maxZoom = 10)) %>%
    addProviderTiles("Esri.WorldTopoMap",
                     group = "Topo") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
    addProviderTiles("CartoDB.Positron",     group = "CartoDB") %>%
    addLayersControl(baseGroups = c("Toner", "Topo", "Mapnik", "CartoDB"),
                     options = layersControlOptions(collapsed = TRUE))

library(htmlwidgets)
saveWidget(l, "map.html")

library(maps)

map("world", col="lightgrey", mar=c(0,0,0,0))
points(Lat ~ Long, l3c)
points(Lat ~ Long, l12, cex=sqrt(l12$Confirmed/max(l12$Confirmed)), pch=19)
