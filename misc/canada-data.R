library(jsonlite)

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
summary(x)
str(x)

z <- lapply(sort(unique(x$prname)), function(i) x[x$prname==i,])
names(z) <- sort(unique(x$prname))
sapply(z, dim)

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

for (i in names(z)) {
    s <- straight_ts(z[[i]])
    if (sum(s$numtotal, na.rm=TRUE) > 0) {
        r <- rate_ts(s$numtotal)
        s$rate <- r[match(seq_len(nrow(s)), r[,"x"]),"r"]
        s$double <- ifelse(!is.na(s$rate) & s$rate > 1.00001,
            log(2)/log(s$rate), NA)
    } else {
        s$rate <- NA
        s$double <- NA
    }
    z[[i]] <- s
}
toJSON(z)



baseurl <- "https://analythium.github.io/covid-19"
load(url(file.path(baseurl, "data", "covid-19.RData")))
load(url(file.path(baseurl, "data", "covid-19-canada-alberta.RData")))
get_cases1 <- function(i) {
    z <- clean[[i]]
    y <-z$observed$confirmed
    out <- data.frame(Days=seq_along(y) - 1L, Cases=y)
    out$Region <- x[i, "location"]
    out$Date <- as.Date(z$observed$date)
    out
}
get_cases <- function(i) {
    do.call(rbind, lapply(i, get_cases1))
}
widen <- function(x) {
    xx <- data.frame(Date=sort(unique(COUNTRIES$Date)))
    for (i in unique(x$Region)) {
        tmp <- x[x$Region==i,]
        cc <- tmp$Cases[match(xx$Date, tmp$Date)]
        for (j in which(is.na(cc))) {
            if (j > 1)
                cc[j] <- cc[j-1]
        }
        xx[[i]] <- cc
    }
    xx <- xx[rowSums(!is.na(xx))>1,]
    xx
}

COUNTRIES <- get_cases(c("france", "spain", "italy", "japan",
    "canada-combined", "germany", "morocco", "hungary"))
COUNTRIES$Region <- gsub(", Combined", "", COUNTRIES$Region)
COUNTRIES <- widen(COUNTRIES)

Can <- names(clean)[grepl("canada-", names(clean))]
Can <- Can[!grepl("-princess", Can)]
Can <- Can[Can != "canada-combined"]
Can <- Can[Can != "canada-recovered"]
CANADA <- get_cases(Can)
CANADA$Region <- gsub("Canada/", "", CANADA$Region)
CANADA <- widen(CANADA)

ALBERTA <- do.call(rbind, lapply(1:5, function(i) {
    y <- cumsum(out$zones$y[[i]])
    o <- data.frame(Days=seq_along(y) - 1L, Cases=y)
    o$Region <- out$zones$name[i]
    o$Date <- as.Date(out$zones$x[[i]])
    o
}))
ALBERTA$Region <- gsub(" Zone", "", ALBERTA$Region)
ALBERTA <- widen(ALBERTA)

toJSON(ALBERTA)#, pretty=TRUE)
toJSON(CANADA)#, pretty=TRUE)



p1 <- ggplot(data=COUNTRIES, aes(Date, y, group=Region)) +
    geom_line(aes(color=Region)) +
    labs(title="COVID-19 cases globally") +
    scale_y_continuous(trans='log10') +
    xlab("Date") +
    ylab("Confirmed cases") #+ geom_smooth(aes(color=Region))
ggplotly(p1)

p2 <- ggplot(data=CANADA[CANADA$Date >= as.Date("2020-03-01"),],
            aes(Date, y, group=Region)) +
    labs(title="Provinces/territories in Canada") +
    geom_line(aes(color=Region)) +
    scale_y_continuous(trans='log10') +
    xlab("Date") +
    ylab("Confirmed cases") #+ geom_smooth(aes(color=Region))
ggplotly(p2)

p3 <- ggplot(data=ALBERTA,
            aes(Date, y, group=Zone)) +
    #geom_point(aes(color=Zone)) +
    geom_line(aes(color=Region)) +
    labs(title="Alberta zones") +
    scale_y_continuous(trans='log10') +
    xlab("Date") +
    ylab("Confirmed cases")# +
    #geom_smooth(aes(color=Zone))
ggplotly(p3)

yy <- out$routes$y[[1]] + out$routes$y[[2]] + out$routes$y[[3]]
comp <- data.frame(x=as.Date(out$routes$x[[1]]),
    y=100*c(out$routes$y[[1]]/yy, out$routes$y[[2]]/yy, out$routes$y[[3]]/yy),
    Route=rep(out$routes$name, each=length(yy)))
p4 <- ggplot(comp, aes(x=x, y=y, fill=Route)) +
    geom_area() +
    labs(title="Route of suspected acquisition in Alberta") +
    xlab("Date") +
    ylab("% of cases")
fig4 <- ggplotly(p4)
htmlwidgets::saveWidget(fig4, "fig4.html")

library(sp)
library(rgdal)
library(rgeos)
library(mefa4)

ab <- readOGR("./data/alberta-areas.geojson")
## EPSG:3400
ab <- spTransform(ab, "+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
ab@data$ID2 <- as.character(ab@data$ID)
for (i in c("Edmonton", "Calgary", "Lethbridge", "Red Deer"))
    ab@data$ID2[grep(paste0(i, " - "), ab@data$ID2)] <- i
ab0 <- ab
ab <- gUnaryUnion(ab, ab@data$ID2)
ab <- as(ab, "SpatialPolygonsDataFrame")

cent <- gCentroid(ab, byid=TRUE)


SEQ <- seq(as.Date("2020-03-24"), Sys.Date(), 1)
dates <- c("2020-03-20", "2020-03-22", as.character(SEQ))

j <- lapply(dates, function(i)
    fromJSON(paste0(baseurl, "/api/v1/data/canada/alberta/", i, ".json"))$areas)
names(j) <- dates

d <- matrix(0, length(ab0@data$ID), length(dates))
dimnames(d) <- list(ab0@data$ID, dates)
for (i in dates)
    d[,i] <- j[[i]]$cases[match(ab0@data$ID, j[[i]]$area)]
d <- groupSums(d, 1, ab0@data$ID2)[rownames(ab@data),]

D <- d
storage.mode(D) <- "character"

COL <- hcl.colors(max(round(sqrt(d)))+1, "Lajolla")
COL[1] <- "#E8E8E8"
for (i in dates)
    D[,i] <- COL[round(sqrt(d[,i]))+1]

#ab@data <- data.frame(ab@data, D)
library(magick)

bb <- bbox(ab)
kk <- bb[2,1]+seq(0.02, 0.18, length.out = 200)*diff(bb[2,])

vv <- c(0, 2, 4, 6, 8)+0.6

setwd("misc")

pts <- TRUE
ii <- c(dates[1], dates[1], dates, rep(dates[length(dates)], 3))
for (i in seq_along(ii)) {
    png(paste0(i, ".png"), height=600, width=400)
    op <- par(mar=c(0,0,1,0)+0.1)
    if (pts) {
        plot(ab, col=ifelse(d[,ii[i]]>0, "#e8e8ff", "#E8E8E8"), border="white", main=ii[i])
        plot(cent[d[,ii[i]]>0,], add=TRUE, col="#ff000044", pch=19,
            cex=8*sqrt(d[d[,ii[i]]>0,ii[i]]/max(d))+0.6)
        points(rep(bb[1,1]+0.1*diff(bb[1,]), length(vv)),
               rep(bb[2,1]+0.15*diff(bb[2,]), length(vv)),
               cex=vv, col="#ff000044", pch=19)
        text(bb[1,1]+0.1*diff(bb[1,]), bb[2,1]+0.21*diff(bb[2,]),
            max(d), cex=0.9)
    } else {
        plot(ab, col=D[,ii[i]], border=D[,ii[i]], main=ii[i])
        for (iii in 1:200) {
            lines(bb[1,1]+c(0.02, 0.12)*diff(bb[1,]), kk[c(iii,iii)],
                  col=hcl.colors(200, "Lajolla")[iii])
        }
        text(bb[1,1]+0.15*diff(bb[1,]), kk[1], 0,
             cex=0.8, col=hcl.colors(200, "Lajolla")[200])
        text(bb[1,1]+0.15*diff(bb[1,]), kk[200], max(d),
             cex=0.8, col=hcl.colors(200, "Lajolla")[200])
    }
    par(op)
    dev.off()
}
fn <- paste0(seq_along(ii), ".png")
img <- image_read(fn)
img3 <- image_animate(image_morph(img, 5), 10)
image_write(img3, "covid-ab.gif")
unlink(fn)


xv <- out$routes$x[[1]]
yv <- do.call(cbind, out$routes$y)
rownames(yv) <- xv
colnames(yv) <- out$routes$name
for (i in 1:3)
    yv[-1,i] <- yv[-1,i] - yv[-nrow(yv),i]
yv <- yv/rowSums(yv)

library(forecast)

s <- read.csv("../data/covid-19-canada-alberta.csv")

#y <- s$Total_confirmed - s$Total_deaths - s$Total_recovered
y <- s$Total_confirmed
res <- list()
for (i in 1:(length(y)-7)) {
#    f <- ets(y[1:i])
    f <- ets(log(y[1:i]))
    p <- forecast(f, 7)
    pm <- cbind(mean=p$mean, lower=p$lower[,2], upper=p$upper[,2])
    pm <- exp(pm)
    ob <- y[(i+1):(i+7)]
    res[[i]] <- data.frame(as.matrix(cbind(x=(i+1):(i+7),
        obs=ob, inside=ifelse(pm[,2] <= ob & pm[,3] >= ob, 1, 0), pm)))
}
Res <- data.frame(do.call(rbind, res))
c(p=mean(Res$inside), ssq=sum((Res$obs-Res$pm.mean)^2)/nrow(Res))


for (i in seq_along(res)) {
    png(paste0("ets", i, ".png"), height=400, width=600)
    plot(y[1:(i+7)], xlim=c(1, length(y)), lwd=2,
         ylim=c(0, max(res[[length(res)]]$pm.upper)), type="l",
         xlab="Days since case #1", ylab="# cases")
    polygon(c(res[[i]]$x, rev(res[[i]]$x)),
            c(res[[i]]$pm.lower, rev(res[[i]]$pm.upper)),
            col="#ff000044", border="#ff000044")
    lines(res[[i]]$x, res[[i]]$pm.mean, col=2, lwd=2)
    dev.off()
}

img <- image_read(paste0("ets", seq_along(res), ".png"))

img3 <- image_animate(image_morph(img, 15), 10)

image_write(img3, "covid-ets.gif")
unlink(paste0("ets", seq_along(res), ".png"))



library(PVAClone)

m <- pva(y, ricker("poisson", fixed=c(b=0)), 10)

prx <- 0:(length(y)+7)
pr <- numeric(length(prx))
pr[1] <- log(y[1])
for (i in 2:length(pr))
    pr[i] <- pr[i-1] + coef(m)["a"] + coef(m)["b"]*exp(pr[i-1])
pr <- exp(pr)

plot(prx, pr, type="l",
    ylab="# of cases", xlab="Days since")
lines(seq_along(y)-1, y, col=2, type="b", lwd=2, pch=19)


## log-linear model

wres <- list()
for (w in 3:10) {

#    w <- 4
    ww <- (w+1):(length(y)-w)
    res <- list()
    for (i in seq_along(ww)) {
        yy <- log(y[(ww[i]-w):(ww[i]-1)])
        xx <- seq(-1, 0, length.out = w)
        f <- lm(yy ~ xx)
        p <- predict(f, newdata=data.frame(xx=seq(0, 1, length.out = w+1)[-1]),
                     interval = "pred")
        colnames(p) <- c("mean", "lower", "upper")
        pm <- exp(p)
        ob <- y[(ww[i]):(ww[i]+w-1)]
        res[[i]] <- data.frame(x=(ww[i]):(ww[i]+w-1),
            obs=ob, tt=1:w,
            inside=ifelse(pm[,2] <= ob & pm[,3] >= ob, 1, 0), pm)
    }
    Res <- do.call(rbind, res)
    Res <- Res[Res$tt==2,]
    wres[[length(wres)+1]] <- c(w=w, p=mean(Res$inside),
                                   ssq=sum((Res$obs-Res$mean)^2)/nrow(Res))
    if (FALSE) {
    for (i in seq_along(res)) {
        png(paste0("ets", i, ".png"), height=400, width=600)
        plot(y, xlim=c(1, length(y)),
             ylim=c(0, max(res[[length(res)]]$upper)), type="n",
             xlab="Days since case #1", ylab="# cases")
        polygon(c(res[[i]]$x, rev(res[[i]]$x)),
                c(res[[i]]$lower, rev(res[[i]]$upper)),
                col="#ff000044", border="#ff000044")
        lines(1:max(res[[i]]$x), y[1:max(res[[i]]$x)], col=1, lwd=2)
        lines(res[[i]]$x, res[[i]]$mean, col=2, lwd=2)
        dev.off()
    }

    img <- image_read(paste0("ets", seq_along(res), ".png"))

    img3 <- image_animate(image_morph(img, 15), 10)

    image_write(img3, paste0("covid-ets-w", w, ".gif"))
    unlink(paste0("ets", seq_along(res), ".png"))
    }
}

do.call(rbind, wres)

xx <- read.csv("data/covid-19-canada-alberta.csv")
d <- as.integer(as.Date(xx$Date))-as.integer(as.Date(xx$Date))[1]
y <- xx$Total_confirmed

xx <- get_cases("canada-combined")
d <- xx$Days
y <- xx$Cases

w <- 4
n <- length(y)
r <- data.frame(t(sapply(seq_len(n-w), function(i) {
    logy <- log(y[i:(i+w-1)])
    x <- (-w+1):0
    c(t=mean(i:(i+w-1)), Rt=unname(exp(coef(lm(logy ~ x))[2])))
})))


library(segmented)
library(mgcv)

m <- lm(Rt ~ t, r)
s <- segmented(m)

plot(s, col=2, lty=2, ylim=range(r$Rt))
lines(Rt ~ t, r)
lines(fitted(mgcv::gam(Rt ~ s(t), data=r)), col=3)


## Alberta blob

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


