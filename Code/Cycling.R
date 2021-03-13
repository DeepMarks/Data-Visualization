# Type ctrl-L to clear the console
rm(list=ls())                   # Delete all variables
hjSys.setenv(LANG = "en")         # Set language to English

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(digest)
# if needed:
#install.packages("corrplot")
library(corrplot)
# if needed:
#install.packages("GGally")
library(GGally)
library(ggpubr)
library(plotly)
library(showtext)
library(XML)
library(rJava)
library(raster)
library(gplots)
library(wbstats)
library(lubridate)

showtext_auto()
windowsFonts(Garamond="Garamond")
font_add("Garamond",regular="garabd.ttf")


vecshi = function (vec, shi) {
  if(length(vec) <= abs(shi)) {
    rep(NA, length(vec))
  } else {
    if(shi >= 0) {
      c(rep(NA, shi), vec[1:(length(vec)-shi)]) }
    else {
      c(vec[(abs(shi)+1):length(vec)], rep(NA, abs(shi))) } } }

run = htmlTreeParse("Cycling.gpx",
                    error = function (...) {}, useInternalNodes = T)

ele = as.numeric(xpathSApply(run, path = "//trkpt/ele", xmlValue))

time = xpathSApply(run, path = "//trkpt/time", xmlValue)

coor = xpathSApply(run, path = "//trkpt", xmlAttrs)

lat = as.numeric(coor["lat",])

lon = as.numeric(coor["lon",])

geodf = data.frame(lat = lat, lon = lon, ele = ele, time = time)

geodf$lat.p1 = vecshi(geodf$lat, -1)

geodf$lon.p1 = vecshi(geodf$lon, -1)

geodf$dist.to.prev = apply(geodf, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row["lat.p1"]),
                  as.numeric(row["lon.p1"])),
                c(as.numeric(row["lat"]), as.numeric(row["lon"])),
                lonlat = T)
})

geodf$time = strptime(geodf$time, format = "%Y-%m-%dT%H:%M:%OS")

geodf$time.p1 = vecshi(geodf$time, -1)

geodf$time.diff.to.prev = as.numeric(difftime(geodf$time.p1, geodf$time))

geodf$speed.m.per.sec = geodf$dist.to.prev / geodf$time.diff.to.prev

geodf$speed.km.per.h = geodf$speed.m.per.sec * 3.6

geodf$speed.km.per.h = ifelse(is.na(geodf$speed.km.per.h), 0,
                              geodf$speed.km.per.h)

geodf$lowess.speed = lowess(geodf$speed.km.per.h, f = 0.2)$y

geodf$lowess.ele = lowess(geodf$ele, f = 0.2)$y

plot(geodf$ele, type = "l", bty = "n", xaxt = "n", ylab = "Elevation",
     xlab = "", col = "grey40")
lines(geodf$lowess.ele, col = "grey50", lwd = 4)
legend(x = "bottomright", legend = c("GPS elevation", "LOWESS elevation"),
       col = c("grey40", "grey50"), lwd = c(1, 4), bty = "n")

plot(geodf$speed.km.per.h, type = "l", bty = "n", xaxt = "n",
     ylab = "Speed (km/h)", xlab = "", col = "grey40", family = "Garamond", font = 2, ps = 16, cex = 20)
lines(geodf$lowess.speed, col = "grey50", lwd = 4)
legend(x = "topleft", legend = c("GPS speed", "LOWESS speed"),
       col = c("grey40", "grey50"), lwd = c(1, 4), bty = "n")
abline(h = mean(geodf$speed.km.per.h), lty = 2, col = "black")

plot(rev(geodf$lon), rev(geodf$lat), type = "l", col = "grey50", lwd = 4,
     bty = "n", ylab = "Latitude", xlab = "Longitude")

map = openmap(as.numeric(c(max(geodf$lat)+0.006, min(geodf$lon)-0.006)),
              as.numeric(c(min(geodf$lat)-0.006, max(geodf$lon)+0.006)),
              type = "osm")

transmap = openproj(map, projection = "+proj=longlat")

png("Map.pdf", width = 4000, height = 2000, res = 100)
par(mar = rep(0,4))
plot(transmap, raster = T)
lines(geodf$lon, geodf$lat, type = "l", col = scales::alpha("grey50", .8), lwd = 15)
dev.off()

scatter_morning = time_of_day_scatter_df(time_of_day='morning')
scatter_afternoon = time_of_day_scatter_df(time_of_day='afternoon')
scatter_evening = time_of_day_scatter_df(time_of_day='night')
