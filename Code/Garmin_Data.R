rm(list = ls())
graphics.off()

library(tidyverse)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(data.table)
library(ggmap)


register_google(key="AIzaSyD-F8MF62Kg0ITsjP_q8B1Idva_pURNyFA")

load("gps.RData")

    
    mapImageData <- get_googlemap(center = c(lon = median(gps$X), lat = median(gps$Y)),
                                  zoom = 14,
                                  # size = c(500, 500),
                                  maptype = c("hybrid"))
    
    ggmap(mapImageData,
          extent = "device") + # takes out axes, etc.
      geom_point(aes(x = X,
                     y = Y),
                 data = gps,
                 colour = "yellow",
                 size = 1,
                 pch = 23)


