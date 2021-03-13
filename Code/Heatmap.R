
rm(list = ls())
graphics.off()

# Packages ----------------------------------------------------------------

library(showtext)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(plyr)

# Data --------------------------------------------------------------------

load("Heatmap.RData")

#col = brewer.pal(11,"YlOrRd")

# Plot --------------------------------------------------------------------

data = as.data.frame(Z)
data$trees = as.numeric(rownames(data))

data = gather(data, trees)
colnames(data)[1] = "features"
data$features = as.numeric(data$features)
data$trees = rep(1:10,30)
data = data[,c(1,3,2)]

plot = ggplot() +
  geom_tile(data = data, aes(x = features, y = trees, fill = value)) +
  scale_y_continuous(breaks = c(1:10),
                  labels=as.character(c(5,10,15,20,100,250,500,750,1000,2000))) +
  scale_x_continuous(breaks = c(1,5,10,15,20,25,30)) +
  scale_fill_gradientn(colors = brewer.pal(9,"YlOrRd"),
                      guide = guide_colorbar(title = "MSE",
                                             ticks.linewidth = 5,
                                             ticks.colour = "white")) +
  theme_bw() +
  labs(title="Heatmap of Random Forest Performance on Testing Data",
       subtitle="Mean Squared Error (MSE) is lower for more trees and fewer features",
       y="Number of Trees", 
       x="Number of Features", 
       caption="Source: https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work") +
  theme(panel.grid.minor = element_line(size = 0.5),
        panel.grid.major = element_line(size = 0.5),
        panel.border=element_rect(fill=NA, size = 1),
        text=element_text(family="Garamond"),
        plot.background=element_rect(fill = "#ffffff", color=NA),
        panel.background=element_rect(fill = "#ffffff"),
        axis.ticks.x=element_line()) +
  coord_fixed(ratio = 1.5, expand = 0)

ggsave("Heatmap.pdf", plot = plot, 
       device = cairo_pdf,
       width = 9, height = 6, dpi = 150,
       bg = "#ffffff")
