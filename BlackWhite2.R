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



## The data set
# "grade" is a fictitious data set of student properties and grade.
load("admission.RData")

admission$Research[admission$Research == 0] <- "no Research"
admission$Research[admission$Research == 1] <- "Research"

Sys.setenv("plotly_username"="TheTrueHOOHA")
Sys.setenv("plotly_api_key"="5o4M119G9QP92JQMNlhq")

X <- admission$`GRE Score`
Y <- admission$`TOEFL Score`
Z <- admission$CGPA
'Admission' <- admission$`Chance of Admit`
s <- admission$`University Rating`

showtext_auto()
windowsFonts(Garamond="Garamond")
font_add("Garamond",regular="garabd.ttf")

p <- plot_ly(admission, x = ~X, y = ~Y, z = ~Z, color = ~Admission, colors = "Greys", showscale=FALSE, size = ~s) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'GRE Score'),
                      yaxis = list(title = 'TOEFL Score'),
                      zaxis = list(title = 'CGPA')
                      ),
         font=list(face="bold", size=16, family="Garamond"))


# Create a shareable link to your chart
chart_link = api_create(p, filename="scatter3d-basic")
chart_link
