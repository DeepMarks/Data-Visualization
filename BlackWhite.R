# --> FIRST: set the working directory! 
# Your saved files will end up there!
# Use the menu item 
# --> Session/Set Working Directory/To Source File Location

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


## The data set
# "grade" is a fictitious data set of student properties and grade.
load("admission.RData")

admission$Research[admission$Research == 0] <- "no Research"
admission$Research[admission$Research == 1] <- "Research"

# This is the structure of the dataset
str(admission)

X <- admission$`GRE Score`
Y <- admission$CGPA

showtext_auto()
windowsFonts(Garamond ="Garamond")
font_add("Garamond",regular="garabd.ttf")

# Adding a linear model to data:
theme_set(theme_bw())

plot <- ggplot(data=admission, aes(x = X, y = Y, color = admission$`Chance of Admit`)) + 
                geom_point(aes(size=admission$`University Rating`)) + 
                geom_rug() + xlab("GRE Score") + ylab("CGPA") + 
                labs(caption = "Source: https://www.kaggle.com/mohansacharya/graduate-admissions", align = "right") +
                scale_color_gradient(low = "grey95", high = "grey5") + 
                labs(color="Chance of Admission", size="University Rating") + 
                theme(panel.grid.minor = element_line(size = 0.5),
                      panel.grid.major = element_line(size = 0.5),
                      panel.border=element_rect(fill=NA, size = 1),
                      legend.spacing.y = unit(0.2, 'cm'),
                      legend.margin = margin(0.1,0,0,0, unit="cm"),
                      text=element_text(face="bold", size=16, family="Garamond"),
                      plot.background=element_rect(fill = "#ffffff", color=NA),
                      panel.background=element_rect(fill = "#ffffff"),
                      axis.ticks.x=element_line())

ggsave("Black.pdf", plot = plot, 
       device = cairo_pdf,
       width = 10, height = 6, dpi = 150,
       bg = "#ffffff")
