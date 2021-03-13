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
load("Student.RData")

# This is the structure of the dataset
str(grade)

# 6 relevant correlations:

gradeNum <- grade[,c("math","stats","econ","arts")]


# Adding a linear model to data:

a <- ggplot(data = grade, aes(x = math, y = stats)) + geom_point(aes(shape=gender, color=country), size=2) + geom_smooth(method = "lm", se = TRUE) + theme(text=element_text(face="bold", size=16, family="Garamond"))
b <- ggplot(data = grade, aes(x = math, y = econ)) + geom_point(aes(shape=gender, color=country), size=2) + geom_smooth(method = "lm", se = TRUE) + theme(text=element_text(face="bold", size=16, family="Garamond"))
c <- ggplot(data = grade, aes(x = math, y = arts)) + geom_point(aes(shape=gender, color=country), size=2) + geom_smooth(method = "lm", se = TRUE) + theme(text=element_text(face="bold", size=16, family="Garamond"))
d <- ggplot(data = grade, aes(x = stats, y = econ)) + geom_point(aes(shape=gender, color=country), size=2) + geom_smooth(method = "lm", se = TRUE) + theme(text=element_text(face="bold", size=16, family="Garamond"))
e <- ggplot(data = grade, aes(x = stats, y = arts)) + geom_point(aes(shape=gender, color=country), size=2) + geom_smooth(method = "lm", se = TRUE) + theme(text=element_text(face="bold", size=16, family="Garamond"))
f <- ggplot(data = grade, aes(x = econ, y = arts)) + geom_point(aes(shape=gender, color=country), size=2) + geom_smooth(method = "lm", se = TRUE) + theme(text=element_text(face="bold", size=16, family="Garamond"))

plot <- ggarrange(a, b, c, d, e, f, ncol=2, nrow=3, common.legend = TRUE, legend="right")

ggsave("Student.pdf", plot = plot, 
       device = cairo_pdf,
       width = 12, height = 8, dpi = 150,
       bg = "#ffffff")