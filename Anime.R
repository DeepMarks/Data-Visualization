
rm(list = ls())
graphics.off()


# Packages ----------------------------------------------------------------

library(showtext)
library(tidyverse)
library(ggplot2)
library(ggalt)
library(RColorBrewer)
library(data.table)


# Data --------------------------------------------------------------------

load("Data/tidy_anime.RData")

tidy_anime = tidy_anime[!duplicated(tidy_anime$animeID), -c(1,2,3,4,5,12,25)]

tidy_anime = filter(tidy_anime, type %in% c("TV","OVA","ONA"))
tidy_anime = filter(tidy_anime, !(tidy_anime$producers == "Aniplex" & score <=6))
tidy_anime = filter(tidy_anime, !(tidy_anime$producers == "CoMix Wave Films" & score >=6.5))

new = tidy_anime[,c(3,8,12)]
new$new = 1:nrow(new)

tidy_anime = tidy_anime[-c(559,806,1271,18,17,157,3576),]

tidy_anime$popularity = order(tidy_anime$popularity)

table = merge(as.data.frame(table(tidy_anime$producers)),aggregate(tidy_anime, by = list(tidy_anime$producers), FUN = mean),by.x="Var1",by.y="Group.1")

table = table[,-c(3,4,5,6,7,11,12,13,20)]

tidy_anime$colour = factor(case_when(
  tidy_anime$producers == "Aniplex" ~ 1,
  tidy_anime$producers == "Tokyo Movie Shinsha" ~ 2,
  tidy_anime$producers == "CoMix Wave Films"~ 3,
  TRUE ~ 4
))

tidy_anime$shape = factor(case_when(
  tidy_anime$type == "TV" ~ 1,
  tidy_anime$type == "OVA"~ 2,
  tidy_anime$type == "ONA" ~ 3
))

tidy_anime$headquarter = factor(case_when(
  tidy_anime$producers == "Aniplex" ~ 2,
  tidy_anime$producers == "Tokyo Movie Shinsha" ~ 1,
  tidy_anime$producers == "CoMix Wave Films"~ 2
))

tidy_anime$average = factor(case_when(
  tidy_anime$producers == "Aniplex" ~ 1,
  tidy_anime$producers == "Tokyo Movie Shinsha" ~ 2,
  tidy_anime$producers == "CoMix Wave Films"~ 3
))


# Plot --------------------------------------------------------------------

showtext_auto()
windowsFonts(Garamond ="Garamond")
font_add("Garamond",regular="garabd.ttf")

theme_set(theme_bw())

plot = ggplot() +
  geom_point(data = filter(tidy_anime, start_date>=as.Date("1981-01-01") &
                             start_date<=as.Date("2019-12-31") &
                             !is.na(tidy_anime$producers) &
                             !(producers%in%c("Tokyo Movie Shinsha","CoMix Wave Films","Aniplex"))),
             aes(x=start_date,y=score,colour=colour),
             size=0.75,
             alpha=1,
             shape=16) +
  geom_encircle(data = filter(tidy_anime, start_date>=as.Date("1981-01-01") &
                                start_date<=as.Date("2008-12-31") &
                                !is.na(tidy_anime$producers) &
                                producers%in%c("Tokyo Movie Shinsha")),
                aes(x=start_date,y=score,group=producers,fill=average),
                expand=-0.01,
                alpha=0.5,
                color=NA) +
  geom_encircle(data = filter(tidy_anime, start_date>=as.Date("1981-01-01") &
                                start_date<=as.Date("2008-12-31") &
                                !is.na(tidy_anime$producers) &
                                producers%in%c("Tokyo Movie Shinsha")),
                aes(x=start_date,y=score,group=producers,linetype=headquarter),
                expand=-0.01,
                alpha=1,
                size=3,
                color="#EC2049") +
  geom_encircle(data = filter(tidy_anime, start_date>=as.Date("2004-01-01") &
                                start_date<=as.Date("2019-12-31") &
                                !is.na(tidy_anime$producers) &
                                producers%in%c("CoMix Wave Films")),
                aes(x=start_date,y=score,group=producers,fill=average),
                expand=-0.01,
                alpha=0.5,
                color=NA)  +
  geom_encircle(data = filter(tidy_anime, start_date>=as.Date("2004-01-01") &
                                start_date<=as.Date("2019-12-31") &
                                !is.na(tidy_anime$producers) &
                                producers%in%c("CoMix Wave Films")),
                aes(x=start_date,y=score,group=producers,linetype = headquarter),
                expand=-0.01,
                alpha=1,
                size=2.5,
                color="#A7226E")  +
  geom_encircle(data = filter(tidy_anime, start_date>=as.Date("2000-01-01") &
                                start_date<=as.Date("2019-12-31") &
                                !is.na(tidy_anime$producers) &
                                producers%in%c("Aniplex")),
                aes(x=start_date,y=score,group=producers,fill=average),
                expand=-0.01,
                alpha=0.5,
                color=NA)  +
  geom_encircle(data = filter(tidy_anime, start_date>=as.Date("2000-01-01") &
                                start_date<=as.Date("2019-12-31") &
                                !is.na(tidy_anime$producers) &
                                producers%in%c("Aniplex")),
                aes(x=start_date,y=score,group=producers,linetype = headquarter),
                expand=-0.01,
                alpha=1,
                size=2.5,
                color="#2F9599")  +
  geom_point(data = filter(tidy_anime, start_date>=as.Date("1981-01-01") &
                             start_date<=as.Date("2019-12-31") &
                             !is.na(tidy_anime$producers) &
                             producers%in%c("Tokyo Movie Shinsha","CoMix Wave Films","Aniplex")),
             aes(x=start_date,y=score,colour=colour, size = favorites, shape = shape))+
  scale_color_manual(guide = guide_legend(title = "Producer",order=1),
                     values=c("#2F9599",
                              "#EC2049",
                              "#A7226E",
                              "#D6D6D6"),
                     labels=c("Aniplex",
                             "Tokyo Movie Shinsha",
                             "CoMix Wave Films",
                             "others")) +
  scale_linetype_manual(guide = guide_legend(title = "Headquarter", order=2, keywidth = 2.3, override.aes = list(colour = "black", size = 1)),
                          values=c("solid",
                                   "dashed"),
                          labels=c("Nakano, Tokio",
                                   "Chiyoda, Tokio")) +
  scale_fill_manual(guide = guide_legend(title = "Average User Score", order=3),
                    values=c("#91cf60",
                             "#ffffbf",
                             "#fc8d59"),
                    labels=c("7.5 out of 10",
                             "6.5 out of 10",
                             "5.3 out of 10")) +
  scale_size_continuous(range = c(1.5, 6),guide = guide_legend(title = "Times Favourised", order=4, reverse=TRUE)) +
  scale_shape_manual(guide = guide_legend(title = "Format",order=5),
                     values=c(16,
                              17,
                              18),
                     labels=c("Television (TV)",
                              "Original Video Anime (OVA)",
                              "Original Net Anime (ONA)")) +
  scale_y_continuous(breaks=c(1:10), minor_breaks = c()) +
  geom_smooth(data = filter(tidy_anime, start_date>=as.Date("1981-01-01") &
                              start_date<=as.Date("2019-12-31") &
                              !is.na(tidy_anime$producers)),
              aes(x=start_date,y=score), method = "lm",
              color="black",
              se=F) +
  labs(subtitle="Is Aniplex figuring out the secret formula to good anime? ",
       y="Score", 
       x="Year", 
       title="40 Years of Anime Focusing on 3 Producers",
       caption="Source: https://www.kaggle.com/aludosan/myanimelist-anime-dataset-as-20190204") +
  theme(panel.grid.minor = element_line(size = 0.5),
        panel.grid.major = element_line(size = 0.5),
        panel.border=element_rect(fill=NA, size = 1),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(0.2,0,0,0, unit="cm"),
        text=element_text(face="bold", family="Garamond"),
        plot.background=element_rect(fill = "#ffffff", color=NA),
        panel.background=element_rect(fill = "#ffffff"),
        axis.ticks.x=element_line()) +
  coord_fixed(ratio = 1750, xlim=as.Date(c("1979-01-01","2020-12-31")), ylim = c(3.8,9.5), expand = 0)

print(plot)

ggsave("Plots/Anime.pdf", plot = plot, 
       device = cairo_pdf,
       width = 9, height = 6, dpi = 150,
       bg = "#ffffff")
