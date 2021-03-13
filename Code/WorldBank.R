
rm(list = ls())
graphics.off()


# Packages ----------------------------------------------------------------

library(showtext)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(wbstats)
library(treemapify)
library(glue)

# Data --------------------------------------------------------------------

data = wb(country="all", indicator=c("NY.ADJ.DCO2.CD",
                                     "NY.ADJ.DNGY.CD"),
          startdate = 2017, enddate = 2017)
data = data[,c(3,5,7)]
data$indicator = case_when(
  data$indicator == "Adjusted savings: carbon dioxide damage (current US$)" ~ "CO2 Damage"
)

data = filter(data, !(data$indicator == "GDP" & data$country != "Switzerland"))

load("Data/countries.RData")

data = merge(data, countries, by = "country")

data$income = factor(case_when(
  data$income == "High income" ~ 1,
  data$income == "Upper middle income" ~ 2,
  data$income == "Lower middle income"~ 3,
  data$income == "Low income" ~ 4
))

data$region = factor(case_when(
  data$region == "East Asia & Pacific" ~ 1,
  data$region == "North America" ~ 2,
  data$region == "South Asia" ~ 3,
  data$region == "Europe & Central Asia" ~ 4,
  data$region == "Middle East & North Africa" ~ 5,
  data$region == "Latin America & Caribbean" ~ 6,
  data$region == "Sub-Saharan Africa" ~ 7
))

data$label = glue(data = data, '{data$country}\n${round(data$value/10^9, 1)}bn')


# Plot --------------------------------------------------------------------

theme_set(theme_bw())

plot = ggplot(data = data, aes(area = value, subgroup = region, subgroup2 = income)) +
  geom_treemap(aes(fill = region, alpha = income),layout = "squarified", color = "black", start = "topleft") +
  geom_treemap_text(aes(label = label), family = "Comfortaa", grow = T, reflow = T, place = "middle", start = "topleft", color = "black") +
  geom_treemap_subgroup_border(start = "topleft", color = "black") +
  scale_fill_manual(guide = guide_legend(title = "Region", order=1),
                    values=c("#EC2049",
                             "#355C7D",
                             "#863d00",
                             "#2F9599",
                             "#A7226E",
                             "#F26B38",
                             "#F7DB4F"),
                    labels=c("East Asia & Pacific",
                             "North America",
                             "South Asia",
                             "Europe & Central Asia",
                             "Middle East & North Africa",
                             "Latin America & Caribbean",
                             "Sub-Saharan Africa")) +
  scale_alpha_manual(guide = guide_legend(title = "Income (GNI/Capita)",order=2),
                     values=c(0.9,
                              0.75,
                              0.3,
                              0.1),
                     labels=c("High Income",
                              "Upper-Middle Income",
                              "Lower-Middle Income",
                              "Low Income")) +
  labs(title="CO2 Damage in 2017 (US$30 per ton of CO2)",
       subtitle="Most damage is caused by upper-middle and high income countries!",
       caption="Source: http://www.worldbank.org/") +
  theme(panel.grid.minor = element_line(size = 0.5),
        panel.grid.major = element_line(size = 0.5),
        panel.border=element_rect(fill=NA, size = 1),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(0.2,0,0,0, unit="cm"),
        text=element_text(family="Comfortaa"),
        plot.background=element_rect(fill = "#ffffff", color=NA),
        panel.background=element_rect(fill = "#ffffff", color=NA),
        axis.ticks.x=element_line()) +
  coord_fixed(ratio = 0.75, expand = 0)

ggsave("Plots/WorldBank.pdf", plot = plot, 
       device = cairo_pdf,
       width = 9, height = 6, dpi = 150,
       bg = "#ffffff")
