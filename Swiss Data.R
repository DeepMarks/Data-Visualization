
rm(list = ls())
graphics.off()

# Packages ----------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Data --------------------------------------------------------------------

y = 1800

Theatre = read_tsv("Theatre.csv")

Theatre = mutate(Theatre, year = str_sub(Premierendatum, 7, 10))

Authors = data_frame(Theatre$Autor, Theatre$year)

Authors = Authors %>%
  rename(auth = 'Theatre$Autor') %>%
  rename(year = 'Theatre$year') %>%
  mutate(goet = str_sub(auth, 1, 6)) %>%
  mutate(schi = str_sub(auth, 1, 8))

goet = filter(Authors, str_detect(goet, "Goethe"))

goet = goet %>%
  mutate(year = as.integer(year)) %>%
  subset(year > y)

goet_plot = data.frame(goet$year)
goet_plot$auth = 'goet'
names(goet_plot)[1] = paste("year")
names(goet_plot)[2] = paste("auth")

schi = filter(Authors, str_detect(schi, "Schiller"))

schi = schi %>%
  mutate(year = as.integer(year)) %>%
  subset(year > y)

schi_plot = data.frame(schi$year)
schi_plot$auth = 'schi'
names(schi_plot)[1] = paste("year")
names(schi_plot)[2] = paste("auth")

plot = rbind(goet_plot, schi_plot)

jonas = data.frame(rep(seq(1834, 2015, 6),each = 2))
jonas$auth = NA
jonas$freq = NA
colnames(jonas)[1] = c("year")

for(i in seq(1, nrow(jonas), 2)){
  jonas[i,2] = "goet"
  jonas[i,3] = -nrow(filter(plot, plot$year >= jonas$year[i] & plot$year <= jonas$year[i] + 5 & plot$auth == "goet"))
  jonas[i+1,2] = "schi"
  jonas[i+1,3] = nrow(filter(plot, plot$year >= jonas$year[i] & plot$year <= jonas$year[i] + 5 & plot$auth == "schi"))
}

# Plot --------------------------------------------------------------------

theme_set(theme_minimal())

plot = ggplot(data = jonas, aes(x = year, y = freq, fill = auth)) +
  geom_bar(data = subset(jonas, auth == "goet"), stat = "identity") +
  geom_bar(data = subset(jonas, auth == "schi"), stat = "identity") +
  coord_flip() +
  labs(x = "Year of the premiere (bins of six years)",
       y = "Number of productions in Swiss theatres") +
  scale_fill_brewer(palette = "Dark2", name = "Authors", labels = c("Goethe", "Schiller")) +
  theme(text = element_text(size = 16, family = "Garamond", face = "bold"))

ggsave("Swiss.pdf", plot = plot, 
       device = cairo_pdf,
       width = 12, height = 8, dpi = 150,
       bg = "#ffffff")
