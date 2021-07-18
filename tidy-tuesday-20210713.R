# load packages
library(tidyverse)
library(ggtext)
library(ggridges)

# import data
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

# wrangle
scoobydoo$engagement <- as.numeric(scoobydoo$engagement)

scooby_avg_ratings <- scoobydoo %>%
  drop_na(c(imdb, engagement)) %>%
  filter(grepl('TV Series', format),
         season != 'Special') %>%
  select(index, series_name, season, network, format, date_aired, imdb, engagement) %>%
  mutate(years_aired = case_when(
    series_name == 'Scooby Doo, Where Are You!' ~ '1969-70',
    series_name == 'The New Scooby-Doo Movies' ~ '1972-73',
    series_name == 'The Scooby-Doo Show' ~ '1976-78',
    series_name == 'Laff-a-Lympics' ~ '1977-78',
    series_name == 'Scooby-Doo and Scrappy-Doo (first series)' ~ '1979-80',
    series_name == 'Scooby-Doo and Scrappy-Doo (second series)' ~ '1980-82',
    series_name == 'The New Scooby and Scrappy Doo Show' ~ '1983',
    series_name == 'The New Scooby-Doo Mysteries' ~ '1984',
    series_name == 'The 13 Ghosts of Scooby-Doo' ~ '1985',
    series_name == 'A Pup Named Scooby-Doo' ~ '1988-91',
    series_name == "What's New Scooby-Doo?" ~ '2002-06',
    series_name == 'Shaggy & Scooby-Doo Get a Clue!' ~ '2006-08',
    series_name == 'Scooby-Doo Mystery Incorporated' ~ '2010-13',
    series_name == 'Be Cool, Scooby-Doo!' ~ '2015-18',
    series_name == 'Scooby-Doo and Guess Who?' ~ '2019-21')) %>%
  mutate(series_label = paste(series_name, years_aired, sep = ", ")) %>%
  mutate(emphasis = case_when(
    series_name == 'Scooby Doo, Where Are You!' ~ 'A',
    series_name == 'Scooby-Doo Mystery Incorporated' ~ 'B',
    TRUE ~ 'C'))

scooby_avg_ratings$series_label <- factor(scooby_avg_ratings$series_label,
                                   levels = c('Scooby Doo, Where Are You!, 1969-70',
                                           'The New Scooby-Doo Movies, 1972-73',
                                           'The Scooby-Doo Show, 1976-78',
                                           'Laff-a-Lympics, 1977-78',
                                           'Scooby-Doo and Scrappy-Doo (first series), 1979-80',
                                           'Scooby-Doo and Scrappy-Doo (second series), 1980-82',
                                           'The New Scooby and Scrappy Doo Show, 1983',
                                           'The New Scooby-Doo Mysteries, 1984',
                                           'The 13 Ghosts of Scooby-Doo, 1985',
                                           'A Pup Named Scooby-Doo, 1988-91',
                                           "What's New Scooby-Doo?, 2002-06",
                                           'Shaggy & Scooby-Doo Get a Clue!, 2006-08',
                                           'Scooby-Doo Mystery Incorporated, 2010-13',
                                           'Be Cool, Scooby-Doo!, 2015-18',
                                           'Scooby-Doo and Guess Who?, 2019-21'))  

# plot
scooby_avg_ratings %>%
  ggplot(mapping = aes(x = engagement, y = series_label, fill = emphasis), color = emphasis) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(axis.text = element_text(size = 10, family = 'mono', color = 'white'),
        axis.ticks = element_blank(),
        axis.title.x = element_text(angle = 0, hjust = 0.5, size = 10, color = 'white', family = 'mono'),
        axis.title.y = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'black'),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = '#8f1954'),
        plot.title = element_text(face = 'bold', family = 'mono', color = '#eb9025'),
        plot.subtitle = element_text(face = 'bold', family = 'mono', color = '#4b945b'),
        plot.caption = element_text(size = 9, family = 'mono', color = 'white', hjust = -11.6),
        plot.title.position = 'plot') +
  scale_x_continuous(limits = c(0,600),
                     breaks = c(0,100,200,300,400,500,600),
                     expand = c(0,0)) +
  scale_y_discrete(limits = rev) +
  labs(title = "Zoinks! The original Scooby Doo TV series, 'Scooby Doo, Where Are You!', \nhas generated the most IMDB reviews per episode of any Scooby Doo series." ,
       subtitle = "Among 14 subsequent series, 'Scooby Doo Mystery Incorporated' produced the most engagement.",
       x = "Number of IMDB reviews per episode",
       caption = "SOURCE: https://www.kaggle.com/williamschooleman/scoobydoo-complete") +
  scale_fill_manual(values = c('#eb9025', '#4b945b', '#e9e0cc'))
