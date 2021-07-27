# load packages
library(tidyverse)
library(showtext)

# load dataset
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# wrangle
last_year <- olympics %>%
  select(sport,year) %>%
  arrange(sport, year) %>%
  unique() %>%
  group_by(sport) %>%
  arrange(desc(year)) %>%
  slice(1L) %>%
  filter(year != 2014,
         year != 2016)

# plot
olympics %>%
  select(sport,year) %>%
  arrange(sport, year) %>%
  unique() %>%
  group_by(sport) %>%
  slice(1L)%>%
  right_join(last_year, by = 'sport') %>%
  pivot_longer(!sport, names_to = 'first_or_last', values_to = 'year') %>%
  filter(sport != 'Baseball',
         sport != 'Softball') %>%
  mutate(sport = case_when(
    sport == 'Rugby' ~ 'Rugby (15s)',
    TRUE ~ `sport`
  )) %>%
  ggplot(aes(x = year, y = sport)) +
  geom_point(size = 4, shape = 15, color = 'navy') +
  scale_y_discrete(limits = rev) +
  geom_line(mapping = aes(x = year, y = sport, group = sport), color = 'navy',
            size = 1.5) +
  theme(axis.text = element_text(family = 'space grotesk', size = 11),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = 'gray'),
        plot.title = element_text(face = 'bold', family = 'alegreya', size = 28),
        plot.title.position = 'plot',
        plot.subtitle = element_text(family = 'space grotesk', size = 12),
        plot.caption = element_text(color = 'grey25'),
        plot.margin = margin(10,10,10,10)) +
  labs(title = "From Motorboating to Tug-of-War: \nOlympic Events of Yesteryear",
       subtitle = "Athletes have not competed in the events below at an Olympic Games since 1948.",
       caption = "SOURCE: www.sports-reference.com via Kaggle") +
  scale_x_continuous(limits = c(1899,1950),
                     breaks = c(1900,1904,1908,1912,1920,1924,1936,1948),
                     expand = c(0,0))