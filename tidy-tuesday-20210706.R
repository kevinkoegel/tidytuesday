# Load packages
library(tidyverse)

# Load dataset
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

# Wrangle data
holidays_clean <- holidays %>%
  mutate(fifty_years = case_when(
    year >= 1800 & year <1850 ~ '1800-1849',
    year >= 1850 & year <1900 ~ '1850-1900',
    year >= 1900 & year <1950 ~ '1901-1949',
    year >= 1950 ~ '1950-present')) %>%
  mutate(ind_from_clean = case_when(
      grepl('Soviet Union', independence_from) ~ 'Soviet Union',
      grepl('Spanish Empire', independence_from) ~ 'Spanish Empire or Spain',
      independence_from == 'Spain' ~ 'Spanish Empire or Spain',
      independence_from == 'France' ~ 'France',
      independence_from == 'Ottoman Empire' ~ 'Ottoman Empire',
      independence_from == 'Portugal' ~ 'Portugal',
      independence_from == 'Russian Soviet Federative Socialist Republic' ~ 'Russian Soviet Federative \nSocialist Republic',
      independence_from == 'United Kingdom' ~ 'United Kingdom',
      TRUE ~ 'Other (or NA)')) 

# Set factor level for 'independence_from'
holidays_clean$ind_from_clean <- factor(holidays_clean$ind_from_clean, 
                                     levels = c('Spanish Empire or Spain',
                                                'Ottoman Empire',
                                                'Russian Soviet Federative \nSocialist Republic',
                                                'France',
                                                'Portugal',
                                                'United Kingdom',
                                                'Soviet Union',
                                                'Other (or NA)'))

# Plot
holidays_clean %>% 
  select(country, year, fifty_years, ind_from_clean) %>%
  filter(year >= 1800) %>%
  ggplot(aes(x = year, y = ind_from_clean)) +
  geom_jitter(aes(color = ind_from_clean)) +
  scale_x_continuous(limits = c(1800,2030),
                     breaks = c(1800,1850,1900,1950,2000)) +
  scale_y_discrete(limits = rev) +
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 10),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = 'gray'),
        plot.title = element_text(face = 'bold'),
        plot.title.position = 'plot') +
  labs(title = "Countries that gained independence from another entity (country or empire) \noften did so during the same period as one or more others ruled by the same entity.",
       subtitle = "Each dot below represents a country that gained independence from another entity from 1800 to present.",
       y = "Entity \nfrom which \nindependence \nwas gained",
       caption = "SOURCE: Wikipedia. List of national independence days.") +
  scale_color_brewer(palette = 'Spectral', direction = 1)