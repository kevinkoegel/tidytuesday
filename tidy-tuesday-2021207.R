#load packages
library(stringr)
library(tidyverse)
library(countrycode)
library(spData)
library(maps)
library(gganimate)

#read in tidy tuesday data
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# get rid of notes in parentheses
# remove other weird notes like "introduced to canada - i just want countries! 
# pivot longer countries separated by "," in lists
spiders <- spiders %>% 
  mutate(distribution = sub("\\(.*)", "", distribution)) %>% 
  mutate(distribution = str_remove(distribution, "Introduced to")) %>% 
  separate_rows(distribution, sep = ", ") %>% 
  mutate(distribution = str_remove(distribution, '"'))

# standardize country names to country name in english
spiders$continent <- countrycode(spiders$distribution, origin = "country.name", destination = "continent")
spider_species_country <- spiders %>% 
  group_by(year, distribution) %>% 
  summarise(unique_species = n_distinct(speciesId)) %>%
  arrange(distribution, year) %>%
  group_by(distribution) %>%
  mutate(cumulative = cumsum(unique_species))

world <- map_data("world")
spiders_world <- world %>%  
  left_join(spider_species_country, by = c("region" = "distribution")) %>% 
  filter(region != "Antarctica")

ggplot() + 
  geom_map(
    data = spiders_world,
    map = spiders_world,
    aes(long, lat, map_id = region, fill = unique_species)) +
  theme_void() +
  theme(plot.title = element_text("serif", size = 20, hjust = 0.5),
        plot.subtitle = element_text("serif", size = 10, hjust = 0.5),
        legend.title = element_text("serif"),
        legend.text = element_text("serif"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect("ivory2"),
        plot.background = element_rect("ivory2"),
        legend.position = c(0.15, 0.25))+
  labs(title = "Unique Spider Species Per Country",
       subtitle = "Data from the World Spider Database",
       fill = "Unique Spider Species") +
  xlab("Longitude") +
  ylab("Latitude") + 
  scale_fill_distiller(palette = "RdPu") +
  transition_states(year,
                    transition_length = 2,
                    state_length = 4)
