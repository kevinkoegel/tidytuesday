#Load packages
library(tidyverse)
library(ggmap)
library(sf)
library(mapview)
library(ggrepel)

register_google() # Input your API key

#Import data
bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

#Wrangle
bird_baths_2015 <- bird_baths %>%
  filter(bird_count != 0,
         survey_year == 2015) %>%
  drop_na(c(bird_count, bioregions)) %>%
  group_by(survey_year, bioregions) %>%
  summarize(total_bird_count = sum(bird_count)) %>%
  ungroup()

bioregions <- bird_baths_2015 %>%
  select(bioregions) %>%
  distinct(bioregions) %>%
  mutate(region_num = c(1,2,3,4,5,6,7,8,9,10),
         place = case_when(
           region_num == 1 ~ 'Moranbah',
           region_num == 2 ~ 'Olary',
           region_num == 5 ~ 'Bairnsdale',
           region_num == 6 ~ 'Canberra',
           region_num == 8 ~ 'Sydney',
           TRUE ~ bioregions
         )) %>%
  select(bioregions, place)

bioregions_df <- as.data.frame(bioregions)

locations <- mutate_geocode(bioregions_df, place)

locations_sf <- st_as_sf(locations, coords = c('lon','lat'), crs = 4326)

mapview(locations_sf)

map <- get_googlemap(center = c(146, -30), zoom = 5,
                     color = "bw",
                     style = "feature:road|visibility:off&style=element:labels|visibility:off&style=feature:administrative|visibility:off")

locations_for_map <- locations %>%
  select(-place) %>%
  left_join(bird_baths_2015, by = 'bioregions')

ggmap(map) +
  geom_point(data = locations_for_map, aes (x = lon, y = lat, size = total_bird_count, color = total_bird_count)) +
  geom_text_repel(data = locations_for_map, aes(x = lon, y = lat, label = bioregions, color = total_bird_count), size = 4) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = 'bold')) +
  scale_size_continuous(range = c(1,5)) +
  scale_color_gradient(low = 'lightskyblue3', high = 'blue4') +
  labs(title = "In the 'Bathing Birds Study' summer data collection period (late Jan to late Feb 2015), \nthe most bird visits were observed at bird baths in South Eastern Queensland (n=438), \nSouth Eastern Highlands (n=432), and Sydney Basin (n=426).")