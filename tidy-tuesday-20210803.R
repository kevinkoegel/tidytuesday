# load packages
library(tidyverse)
library(gganimate)

# import data
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

# wrangle
athletes$year <- as.character(athletes$year)
  
hoops <- function(mw) {
  
}  

athletes %>%
  filter(type == 'Basketball') %>%
  mutate(gender = case_when(
    event == 'Women' ~ 'Women',
    event == 'Men' ~ 'Men',
    TRUE ~ gender)
    ) %>% 
  group_by(abb, year, gender) %>%
  select(gender, abb, year) %>%
  unique() %>%
  count() %>%
  pivot_wider(id_cols = c(abb,gender), names_from = year, values_from = n) %>%
  ungroup() %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  pivot_longer(cols = -c(abb,gender), names_to = 'year', values_to = 'medals') %>%
  filter(gender == 'Men') %>%
  arrange(abb,year) %>%
  group_by(abb) %>%
  mutate(medal_count = cumsum(medals)) %>%
  ggplot(aes(x = medal_count, y = abb, fill = abb, label = medal_count)) +
  geom_col() +
  geom_text() +
  transition_states(year,
                    transition_length = 2,
                    state_length = 4) +
  ease_aes('sine-in-out')
