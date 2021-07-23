# load packages
library(tidyverse)
library(stringi)
library(gganimate)

# import data
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

# wrangle
over_time <- drought %>%
  filter(drought_lvl != 'None') %>%
  mutate(label_drought_lvl = recode(drought_lvl, 'D0' = 'Abnormally dry',
                                    'D1' = 'Moderate drought',
                                    'D2' = 'Severe drought',
                                    'D3' = 'Extreme drought',
                                    'D4' = 'Exceptional drought'))

over_time$label_drought_lvl <- factor(over_time$label_drought_lvl,
                               levels = c('Exceptional drought',
                                          'Extreme drought',
                                          'Severe drought',
                                          'Moderate drought',
                                          'Abnormally dry'))

# plot
plot <- over_time%>%
  filter(map_date > 20200000) %>%
  ggplot(mapping = aes(x = pop_pct, y = state_abb, fill = label_drought_lvl)) +
  geom_bar(stat = 'identity', position = position_stack(reverse = TRUE)) +
  theme(axis.text.y = element_text(size = 8),
        axis.ticks = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5, size = 10),
        legend.title = element_blank(),
        legend.position = 'top',
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = 'gray'),
        plot.title = element_text(face = 'bold'),
        plot.title.position = 'plot') +
  labs(title = "The percentage of the population facing severe, extreme, or exceptional \ndrought increased over the course of 2020 in many U.S. states.",
       subtitle = "Week of drought data beginning {closest_state}",
       x = "Percentage of state population",
       y = "State",
       caption = "SOURCE: U.S. Drought Monitor") +
  scale_fill_viridis_d() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(limits = rev) +
  transition_states(valid_start,
                    transition_length = 2,
                    state_length = 4) +
  ease_aes('sine-in-out')

animate(plot, width = 550, height = 500)

anim_save('~/drought.gif', animation = last_animation())