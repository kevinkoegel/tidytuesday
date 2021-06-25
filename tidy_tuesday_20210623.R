library(tidyverse) # Load packages
library(tvthemes)
library(ggtext)
library(showtext)

font_add_google(name = 'Titillium Web', family = 'Titillium')

showtext_auto()

parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv') # Load dataset

parks$spend_per_resident_data <- str_remove(parks$spend_per_resident_data, "[$]")
parks$spend_per_resident_data <- as.numeric(parks$spend_per_resident_data)

table <- parks %>%
  mutate(top_10 = case_when(
    rank <= 10 ~ 'Top_10',
    TRUE ~ 'No')) %>%
  group_by(year, top_10) %>%
  summarize(across(spend_per_resident_data, mean)) %>%
  pivot_wider(id_cols = year, names_from = top_10, values_from = spend_per_resident_data) %>%
  mutate(difference = Top_10 - No)

parks %>%
  group_by(year) %>%
  mutate(top_10 = case_when(
    rank <= 10 ~ 'Top_10',
    TRUE ~ 'No')) %>%
  select(year, city, spend_per_resident_data, top_10) %>%
  ggplot(mapping = aes(x=year, y=spend_per_resident_data, color=top_10)) +
  geom_point(size=2, position='jitter') +
  scale_x_continuous(limits=c(2011.5,2020.5),
                     breaks=c(2012,2013,2014,2015,2016,2017,2018,2019,2020)) +
  scale_y_continuous(limits=c(0,400),
                     breaks=c(0,100,200,300,400)) +
  labs(title = "Cities ranked <span style = 'color: gold'>in the ParkScore index top 10</span> <br>spent more per resident than cities ranked <br><span style = 'color: steelblue1'>outside the top 10.</span>",
       subtitle = 'The gap has widened over time, from an average of $93 more \nper resident in 2012 to $157 more in 2020.',
       x = "", y = "Spending \nper \nresident \n(dollars)") +
  theme_parksAndRecLight() +
  theme(axis.line = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.ticks = element_blank(),
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.5),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(family = 'Titillium', face='bold'),
        plot.title.position = "plot") +
  scale_color_manual(values = c('steelblue1', 'gold'))