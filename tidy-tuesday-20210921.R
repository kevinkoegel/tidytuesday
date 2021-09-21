#load packages
library(tidyverse)
library(ggstream)

#import data
nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

#wrangle and plot
nominees %>%
  filter(year > 2011) %>%
  select(year,category,title,distributor) %>%
  unique() %>%
  mutate(Distributor = case_when(
    distributor == 'Netflix' ~ distributor,
    distributor == 'Hulu' ~ distributor,
    distributor == 'HBO Max' ~ distributor,
    distributor == 'Disney+' ~ distributor,
    grepl('Amazon', distributor) | distributor == 'Prime Video' ~ 'Amazon',
    grepl('Apple TV', distributor) ~ 'Apple TV',
    TRUE ~ 'Other'
  )) %>%
  group_by(year, Distributor) %>%
  count() %>%
  ggplot(aes(x = year, y = n, fill = Distributor)) +
  geom_stream(type = 'proportional') +
  scale_x_continuous(limits = c(2012,2021),
                     breaks = c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
                     expand = c(0,0)) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 9, vjust = 9),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = .5, size = 9, color = 'gray20'),
        panel.background = element_blank(),
        plot.title = element_text(face = 'bold', size = 12),
        plot.title.position = 'plot',
        plot.caption = element_text(size = 8, hjust = 1.5)) +
  labs(title = 'Over the past ten years, streaming distributors have \nreceived an increasing share of Emmy nominations.',
       y = 'Share \nof all Emmy \nnominations',
       caption = 'SOURCE: emmys.com') +
  scale_fill_manual(values = c('orange','black','blue','purple','springgreen3','red','gray89'))