# Load packages
library(tidyverse)
library(ggtext)
library(ggpubr)
library(tidytext)
library(wordcloud2)

# Load data
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

animal_rescues$month_of_call <- str_sub(animal_rescues$date_time_of_call, 4, -12)  # Separate months

service_types <- animal_rescues %>%
  group_by(special_service_type_category, cal_year, month_of_call) %>%
  count() %>%
  group_by(special_service_type_category, cal_year) %>%
  summarize(mean(n)) %>%
  rename('Average \nnumber \nof \nrescues \nper \nmonth' = 'mean(n)') %>%
  ggplot(mapping = aes(x = cal_year, y = `Average \nnumber \nof \nrescues \nper \nmonth`, group = special_service_type_category, color = special_service_type_category)) +
  geom_line(size = 1.5) +
  scale_x_continuous(limits = c(2009, 2021),
                   breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) +
  scale_y_continuous(limits = c(0,35),
                     breaks = c(0,5,10,15,20,25,30,35)) +
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'gray'),
        panel.grid.minor.x = element_blank(),
        plot.title = element_markdown(face = "bold")) +
  labs(title = "Since 2012, calls to the London Fire Brigade for <span style = 'color: firebrick2'>other animal assistance</span> <br>account for more rescues per month than any other special service category. <br>What do these 'other' forms of assistance entail?") +
  scale_color_manual(values = c('snow3', 'snow4','snow2','firebrick2'))

other_services <- animal_rescues %>%
  filter(special_service_type_category == 'Other animal assistance') %>%
  mutate(animal_group_parent = recode(animal_group_parent, 'cat' = 'Cat'),
         other_animal_detail = case_when(
    grepl('domestic', special_service_type) ~ 'Domestic',
    grepl('livestock', special_service_type) ~ 'Livestock',
    grepl('wild', special_service_type) ~ 'Wild'
   )) %>%
  group_by(other_animal_detail, animal_group_parent) %>%
  count() %>%
  mutate(animal_group_parent = case_when(
    grepl('Unknown', animal_group_parent) ~ 'Other \nanimal',
    n > 80 ~ animal_group_parent,
    TRUE ~ 'Other \nanimal')) %>%
  summarize(sum(n)) %>%
  rename('Total number of rescues, 2009-2021' = 'sum(n)') %>%
  ggplot(mapping = aes(x = `Total number of rescues, 2009-2021`,
                       y = reorder(other_animal_detail, desc(other_animal_detail)),
                       fill = animal_group_parent)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(limits = c(0,2100),
                     breaks = c(0,500,1000,1500,2000)) +
  theme(axis.text.y = element_text(hjust = 1),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = 'gray'),
        plot.title = element_markdown(face = "bold")) +
  scale_fill_manual(values = c('dodgerblue2','firebrick4','sienna4','firebrick2','sienna3','orange','snow3')) +
  labs(title = "From 2009 through early 2021, calls to the London Fire Brigade for 'other' animal assistance <br>most often involved domestic animals - primarily cats and dogs.",
       subtitle = "'Other' calls for wild animals, on the other hand, most frequently involved birds, foxes, or deer.") +
  geom_text(aes(label = animal_group_parent),
            color = 'white',
            size = 3,
            position = position_stack(vjust = 0.5, reverse = TRUE))

ggarrange(service_types, " ", other_services, nrow = 3, heights = c(4.25,0.25,3))

other_descriptions <- animal_rescues %>% 
  filter(special_service_type_category == 'Other animal assistance',
         final_description != 'Redacted') %>%
  select(final_description)
  
other_descriptions$final_description <- word(other_descriptions$final_description, 2, -1)

other_descriptions %>%
  unnest_tokens(word, final_description) %>%
  group_by(word) %>%
  count() %>%
  filter(n > 40,
         !word %in% c('in','of','with','and','on','to','the','a','by','two','at')) %>%
  wordcloud2(size = .7, color = 'random-dark')