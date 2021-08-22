#load packages
library(tidyverse)
library(janitor)
library(formattable)
library(ggimage)
library(trekcolors)
library(trekfont)
library(showtext)
library(ggtext)

#import data
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

#wrangle and plot
char_images <- data.frame(char = c('Picard','Riker','Geordi','Worf','Troi','Data','Beverly'),
                          image = c("https://miro.medium.com/max/768/0*1EodT8AmjTMP6FG6.",
                                 "https://cdn.costumewall.com/wp-content/uploads/2018/08/william-riker.jpg",
                                 "https://treknews.net/wp-content/uploads/2016/05/geordi.jpg",
                                 "https://d.newsweek.com/en/full/1574814/star-trek-picard-season-3-worf.jpg?w=1600&h=1600&l=51&t=34&q=88&f=9776e93300c68c7078ed98f45b2c2ae4",
                                 "http://images6.fanpop.com/image/photos/35400000/Star-Trek-The-Next-Generation-marina-sirtis-35468566-694-530.jpg",
                                 "https://upload.wikimedia.org/wikipedia/en/0/09/DataTNG.jpg",
                                 "https://i.pinimg.com/originals/b9/1d/a8/b91da8478d6130000ca06be2ab5b8611.jpg"),
                                 size=10, replace = TRUE)

computer %>%
  select(name, char, nv_resp) %>%
  mutate(char = str_replace(computer$char, " \\s*\\([^\\)]+\\)", ""),
         char = case_when(
           grepl('Computer', char) ~ 'Computer',
           grepl('Picard', char) ~ 'Picard',
           TRUE ~ char
         )) %>%
  unique() %>%
  group_by(char) %>%
  tabyl(char, nv_resp) %>%
  mutate('total' = `FALSE`+`TRUE`,  # TRUE = when the computer completes the query without speaking a response
         'perc_no_response' = `TRUE`/total,
         'perc_label' = formattable::percent(perc_no_response, digits = 1)) %>%
  arrange(perc_label) %>%
  filter(char != 'Computer',
         total > 10) %>%
  left_join(char_images, by = 'char') %>%
  ggplot(aes(x = perc_no_response, y = reorder(char, desc(perc_no_response)), fill = char)) +
  geom_col(width = .3) +
  geom_image(aes(image = image), size = 0.125) +
  theme(axis.text.x = element_text(family = 'Federation', size = 8),
        axis.text.y = element_text(family = 'StarNext', size = 11, color = 'black'),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 8, vjust = -1 ,family = 'Federation'),
        axis.title.y = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = 'gray'),
        plot.background = element_blank(),
        plot.margin = margin(10,5,15,5),
        plot.title = element_text(face = 'bold', family = 'StarNext', size = 16),
        plot.subtitle = element_markdown(face = 'bold', family = 'Federation', size = 9),
        plot.caption = element_text(size = 8, vjust = -1.5),
        plot.title.position = 'plot') +
  scale_x_continuous(limits = c(0,.65),
                     breaks = c(0,.25,.5),
                     expand = c(0,0),
                     labels = scales::percent) +
  labs(title = "O Captain! My Captain!",
       subtitle = "<span style = 'color: darkred'>Captain Picard </span>had the lowest percentage <br>of computer queries unmet by a spoken response <br>of any Star Trek: The Next Generation character <br>with more than ten documented queries." ,
       x = "Percent of queries completed by computer without spoken response",
       caption = "SOURCE: Tea, Earl Grey, Hot: Designing Speech Interactions from the Imagined Ideal \nof Star Trek. Available at: http://www.speechinteraction.org/TNG/") +
  scale_fill_manual(values = c("#AD722C", "#AD722C", "#AD722C", '#5B1414', "#AD722C", "#AD722C", "#AD722C"))