#load packages
library(tidyverse)
library(R.utils)
library(networkD3)
library(htmlwidgets)
library(htmltools)
library(webshot)
library(flextable)

#import data
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

#wrangle
lemurs_clean <- lemurs %>%
  mutate(across(c(name,dam_name,sire_name), tolower),
         dam_name = str_to_title(dam_name),
         sire_name = str_to_title(sire_name)) %>%
  filter(current_resident == 'Y',
         taxon == 'MMUR') %>%
  select(c(name,dam_name,sire_name)) %>%
  unique() %>%
  drop_na(c(dam_name | sire_name)) %>%
  mutate(number = 1) %>%
  select(-name) %>%
  group_by(dam_name, sire_name) %>%
  summarize(pair_total = sum(number)) %>%
  ungroup()

#flextable of dom-sire pairings
lemur_table <- lemurs_clean %>%
  select(-starts_with('ID')) %>%
  arrange(dam_name) %>%
  rename('Female Parent' = dam_name,
         'Male Parent' = sire_name,
         'Number of Spawn' = pair_total) %>%
  flextable() %>%
  autofit() %>%
  theme_vanilla() %>%
  bg(bg = 'white', part = 'all')

lemur_table

flextable::save_as_image(lemur_table, "lemur_table.png")

#create and export sankey plot
nodes <- data.frame(name = c(as.character(lemurs_clean$dam_name), 
                             as.character(lemurs_clean$sire_name)) %>% 
                      unique())

lemurs_clean$IDdam_name <- match(lemurs_clean$dam_name, nodes$name)-1 
lemurs_clean$IDsire_name <- match(lemurs_clean$sire_name, nodes$name)-1

lemur_sankey <- sankeyNetwork(Links = lemurs_clean, 
                              Nodes = nodes,
                              Source = "IDdam_name",
                              Target = "IDsire_name",
                              Value = "pair_total", 
                              NodeID = "name",
                              fontSize = 16,
                              width = 800,
                              height = 600)

lemur_sankey <- htmlwidgets::prependContent(lemur_sankey, htmltools::tags$h3("Mircocebus murinus female (left) and male (right) parent pairings"))
lemur_sankey <- htmlwidgets::prependContent(lemur_sankey, htmltools::tags$h3("Gray mouse lemurs currently residing at Duke Lemur Center"))
lemur_sankey <- htmlwidgets::appendContent(lemur_sankey, htmltools::tags$p("SOURCE: Duke Lemur Center. Available at: https://lemur.duke.edu/"))

lemur_sankey

saveWidget(lemur_sankey, file="lemur_sankey.html")

webshot("lemur_sankey.html" , "lemur_sankey.png")