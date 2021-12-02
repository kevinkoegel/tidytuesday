#load packages
library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)

#import data
matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

#wrangle
team1 <- matches %>%
  select(-c(team2,score_team2,wickets_team2,team2_home_away)) %>%
  rename("team" = team1,
         "score" = score_team1,
         "wickets" = wickets_team,
         "home_away" = team1_away_or_home)

matchesbyteam <- matches %>%
  select(-c(team1,score_team1,wickets_team,team1_away_or_home)) %>%
  rename("team" = team2,
         "score" = score_team2,
         "wickets" = wickets_team2,
         "home_away" = team2_home_away) %>%
  bind_rows(team1) %>%
  mutate(win_or_loss = case_when(
    team == winner ~ 'win',
    TRUE ~ 'loss'
  ))

counts <- matchesbyteam %>%
  group_by(team) %>%
  count()

home_counts <- matchesbyteam %>%
  filter(home_away == 'home') %>%
  group_by(team) %>%
  count()

win_perc <- counts %>%
  left_join(matchesbyteam, by = 'team') %>%
  rename('total_matches' = n) %>%
  group_by(team, total_matches, win_or_loss) %>%
  count(win_or_loss) %>%
  pivot_wider(names_from = win_or_loss, values_from = n) %>%
  replace(., is.na(.), 0) %>%
  mutate(overall_win_percentage = win/total_matches,
         overall_win_percentage_formatted = scales::percent(win/total_matches, accuracy=1)) %>%
  ungroup() %>%
  select(team, overall_win_percentage)

home_win_perc <- home_counts %>%
  left_join(matchesbyteam, by = 'team') %>%
  rename('total_matches' = n) %>%
  filter(home_away == 'home') %>%
  group_by(team, total_matches, win_or_loss) %>%
  count(win_or_loss) %>%
  pivot_wider(names_from = win_or_loss, values_from = n) %>%
  replace(., is.na(.), 0) %>%
  mutate(home_win_percentage = win/total_matches) %>%
  ungroup() %>%
  select(team, home_win_percentage)

summary <- counts %>%
  left_join(matchesbyteam, by = 'team') %>%
  left_join(home_win_perc, by = 'team') %>%
  rename('total_matches' = n) %>%
  group_by(team, total_matches, home_win_percentage, venue, home_away, win_or_loss) %>%
  count(win_or_loss) %>%
  pivot_wider(names_from = win_or_loss, values_from = n) %>%
  replace(., is.na(.), 0) %>%
  mutate(number_of_matches = win + loss,
         percent_home_away = number_of_matches/total_matches,
         win_percentage = win/number_of_matches)

venues <- summary %>%
  filter(venue == 'R Premadasa Stadium, Colombo' |
           venue == 'Arnos Vale Ground, Kingstown, St Vincent',
         home_away == 'home')

showtext_auto()

#plot
summary %>%
  filter(total_matches > 50,
         home_away == 'home') %>%
  ggplot(aes(x = win_percentage,
             y = reorder(team, home_win_percentage))) +
  geom_point(aes(size = win, color = team, alpha = win), position = 'jitter') +
  geom_text_repel(data = venues, aes(label = venue), 
                  size = 3, min.segment.length = 0, nudge_x = -.2) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = 'gray93'),
        panel.background = element_blank(),
        plot.title = element_text(face = 'bold', family = 'Arial', size = 12),
        plot.title.position = 'plot',
        plot.subtitle = element_markdown(size = 11),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.caption = element_text(size = 8, hjust = 1.1)) +
  coord_cartesian(clip = 'off') +
  scale_size_continuous(range = c(2,8)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = 'none', alpha = 'none') +
  labs(title = 'Which nations had the best "home oval advantage" in ODI cricket, 1996-2005?',
       subtitle = '<span style = "color: navyblue"><b>Sri Lanka</b></span> fared the best at home grounds (among nations competing in at least 50 matches) with an <span style = "color: navyblue"><b>81% win <br>percentage</b></span> in 81 ODI matches as the home side, including 33 wins at R Premadasa Stadium, Colombo. <br><span style = "color: maroon"><b>West Indies </b></span>won 92% (!) of matches held at Arnos Vale Ground in Kingstown, St. Vincent. <br><span style = "color: darkgoldenrod1"><b>Australia (69%)</b></span> and <span style = "color: darkgreen"><b>South Africa (68%)</b></span> had the highest overall win-rates (home and away) over the period.',
       x = 'Win percentage per home venue \n(nations ordered by overall home win percentage)',
       size = 'Number \nof wins',
       caption = 'SOURCE: ESPN Cricinfo') +
  scale_color_manual(values = c('darkgoldenrod1','seagreen','navyblue','dodgerblue1','darkgreen','black','darkgreen','darkgreen','navyblue','maroon','red2'))

