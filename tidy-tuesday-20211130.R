#load packages
library(tidyverse)
library(ggrepel)

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

#plot
summary %>%
  filter(total_matches > 50,
         home_away == 'home') %>%
  ggplot(aes(x = win_percentage,
             y = reorder(team, home_win_percentage))) +
  geom_point(aes(size = win, color = team), alpha = 3/4, position = 'jitter') +
  geom_text_repel(data = venues, aes(label = venue), 
                  size = 2.5, min.segment.length = 0, nudge_x = -.2) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_line(color = 'gray93'),
        panel.background = element_blank(),
        plot.title = element_text(face = 'bold', size = 12),
        plot.title.position = 'plot',
        plot.subtitle = element_text(size = 10),
        legend.title = element_text(size = 9),
        plot.caption = element_text(size = 8, hjust = 1.15)) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(labels = scales::percent_format(scale = 100)) +
  guides(color = 'none') +
  labs(title = 'Which nations had the best "home oval advantage" in ODI cricket, 1996-2005?',
       subtitle = 'Sri Lanka fared the best at home (among nations competing in at least 50 matches) with an 81% win \npercentage in 81 ODI matches as the home side, including 33 wins at R Premadasa Stadium, Colombo. \nWest Indies had a 92% (!) winning percentage at Arnos Vale Ground, Kingstown, St Vincent. \nAustralia (69%) and South Africa (68%) had the highest overall winning percentages over the period.',
       x = 'Win percentage per home venue',
       size = 'Number \nof wins',
       caption = 'SOURCE: ESPN Cricinfo') +
  scale_color_manual(values = c('yellow1','seagreen','gray','dodgerblue1','darkgreen','black','darkgreen','darkgreen','navyblue','maroon','red2'))
