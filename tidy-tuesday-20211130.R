#load packages
library(tidyverse)
library(tidyr)

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

mostfrequent <- matchesbyteam %>%
  group_by(team) %>%
  count() %>%
  filter(n > 50) 

win_perc <- mostfrequent %>%
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

summary <- mostfrequent %>%
  left_join(matchesbyteam, by = 'team') %>%
  left_join(win_perc, by = 'team') %>%
  rename('total_matches' = n) %>%
  group_by(team, total_matches, overall_win_percentage, home_away, win_or_loss) %>%
  count(win_or_loss) %>%
  pivot_wider(names_from = win_or_loss, values_from = n) %>%
  replace(., is.na(.), 0) %>%
  mutate(number_of_matches = win + loss,
         percent_home_away = scales::percent(number_of_matches/total_matches, accuracy=1),
         win_percentage = scales::percent(win/number_of_matches, accuracy=1),
         loss = loss*-1) %>%
  pivot_longer(cols = c(win, loss), names_to = 'win_or_loss') %>%
  unite('team_home_away', c(team,home_away), remove = FALSE)

home_away_win_perc <- summary_all %>%
  ungroup() %>%
  select(team, home_away, win_percentage)

home_only <- summary %>%
  filter(home_away == 'home')

win_only <- summary %>%
  filter(win_or_loss == 'win')

#plot
summary %>%
  ggplot(aes(x = value,
             y = reorder(team_home_away, overall_win_percentage))) +
  geom_col(fill = 'lightgray') +
  geom_vline(xintercept = 0, color = 'gray50') +
  geom_text(data = win_only, aes(label = win_percentage)) 
  
