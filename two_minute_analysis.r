library(tidyverse)
library(nflverse)

two_minute_summary <- function(year) {
  pbp <- load_pbp(year)
  
drive_starts <- pbp %>%
  filter(qtr == 4, !is.na(posteam)) %>%
  group_by(game_id, posteam, drive) %>%
  arrange(desc(game_seconds_remaining)) %>%  
  slice(1) %>%                               
  ungroup()

opportunities <- drive_starts %>%
  filter(game_seconds_remaining <= 150,
         game_seconds_remaining >= 30,
         score_differential < 0,
         score_differential >= -8) %>%
  select(game_id, posteam, drive, game_seconds_remaining, score_differential)
  
  final_scores <- pbp %>%
    group_by(game_id) %>%
    summarize(
      home_team = first(home_team),
      away_team = first(away_team),
      final_home_score = last(total_home_score),
      final_away_score = last(total_away_score),
      .groups = "drop"
    )
  
  opportunities <- opportunities %>%
    left_join(final_scores, by = "game_id") %>%
    mutate(
      final_diff = case_when(
        posteam == home_team ~ final_home_score - final_away_score,
        posteam == away_team ~ final_away_score - final_home_score,
        TRUE ~ NA_real_
      ),
      success = final_diff >= 0
    )
  
  league_summary <- opportunities %>%
    summarize(
      opportunities = n(),
      successes = sum(success, na.rm = TRUE),
      success_rate = round(successes / opportunities, 3)
    )
  
  league_summary %>%
    mutate(season = year) %>%
    select(season, everything())  # season as first column
}

years <- 1999:2024

all_years_summary <- map_dfr(years, two_minute_summary)

print(all_years_summary, n = 26)
write_csv(all_years_summary, "two_minute_drill_summary.csv")