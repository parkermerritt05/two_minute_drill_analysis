library(tidyverse)
library(nflverse)
library(ggplot2)
library(broom)

analyze_two_minute <- function(season) {
  message("Processing season: ", season)
  
  pbp <- load_pbp(season)
  
  opportunities <- pbp %>%
    filter(
      qtr == 4,
      play_type %in% c("run", "pass", "no_play"),
      score_differential <= 0,
      score_differential >= -8
    ) %>%
    group_by(game_id, drive) %>%
    arrange(desc(game_seconds_remaining)) %>%
    summarize(
      posteam = first(na.omit(posteam)),
      start_time = max(game_seconds_remaining),
      end_time = min(game_seconds_remaining),
      start_score_diff = first(na.omit(score_differential)),
      posteam_timeouts_remaining = first(na.omit(posteam_timeouts_remaining)),
      .groups = "drop"
    ) %>%
    filter(!is.na(posteam),
           start_time <= 150,
           start_time >= 13)
  
  drive_outcomes <- pbp %>%
    group_by(game_id, posteam, drive) %>%
    summarize(
      end_score_diff = last(score_differential),
      made_fg = any(str_detect(desc, "GOOD") & play_type == "field_goal", na.rm = TRUE),
      .groups = "drop"
    )
  
  opportunities <- opportunities %>%
    left_join(drive_outcomes, by = c("game_id", "posteam", "drive")) %>%
    mutate(
      success = (end_score_diff >= 0 & end_score_diff > start_score_diff) | made_fg,
      season = season
    )
  
  opportunities
}

seasons <- 2000:2024
opportunities_all <- map_dfr(seasons, analyze_two_minute)

results_all <- opportunities_all %>%
  group_by(posteam, season) %>%
  summarize(
    opportunities = n(),
    successes = sum(success, na.rm = TRUE),
    success_rate = round(successes / opportunities, 3),
    .groups = "drop"
  )

timeouts_summary_all <- opportunities_all %>%
  filter(!is.na(posteam_timeouts_remaining)) %>%
  group_by(posteam_timeouts_remaining, season) %>%
  summarize(
    opportunities = n(),
    successes = sum(success, na.rm = TRUE),
    success_rate = round(successes / opportunities, 3),
    .groups = "drop"
  )

timeouts_summary_league <- opportunities_all %>%
  filter(!is.na(posteam_timeouts_remaining)) %>%
  group_by(posteam_timeouts_remaining) %>%
  summarize(
    opportunities = n(),
    avg_success_rate = mean(success, na.rm = TRUE),
    avg_start_time = mean(start_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg_success_rate = round(avg_success_rate, 3),
    avg_start_time = round(avg_start_time, 1)
  )

print(timeouts_summary_league)

model_data <- opportunities_all %>%
  filter(!is.na(success),
         !is.na(posteam_timeouts_remaining),
         posteam_timeouts_remaining == 0 | posteam_timeouts_remaining == 1,
         !is.na(start_time)) %>%
  mutate(
    success = as.integer(success),            # Convert logical -> numeric (1/0)
    start_time_std = scale(start_time)        # Standardize time to make coefficients comparable
  )

model <- glm(success ~ posteam_timeouts_remaining + start_time_std,
             data = model_data,
             family = binomial(link = "logit"))

summary(model)

effects <- tidy(model) %>%
  mutate(
    odds_ratio = exp(estimate),
    pct_change_in_odds = (odds_ratio - 1) * 100
  )

print(effects)

ggplot(timeouts_summary_all, aes(x = factor(posteam_timeouts_remaining),
                                 y = success_rate,
                                 fill = factor(season))) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(name = "Season") +
  labs(
    title = "Two-Minute Drill Success Rate by Timeouts Remaining (2000–2024)",
    x = "Timeouts Remaining at Start of Drive",
    y = "Success Rate"
  ) +
  theme_minimal(base_size = 14)

ggsave("two_minute_drill_timeouts_2000_2024.png", width = 10)