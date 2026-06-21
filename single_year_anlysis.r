library(tidyverse)
library(nflverse)
library(ggplot2)

pbp <- load_pbp(2024)

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
    season = first(season),
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
    success = (end_score_diff >= 0 & end_score_diff > start_score_diff) | made_fg
  )

results <- opportunities %>%
  group_by(posteam) %>%
  summarize(
    opportunities = n(),
    successes = sum(success, na.rm = TRUE),
    success_rate = round(successes / opportunities, 3),
    .groups = "drop"
  ) %>%
  arrange(desc(success_rate))

print(results, n = 32)

timeouts_summary <- opportunities %>%
  filter(!is.na(posteam_timeouts_remaining)) %>%
  group_by(posteam_timeouts_remaining) %>%
  summarize(
    opportunities = n(),
    successes = sum(success, na.rm = TRUE),
    success_rate = round(successes / opportunities, 3),
    .groups = "drop"
  )

print(timeouts_summary)

model_data <- opportunities %>%
  filter(!is.na(success),
         !is.na(posteam_timeouts_remaining),
         !is.na(start_time)) %>%
  mutate(
    success = as.integer(success), 
    start_time_std = scale(start_time)  
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

ggplot(timeouts_summary, aes(x = factor(posteam_timeouts_remaining),
                             y = success_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(success_rate, accuracy = 0.1)),
            vjust = -0.5, size = 4.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Two-Minute Drill Success Rate by Timeouts Remaining (2023)",
    x = "Timeouts Remaining at Start of Drive",
    y = "Success Rate"
  ) +
  theme_minimal(base_size = 14)