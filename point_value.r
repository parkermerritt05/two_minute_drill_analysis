library(tidyverse)
library(nflverse)
library(broom)

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

timeout_value_model <- lm(
  point_change ~ posteam_timeouts_remaining + start_time + season,
  data = opportunities
)
summary(timeout_value_model)

timeout_value_summary <- broom::tidy(timeout_value_model, conf.int = TRUE) %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high)

print(timeout_value_summary)

plot <- ggplot(opportunities, aes(x = posteam_timeouts_remaining, y = point_change)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Expected Points Gained vs. Timeouts Remaining (2000–2024)",
    x = "Timeouts Remaining at Start of Drive",
    y = "Average Points Gained by End of Game"
  ) +
  theme_minimal()

print(plot)
