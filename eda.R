library(readr)
library(dplyr)
library(ggplot2)
library(scales)


df <- read_csv("/Users/enika/Desktop/stat8111/work/nsw_crash_clean.csv", show_col_types = FALSE)


p1 <- df %>%
  count(two_hour_intervals) %>%
  ggplot(aes(x = two_hour_intervals, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "When Do Crashes Happen?",
    x = "Two-hour interval",
    y = "Number of crashes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("/Users/enika/Desktop/stat8111/work/fig_crashes_by_time.png", p1, width = 8, height = 4.5, dpi = 300)


p2 <- df %>%
  count(natural_lighting) %>%
  ggplot(aes(x = natural_lighting, y = n)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Crashes by Lighting Condition",
    x = "Natural lighting",
    y = "Number of crashes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("/Users/enika/Desktop/stat8111/work/fig_lighting.png", p2, width = 7, height = 4.5, dpi = 300)


p3 <- df %>%
  mutate(
    severity = ifelse(degree_of_crash_detailed == "Non-casualty (towaway)", "Towaway", "Injury")
  ) %>%
  count(weather, severity) %>%
  group_by(weather) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = weather, y = pct, fill = severity)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Weather and Injury Share",
    x = "Weather",
    y = "Proportion of crashes",
    fill = "Crash type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("/Users/enika/Desktop/stat8111/work/fig_weather_injury.png", p3, width = 8, height = 4.5, dpi = 300)

cat("Charts saved in /work folder.")

