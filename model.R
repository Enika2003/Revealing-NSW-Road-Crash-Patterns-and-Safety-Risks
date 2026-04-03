library(readr)
library(dplyr)
library(broom)
library(ggplot2)


mdata <- read_csv("/Users/enika/Desktop/stat8111/work/nsw_crash_model.csv", show_col_types = FALSE)

mdata <- mdata |>
  mutate(
    natural_lighting = relevel(factor(natural_lighting), "Daylight"),
    street_lighting  = relevel(factor(street_lighting),  "On"),
    weather          = relevel(factor(weather),          "Fine"),
    urbanisation     = relevel(factor(urbanisation),     "Country urban"),
    speed_limit      = relevel(factor(speed_limit),      "50 km/h")
  )

m1 <- glm(injury ~ speed_limit + natural_lighting + street_lighting + weather + urbanisation,
          data = mdata, family = binomial())

or_tab <- tidy(m1, conf.int = TRUE, exponentiate = TRUE) |>
  select(term, estimate, conf.low, conf.high, p.value)

write_csv(or_tab, "/Users/enika/Desktop/stat8111/work/odds_ratios.csv")
or_plot <- or_tab |>
  filter(term != "(Intercept)") |>
  mutate(term = reorder(term, estimate)) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point(color = "darkred", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  coord_flip() +
  labs(
    title = "Odds of Injury (vs Towaway)",
    x = "Predictor",
    y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal()

ggsave("/Users/enika/Desktop/stat8111/work/fig_odds_ratios.png", or_plot, width = 7, height = 5, dpi = 300)

cat("Odds Ratios CSV and figure saved in /work folder.")

