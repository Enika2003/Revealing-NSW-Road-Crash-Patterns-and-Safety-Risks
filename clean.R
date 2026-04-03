# Load required packages
library(readxl)
library(dplyr)
library(janitor)
library(forcats)
library(readr)

# Step 1: Load the Excel data
raw <- read_excel("/Users/enika/Desktop/stat8111/data/NSW-crash-data.xlsx", sheet = 1) |>
  clean_names()

# Step 2: Select key variables for analysis
keep <- c("crash_id", "year_of_crash", "month_of_crash", "day_of_week_of_crash",
          "two_hour_intervals", "urbanisation", "natural_lighting", "street_lighting",
          "weather", "speed_limit", "degree_of_crash_detailed")
df <- raw |> select(any_of(keep))

# Step 3: Create a binary injury flag
injury_levels <- c("Minor/Other Injury", "Moderate Injury", "Serious Injury", "Killed")
df <- df |>
  mutate(
    injury = case_when(
      degree_of_crash_detailed == "Non-casualty (towaway)" ~ 0L,
      degree_of_crash_detailed %in% injury_levels ~ 1L,
      TRUE ~ NA_integer_
    )
  )

# Step 4: Clean up factors and categories
df <- df |>
  mutate(
    natural_lighting = fct_lump_n(factor(natural_lighting), 6),
    street_lighting  = fct_lump_n(factor(street_lighting), 6),
    weather          = fct_lump_n(factor(weather), 6),
    urbanisation     = factor(urbanisation),
    speed_limit      = factor(speed_limit)
  )

# Step 5: Drop missing injury cases
model_df <- df |> filter(!is.na(injury))

# Step 6: Save cleaned files
write_csv(df, "/Users/enika/Desktop/stat8111/work/nsw_crash_clean.csv")
write_csv(model_df, "/Users/enika/Desktop/stat8111/work/nsw_crash_model.csv")

cat("Cleaning complete! Files saved in /work")

