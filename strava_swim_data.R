library(rStrava)
library(tidyverse)
library(scales)

token <- httr::config(token = readRDS(".httr-oauth")[[1]])

my_activities <- get_activity_list(token)

df_activity <- compile_activities(my_activities) |>
  as_tibble()

write_csv(df_activity, fs::path("R", "data", "activity_data.csv"))

df_swims <-
  df_activity |>
  filter(sport_type == "Swim") |>
  mutate(
    activity_date = date(ymd_hms(start_date_local)),
    distance = distance * 1000 # convert to meters
  ) |>
  select(
    name,
    activity_date,
    moving_time,
    distance
  ) |>
  summarise(
    across(.cols = c(moving_time, distance), .fns = sum),
    .by = c(name, activity_date)
  )

plot_annual_swim_totals(df_swims, 2025)

plot_monthly_swim_calendar(df_swims, 2025, 3)

# mutate(
#   start_date_local = ymd_hms(start_date_local),
#   distance = distance * 1000, # convert to meters
#   moving_time = moving_time / 60, # convert to minutes
#   pace = 360 / average_speed, # pace = seconds per 100m
#   pace_fmt = seconds_to_period(pace),
#   activity_year = year(start_date_local),
#   activity_month = month(start_date_local),
#   activity_day = day(start_date_local)
# )

# speed = kms / hr

# pace = secs per 100m
