library(readxl)
library(janitor)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

ranked_xl <- here::here("data-raw", "us-cities-ranked-2019.xlsx")
ranked_csv <- here::here("data-raw", "us-cities-ranked-2019.csv")

# Download US City Ranking ------------------------------------------------

# https://data.world/dataremixed/125-us-cities-ranked-2019
if (!file.exists(ranked_xl)) {
  download.file(
    url = "https://query.data.world/s/3vqs6pntd6w5uzqdke5aswdstryb53",
    destfile = ranked_xl
  )
}

# Clean Up City Ranking Data ----------------------------------------------

states <- state.name
names(states) <- state.abb

us_cities_ranked <-
  read_excel(ranked_xl, na = "N/A") %>%
  clean_names() %>%
  separate(
    col = avg_high_low_temps,
    into = c("avg_temp_high", "avg_temp_low"),
    sep = " / "
  ) %>%
  mutate_at(
    vars(avg_temp_high, avg_temp_low, avg_annual_rainfall, avg_commute_time),
    ~ str_remove_all(.x, "[^\\d.]")
  ) %>%
  mutate_at(vars(-city, -state, -link), as.numeric) %>%
  mutate_at(vars(us_news_rank), as.integer) %>%
  mutate(state_full = states[state]) %>%
  rename(
    avg_temp_high_f = avg_temp_high,
    avg_temp_low_f = avg_temp_low,
    avg_annual_rainfall_in = avg_annual_rainfall,
    avg_commute_time_mins = avg_commute_time
  ) %>%
  select(us_news_rank:state, state_full, everything())


# Geocode Cities ----------------------------------------------------------

geocode_csv <- here::here("data-raw", "us-cities-geocodes.csv")
if (!requireNamespace("tmaptools", quietly = TRUE) && !file.exists(geocode_csv)) {
  stop("`tmaptools` is required: install.packages('tmaptools')")
} else {
  geocodes <- if (!file.exists(geocode_csv)) {
    ranked_us_cities %>%
      mutate(query = paste0(city, ", ", state)) %>%
      pull() %>%
      tmaptools::geocode_OSM() %>%
      write_csv(geocode_csv)
  } else {
    read_csv(geocode_csv)
  }
}

if (exists("geocodes")) {
  us_cities_ranked <-
    us_cities_ranked %>%
    mutate(query = paste0(city, ", ", state)) %>%
    left_join(geocodes) %>%
    select(-query)
}

usethis::use_data(us_cities_ranked, overwrite = TRUE)
