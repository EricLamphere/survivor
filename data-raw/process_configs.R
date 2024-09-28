


# LIBRARIES ----
library(dplyr)
library(purrr)
library(ezextras)

# EXEC ----
season_picks <- create_season_picks(
    using = "googlesheets",
    config_path = "data-raw/configs",
    cost_per_day = 1,
    sole_survivor_bonus = 5
)

# WHEN THIS TABLE IS UPDATED: update the data.R doc string
usethis::use_data(season_picks, overwrite = TRUE)
# sinew::makeOxygen(season_picks)
