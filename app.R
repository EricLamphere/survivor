
# load libraries
library(dplyr)
library(ezextras)
library(purrr)
pkgload::load_all(".")

# load data
season_picks <- create_season_picks("googlesheets")

# run app
survivor_app()
