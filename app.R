
# load libraries
library(dplyr)
library(ezextras)
library(purrr)
pkgload::load_all(".")

# GLOBALS
# > I just don't want to refactor everything to take a dataframe input
counter <<- 0
season_picks <<- create_season_picks("googlesheets", augment_with_historical = TRUE)

# run app
survivor_app()
# shiny::runApp()