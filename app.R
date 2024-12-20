
# load libraries
cli::cli_alert_info("Loading libraries")
library(dplyr)
library(ezextras)
library(purrr)

cli::cli_alert_info("Loading survivor package")
pkgload::load_all(".")


# GLOBALS
# > I just don't want to refactor everything to take a dataframe input
counter <<- 0
season_picks <<- create_season_picks("googlesheets", augment_with_historical = TRUE)

# run app
cli::cli_alert_info("Starting application")
survivor_app()
# shiny::runApp()