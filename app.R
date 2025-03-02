
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

gs_auth()

all_data <- gs_get_all_data()
season_picks <<- create_season_picks(all_data = all_data, augment_with_historical = TRUE)
season_participants <<- create_season_participants(all_data = all_data)

# run app
cli::cli_alert_info("Starting application")
survivor_app()
# shiny::runApp()