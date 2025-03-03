
# load libraries
cli::cli_alert_info("Loading libraries")
library(dplyr)
library(ezextras)
library(purrr)

cli::cli_alert_info("Loading survivor package")
pkgload::load_all(".")


# R environ file with special environment variables. Right now, this is the only
# way to deploy from github actions because you can't use the "envVars" parameter
# in rsconnect::deployApp unless you're using Connect
prepare_env <- function() {
    renviron_file <- ".custom_env"
    
    if (file.exists(renviron_file)) {
        cli::cli_alert_warning("Reading variables from {renviron_file} then removing file")
        readRenviron(path = renviron_file)
        file.remove(renviron_file)
    } else {
        cli::cli_alert_warning("{renviron_file} file doesn't exist")
    }
}

print(list.files("./", all.files = TRUE))
print(getwd())
prepare_env()


# GLOBALS
# > I just don't want to refactor everything to take a dataframe input
counter <<- 0

all_data <- gs_get_all_data()
season_picks <<- create_season_picks(all_data = all_data, augment_with_historical = TRUE)
season_participants <<- create_season_participants(all_data = all_data)

# run app
cli::cli_alert_info("Starting application")
survivor_app()
# shiny::runApp()