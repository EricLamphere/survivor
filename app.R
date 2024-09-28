

library(dplyr)
library(ezextras)
library(purrr)
pkgload::load_all(".")

season_picks <- create_season_picks("googlesheets")

survivor_app()
