

#' List Seasons
#' 
#' List the seasons available in this repository that have been set up
#' with a config file listing the survivors and picks
list_seasons <- function() {
    unique(season_picks$season)
}

#' List Season Castaways
#' 
#' List castaways for a given season
#' 
#' @param Season Integer, season number
season_castaways <- function(season) {
  dplyr::filter(
      season_picks,
      season = as.integer(season)
  )  
}