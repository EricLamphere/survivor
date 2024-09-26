

#' List Seasons
#' 
#' List the seasons available in this repository that have been set up
#' with a config file listing the survivors and picks
list_seasons <- function() {
    unique(season_picks$season)
}


#' Default Season
#' 
#' Returns the most recent seasons number
default_season <- function() {
    max(season_picks$season)
}

#' Label for All Seasons
#' 
#' The label used to specify all seasons
all_seasons_label <- function() {
    "All Seasons"
}

#' List Season Castaways
#' 
#' List castaways for a given season
#' 
#' @param szn Integer, season number
get_season_picks <- function(szn = default_season()) {
    all_szn_label <- all_seasons_label()
    if (szn == all_szn_label) {
        picks <- season_picks
    } else {
        picks <- 
            season_picks %>%
            dplyr::filter(season == szn)
    }
    
    picks
}