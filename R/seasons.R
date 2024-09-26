

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
#' @param picked Logical, whether or not to filter only to the castaways
#'  that have been picked
get_season_picks <- function(szn = default_season(), picked = FALSE) {
    all_szn_label <- all_seasons_label()
    if (szn == all_szn_label) {
        picks <- season_picks
    } else {
        picks <- dplyr::filter(season_picks, season == szn)
    }
    
    if (picked) {
        picks <- dplyr::filter(picks, !is.na(person_id))
    }
    
    picks
}


#' Get Season Picking Order
#' 
#' Get the picking order for a season. This only applies when `szn` is an integer.
#' Notes:
#'  * When the previous season doesn't exist, all historical participants 
#'    are sorted randomly
#'  * Only participants in the `season_picks` table are included, so new participants
#'    may need to be added manually
#' 
#' @param szn Integer, the season to find the picking order for
get_season_picking_order <- function(szn = default_season()) {
    if (szn == all_seasons_label()) {
        cli::cli_alert_info("Picking order not implemented when all seasons selected - returning NULL")
        return(NULL)
    }
    
    last_szn <- szn - 1
    if (last_szn %notin% unique(season_picks$season)) {
        cli::cli_alert_info("No previous season to determine order by, sorting everyone randomly")
        all_season_picks <- get_season_picks(all_seasons_label(), picked = TRUE)
        return(sample(unique(all_season_picks$person_id)))
    }
    
    get_season_picks(szn = last_szn, picked = TRUE) %>%
        dplyr::arrange(desc(castaway_finish_placement)) %>%
        dplyr::distinct(person_id) %>%
        dplyr::pull(person_id)
}
