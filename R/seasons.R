

#' List Seasons
#' 
#' List the seasons available in this repository that have been set up
#' with a config file listing the survivors and picks
#' 
#' @export
list_seasons <- function() {
    unique(season_picks$season)
}


#' Default Season
#' 
#' Returns the most recent seasons number
#' 
#' @export
default_season <- function() {
    max(season_picks$season)
}

#' Label for All Seasons
#' 
#' The label used to specify all seasons
#' 
#' @export
all_seasons_label <- function() {
    "All Seasons"
}


#' Get Season Number
#' 
#' If all seasons are specified, then choose the default season
#' 
#' @param szn Season number
force_season_number <- function(szn = default_season()) {
    if (szn == all_seasons_label()) {
        szn <- default_season()
    }
    
    szn
}


#' List Season Castaways
#' 
#' List castaways for a given season
#' 
#' @param szn Integer, season number
#' @param picked Logical, whether or not to filter only to the castaways
#'  that have been picked
#' 
#' @export
get_season_picks <- function(szn = default_season(), picked = FALSE) {
    all_szn_label <- all_seasons_label()
    if (szn == all_szn_label) {
        picks <- season_picks
    } else {
        picks <- dplyr::filter(season_picks, season == szn)
    }
    
    if (picked) {
        picks <- dplyr::filter(picks, !is.na(participant_id))
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
#' @param pretty Logical, whether to show the participants full name (TRUE, default) 
#'  or their ID (FALSE)
#' 
#' @export
get_season_picking_order <- function(szn = default_season(), pretty = TRUE) {
    if (szn == all_seasons_label()) {
        cli::cli_alert_info("Picking order not implemented when all seasons selected - returning NULL")
        return(NULL)
    }
    
    last_szn <- as.integer(szn) - 1
    if (last_szn %notin% unique(season_picks$season)) {
        cli::cli_alert_info("No previous season to determine order by, sorting everyone randomly")
        all_season_picks <- get_season_picks(all_seasons_label(), picked = TRUE)
        if (pretty) {
            return(sample(unique(all_season_picks$participant_full_name)))
        } else {
            return(sample(unique(all_season_picks$participant_id)))
        }
    }
    
    picks_arranged <- 
        get_season_picks(szn = last_szn, picked = TRUE) |>
        dplyr::arrange(desc(castaway_rank)) |> 
        dplyr::distinct(participant_id, participant_full_name)
    
    if (pretty) {
        dplyr::pull(picks_arranged, participant_full_name)
    } else {
        dplyr::pull(picks_arranged, participant_id)
    }
}


#' Last Castaway Voted Out
#' 
#' Get the name of the last castaway voted out. If the season is over, returns
#' the two runners up. If the season just started, returns a string saying no
#' one has been voted out yet
#' 
#' @param szn Season number
#' 
#' @export
last_voted_out <- function(szn = default_season()) {
    picks <- get_season_picks(szn = szn)
    
    # if no one has been voted out yet, say so
    if (!any(picks$castaway_eliminated)) {
        return("No one voted out yet!")
    }
    
    # if the season is over, return the runners up
    if (all(picks$castaway_eliminated)) {
        runners_up <- 
            picks |>
            dplyr::filter(castaway_rank %in% c(2, 3)) |>
            dplyr::pull(castaway_name)
        
        return(paste(runners_up, collapse = " and "))
    }
    
    last_voted_out <- 
        picks |>
        dplyr::filter(season == max(season)) |>
        dplyr::filter(castaway_day == max(castaway_day, na.rm = TRUE)) |>
        dplyr::pull(castaway_name)
    
    last_voted_out
}

#' Get Number of Remaining Castaways
#' 
#' @param szn Season number
#' 
#' @export
get_castaways_remaining <- function(szn = default_season()) {
    picks <- get_season_picks(szn = szn)
    
    sum(!picks$castaway_eliminated)
}


#' Make Season Wiki Link
#' 
#' @param szn Season number
make_season_wiki_link <- function(szn = default_season()) {
    szn <- force_season_number()
    paste0("https://en.wikipedia.org/wiki/Survivor_", szn)
}


#' Filter the season_picks Table
#' 
#' Filters the season_picks table based on a set of condiions passed to `dplyr::filter`
#' 
#' @param ... Filter conditions, e.g. `castaway_name %~% 'rob'`
#' @param .szn Season number
#' 
#' @return Data frame
get_pick_data <- function(..., .szn = default_season()) {
    szn <- force_season_number(.szn)
    season_picks <- get_season_picks(szn)
    
    tryCatch(
        dplyr::filter(season_picks, ...),
        error = function(e) {
            cli::cli_abort(c(
                "Failed with error: {e}",
                "Note: available colums are {crayon::cyan(colnames(season_picks))}"
            ))
        }
    )
}


#' Check if Season Started
#' 
#' @param szn Season number
#' 
#' @export
season_has_started <- function(szn = default_season()) {
    szn <- force_season_number(szn)
    season_picks <- get_season_picks(szn = szn)
    
    sum(season_picks$castaway_eliminated, na.rm = TRUE) > 1
}




