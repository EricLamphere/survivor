


#' Get Season Pool Winner
#' 
#' @param szn Season number. If all seasons specified, returns default season
#' 
#' @return Named vector where name is the participant and value is their pick
get_pool_winner <- function(szn = default_season()) {
    szn <- force_season_number(szn)
    season_picks <- get_season_picks(szn)
    
    if (!any(season_picks$participant_rank == 1, na.rm = TRUE)) {
        return(NULL)
    }
    
    season_picks |> 
        dplyr::filter(participant_rank == 1) |> 
        dplyr::pull(castaway_name, participant_full_name)
}



#' Get Season Sole Survivor
#' 
#' @param szn Season number. If all seasons specified, returns default season
get_sole_survivor <- function(szn = default_season()) {
    szn <- force_season_number(szn)
    season_picks <- get_season_picks(szn)
    
    if (!any(season_picks$sole_survivor, na.rm = TRUE)) {
        return(NULL)
    }
    
    season_picks |> 
        dplyr::filter(sole_survivor) |> 
        dplyr::pull(castaway_name)
}

#' Pool Winners Table
#' 
#' Filter all seasons of the season_picks table to just the pool winners
get_pool_winners_table <- function() {
    get_season_picks(all_seasons_label()) |> 
        dplyr::filter(participant_rank == 1)
}


#' Sole Survivors Table
#' 
#' Filter all seasons of the season_picks table to just the sole survivors
get_sole_survivors_table <- function() {
    get_season_picks(all_seasons_label()) |> 
        dplyr::filter(castaway_rank == 1)
}




