


# options(gargle_oauth_cache = ".secrets")


#' Authenticate With Google Sheets
#' 
#' @param cache Where the secret is stored
#' @param email Email to authenticate with
#' @param deauth_mode Whether or not to run only with [googlesheets4::gs4_deauth()]
#' 
#' @export
gs_auth <- function(cache = ".secrets", email = "elampsart@gmail.com", deauth_mode = FALSE) {
    googlesheets4::gs4_deauth()
    
    if (!deauth_mode) {
        googlesheets4::gs4_auth(cache = cache, email = email)  
    }
}


#' Get Survivor Spreadsheet
#' 
#' Uses [googlesheets4::gs4_get()] to get nested data frame of sheets and tabs
#' 
#' @param tab_name Name of the tab to extract
#' 
#' @export
gs_get_srvivor_data <- function(tab_name) {
    sheet_id <- "1-lTGtzfeH4_Fq0hq6p5WsC760F3x0Ir4TbN7gLMu2UM"
    googlesheets4::read_sheet(sheet_id, sheet = tab_name)
}


#' Process Season Config
#' 
#' Process season config into a data frame
#' 
#' @param config Season config file parsed using `yaml::read_yaml`
#' @param participants Config file of participants in the pool parsed using `yaml::read_yaml`
#' 
#' @export
process_season_config <- function(config, participants) {
    season <- config$season
    cost_per_day <- config$cost_per_day
    sole_survivor_bonus <- config$sole_survivor_bonus
    picks <- config$picks
    castaways <- config$castaways
    
    picks_tbl <- tibble::tibble(
        season = season,
        season_cost_per_day = cost_per_day,
        season_sole_survivor_bonus = sole_survivor_bonus,
        participant_id = names(picks),
        castaway_id = unlist(picks, use.names = FALSE)
    ) |>
        dplyr::left_join(
            participants,
            by = "participant_id"
        ) |>
        dplyr::select(dplyr::starts_with("season"), dplyr::starts_with("participant"), castaway_id)
    
    castaway_tbl <- 
        purrr::imap_dfr(
            castaways,
            ~ tibble::tibble(
                castaway_id = .y,
                castaway_name = .x$full_name,
                castaway_finish_day = .x$finish$day %||% NA_integer_,
                castaway_finish_tie_breaker = .x$finish$tie_breaker %||% NA_integer_
            )
        )
    
    combined <- 
        picks_tbl |>
        dplyr::left_join(
            castaway_tbl,
            by = "castaway_id"
        ) |>
        dplyr::select(dplyr::starts_with("season"), dplyr::starts_with("participant"), dplyr::starts_with("castaway"))
    
    not_picked <- 
        dplyr::filter(castaway_tbl, castaway_id %notin% unique(combined$castaway_id)) |>
        dplyr::mutate(
            season = season, 
            season_cost_per_day = cost_per_day,
            season_sole_survivor_bonus = cost_per_day
        )
    
    dplyr::bind_rows(combined, not_picked)
}


#' Process All Seasons Using Configs
#' 
#' Process each seasons config file with `process_season_config` and bind
#' all seasons together
#' 
#' @param config_path Path to the config files, defaults to the right path
.process_seasons__configs <- function(config_path = "data-raw/configs") {
    seasons_config_path <- config_path %//% "seasons"
    castaway_files <- list.files(seasons_config_path, recursive = TRUE)
    
    pool_party <- yaml::read_yaml(config_path %//% "pool_party.yml")
    participants <- pool_party$participants
    participants_lookup <- 
        purrr::imap_dfr(
            participants,
            ~ tibble::tibble(
                participant_id = .y,
                participant_first = .x$first,
                participant_last = .x$last
            )
        )
    
    season_picks <- purrr::map_dfr(
        castaway_files, 
        ~ process_season_config(
            config = yaml::read_yaml(file.path(seasons_config_path , .x)),
            participants = participants_lookup
        )
    )
}


#' Process All Seasons Using Google Sheets
#' 
#' Process the data in the survivor Google Sheet
#' 
#' @param deauth_mode Whether or not to authenticate with Google Sheets in deauth mode
.process_seasons__googlesheets <- function(deauth_mode = FALSE) {
    gs_auth(deauth_mode = deauth_mode)
    participants <- gs_get_srvivor_data("participants")
    castaways <- gs_get_srvivor_data("castaways")
    picks <- gs_get_srvivor_data("picks")
    seasons <- gs_get_srvivor_data("seasons")
    
    castaways |>
        dplyr::left_join(picks, by = c("season", "castaway_id")) |>
        dplyr::left_join(participants, by = c("participant_id")) |>
        dplyr::left_join(seasons, by = c("season")) |>
        dplyr::rename(
            season_cost_per_day = cost_per_day, 
            season_sole_survivor_bonus = sole_survivor_bonus
        ) |>
        dplyr::select(dplyr::starts_with("season"), dplyr::starts_with("participant"), dplyr::starts_with("castaway"))
}


#' Process Seasons Table
#' 
#' Creates the base season picks table using either the data in googlesheets
#' or the config files
#' 
#' @param using Either googlesheets or configs
#' @param config_path Path to the config files
#' @param deauth_mode Whether or not to authenticate with Google Sheets in deauth mode
#' 
#' @export
process_seasons <- function(using = c("googlesheets", "configs"), 
                            config_path = "data-raw/configs",
                            deauth_mode = FALSE) {
    using <- match.arg(using)
    
    switch(
        using,
        googlesheets = .process_seasons__googlesheets(deauth_mode = deauth_mode),
        configs = .process_seasons__configs(config_path = config_path)
    )
}





#' Calculate Castaway Fields
#' 
#' Calculates the fields relevant to castaways for the pool, including their
#' placement, whether or not they were eliminated, and if they were the sole
#' survivor
#' 
#' @param seasons_tbl Seasons table obtained from `process_seasons`
calculate_castaway_fields <- function(seasons_tbl) {
    seasons_tbl |>
        dplyr::arrange(dplyr::desc(season)) |>
        dplyr::group_by(season) |>
        dplyr::mutate(
            castaway_day_with_tie_breaker = dplyr::if_else(is.na(castaway_finish_tie_breaker), castaway_finish_day, castaway_finish_day + 1/(1+castaway_finish_tie_breaker)),
            castaway_finish_placement = dplyr::if_else(is.na(castaway_day_with_tie_breaker), NA, 1 + dplyr::n() - rank(castaway_day_with_tie_breaker)),
            castaway_eliminated = dplyr::if_else(!is.na(castaway_finish_day), TRUE, FALSE),
            sole_survivor = dplyr::if_else(castaway_finish_placement == 1, TRUE, FALSE)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-c(castaway_day_with_tie_breaker, castaway_finish_tie_breaker))
}

#' Calculate Payments & Rank
#' 
#' Calculates the payments and rank for the participants in the pool party
#' 
#' @param seasons_tbl Seasons table obtained from `process_seasons` and 
#' processed by `calculate_castaway_fields`
calculate_participant_fields <- function(seasons_tbl) {
    picked <- dplyr::filter(seasons_tbl, !is.na(participant_id))
    unpicked <- dplyr::filter(seasons_tbl, is.na(participant_id))
    
    payments_applied <- 
        split(picked, ~season) |>
        purrr::map_dfr(
            ~ .get_season_payments(picks = .x) |> 
                .get_participant_rank()
        )
    
    dplyr::bind_rows(
        payments_applied,
        unpicked
    )
}


#' Get Participant Payments
#' 
#' @param picks Picks dataset
.get_season_payments <- function(picks) {
    if (!all(picks$castaway_eliminated)) {
        return(dplyr::mutate(picks, participant_payment = NA_integer_))
    }
    
    winner_n_days <- max(picks$castaway_finish_day)
    season_with_payments <-
        picks |>
        dplyr::mutate(
            participant_winner = castaway_finish_day == max(castaway_finish_day),
            participant_payment = season_cost_per_day * (winner_n_days - castaway_finish_day)
        )
    
    if (any(picks$sole_survivor, na.rm = TRUE)) {
        season_with_payments <- dplyr::mutate(season_with_payments, participant_payment = participant_payment + season_sole_survivor_bonus)
    }
    
    season_with_payments <-
        season_with_payments |>
        dplyr::mutate(
            participant_payment = dplyr::if_else(
                participant_winner,
                0,
                participant_payment
            ),
            participant_payment = dplyr::if_else(
                participant_winner,
                sum(participant_payment),
                participant_payment
            )
        )
    
    season_with_payments
}


#' Get Participant Rank
#' 
#' @param picks Picks dataset
.get_participant_rank <- function(picks) {
    n_participants = nrow(picks)
    
    picks |>
        dplyr::mutate(
            participant_rank = dplyr::if_else(is.na(castaway_finish_placement), NA, 1 + n_participants - rank(castaway_finish_day))
        )
}


#' Cleanup Season Picks Dataset
#' 
#' Cleanup the season picks dataset after processing has been applied. This 
#' includes removing unused fields and arranging columns and rows
#' 
#' @param season_picks Season picks table after processing is applied
cleanup_season_picks <- function(season_picks) {
    season_picks |>
        dplyr::arrange(dplyr::desc(season)) |>
        dplyr::select(
            season,
            dplyr::starts_with("participant_"),
            dplyr::starts_with("castaway_"),
            sole_survivor
        )
}


#' Create Season Picks Data Frame
#' 
#' Main method for creating the season picks table with all transformations applied
#' 
#' @param using Either googlesheets or configs
#' @param config_path Path to the config files, defaults to the right path
#' @param deauth_mode Whether or not to authenticate with Google Sheets in deauth mode
#' 
#' @export
create_season_picks <- function(using = c("googlesheets", "configs"), 
                                config_path = "data-raw/configs",
                                deauth_mode = FALSE) {
    using <- match.arg(using)
    seasons_tbl <- process_seasons(
        using = using,
        config_path = config_path,
        deauth_mode = deauth_mode
    )
    
    seasons_tbl |>
        calculate_castaway_fields() |>
        calculate_participant_fields() |>
        cleanup_season_picks()
}
