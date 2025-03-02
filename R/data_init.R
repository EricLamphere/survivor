
# googlesheets auth ----

# options(gargle_oauth_cache = ".secrets")


#' Authenticate With Google Sheets
#' 
#' @param svc_acct_json_path Path to service account json. Defaults to `.secrets/gcp-service-account.json`
#' @param gargle_creds_path Path to credentials used by gargle (hot the application deploys using github actions)
#' @param cache Where the secret is stored
#' @param email Email to authenticate with
#' @param deauth_mode Whether or not to run only with [googlesheets4::gs4_deauth()]
#' 
#' @export
gs_auth <- function(svc_acct_json_path = ".secrets/gcp-service-account.json", 
                    gargle_creds_env_name = "GCP_SERVICE_ACCOUNT_KEY_BASE64",
                    cache = ".secrets", 
                    email = "elampsart@gmail.com", 
                    deauth_mode = FALSE) {
    googlesheets4::gs4_deauth()
    
    if (deauth_mode) {
        return(invisible(NULL))
    }
    
    if (file.exists(svc_acct_json_path)) {
        cli::cli_alert_info("Authenticating using service account json key")
        googlesheets4::gs4_auth(path = svc_acct_json_path)
        return(invisible(NULL))
    }
    
    if (length(list.files(cache))) {
        cli::cli_alert_info("Authenticating using auth cache")
        googlesheets4::gs4_auth(cache = cache, email = email)
        return(invisible(NULL))
    }
    
    if (Sys.getenv(gargle_creds_env_name) != "") {
        cli::cli_alert_info("Authenticating using base64 encoded service account json key from environment variable")
        json_key <- Sys.getenv(gargle_creds_env_name)
        
        if (nzchar(json_key)) {
            # Decode the Base64 string back into JSON
            json_key <- rawToChar(base64enc::base64decode(json_key))
            
            # Convert JSON string into a list (used for authentication)
            service_account_info <- jsonlite::fromJSON(json_key)
            
            # Authenticate with Google Sheets
            googlesheets4::gs4_auth(
                credentials = gargle::credentials_service_account(
                    info = service_account_info,
                    scopes = "https://www.googleapis.com/auth/spreadsheets"
                )
            )
            return(invisible(NULL))
        } else {
            cli::cli_alert_warning("No Google Service Account key found in {gargle_creds_env_name} environment variable")
        }  
    }
    
    cli::cli_abort(c(
        "No credentials found in any of the following locations:",
        ">" = svc_acct_json_path,
        ">" = gargle_creds_env_name %&% " environment variable",
        ">" = cache %//% "*"
    ))
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


#' Retrieve All Data From Google Sheets
#' 
#' Retrieve all relevant data from googlesheets
#' 
#' @param deauth_mode Whether or not to authenticate with Google Sheets in deauth mode
gs_get_all_data <- function(deauth_mode = FALSE) {
    gs_auth()
    
    list(
        participants = gs_get_srvivor_data("participants"),
        castaways = gs_get_srvivor_data("castaways"),
        picks = gs_get_srvivor_data("picks"),
        seasons = gs_get_srvivor_data("seasons")    
    )
}



# season picks table ----

#' Process Seasons Table
#' 
#' Creates the base season picks table using either the data in googlesheets
#' or the config files
#' 
#' @param participants Participants data from googlesheet
#' @param castaways Castaway data from googlesheet
#' @param picks Picks data from googlesheet
#' @param seasons Seasons data from googlesheet
process_seasons <- function(participants, castaways, picks, seasons) {
    castaways |>
        dplyr::left_join(picks, by = c("season", "castaway_id")) |>
        dplyr::left_join(participants, by = c("participant_id")) |>
        dplyr::left_join(seasons, by = c("season")) |>
        dplyr::rename(
            season_cost_per_day = cost_per_day, 
            season_sole_survivor_bonus = sole_survivor_bonus
        ) |>
        dplyr::select(
            dplyr::starts_with("season"), 
            dplyr::starts_with("participant"), 
            dplyr::starts_with("castaway")
        )
}


#' Process Season Config
#' 
#' Process season config into a data frame
#' 
#' @param config Season config file parsed using `yaml::read_yaml`
#' @param participants Config file of participants in the pool parsed using `yaml::read_yaml`
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
                castaway_day = .x$finish$day %||% NA_integer_,
                castaway_tie_breaker = .x$finish$tie_breaker %||% NA_integer_
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
            season_sole_survivor_bonus = sole_survivor_bonus
        )
    
    dplyr::bind_rows(combined, not_picked)
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
            castaway_day_with_tie_breaker = dplyr::if_else(is.na(castaway_tie_breaker), castaway_day, castaway_day + 1/(1+castaway_tie_breaker)),
            castaway_rank = dplyr::if_else(is.na(castaway_day_with_tie_breaker), NA, 1 + dplyr::n() - rank(castaway_day_with_tie_breaker)),
            castaway_eliminated = dplyr::if_else(!is.na(castaway_day), TRUE, FALSE),
            sole_survivor = dplyr::if_else(castaway_rank == 1, TRUE, FALSE)
        ) |>
        dplyr::ungroup()
}

#' Calculate Payments & Rank
#' 
#' Calculates the payments and rank for the participants in the pool party
#' 
#' @param seasons_tbl Seasons table obtained from `process_seasons` and 
#' processed by `calculate_castaway_fields`
calculate_participant_fields <- function(seasons_tbl) {
    picked <- 
        dplyr::filter(seasons_tbl, !is.na(participant_id)) |> 
        .process_participant_name()
    unpicked <- dplyr::filter(seasons_tbl, is.na(participant_id))
    
    payments_applied <- 
        split(picked, ~season) |>
        purrr::map_dfr(
            ~ .get_participant_rank(picks = .x) |> 
                .get_season_payments()
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
    
    winner_n_days <- max(picks$castaway_day)
    season_with_payments <-
        picks |>
        dplyr::mutate(
            participant_winner = as.integer(participant_rank) == 1,
            participant_payment = season_cost_per_day * (winner_n_days - castaway_day)
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
            participant_rank = dplyr::if_else(is.na(castaway_day_with_tie_breaker), NA, 1 + n_participants - rank(castaway_day_with_tie_breaker))
        )
}


#' Parse Participant Name
#' 
#' @param picks Picks dataset
.process_participant_name <- function(picks) {
    picks |> 
        dplyr::mutate(
            participant_last = dplyr::case_when(
                participant_last == "last_name_unknown" ~ NA_character_,
                TRUE ~ participant_last
            ),
            participant_full_name = trimws(participant_first %&% ' ' %&% participant_last)
        )
}





#' Cleanup Season Picks Dataset
#' 
#' Cleanup the season picks dataset after processing has been applied. This 
#' includes removing unused fields and arranging columns and rows
#' 
#' @param season_picks Season picks table after processing is applied
cleanup_season_picks <- function(season_picks) {
    columns_cleaned <-
        season_picks |>
        dplyr::select(
            season,
            dplyr::starts_with("participant_"),
            dplyr::starts_with("castaway_"),
            sole_survivor
        ) |> 
        dplyr::arrange(desc(season), castaway_rank, castaway_id) |> 
        dplyr::select(-c(castaway_day_with_tie_breaker, castaway_tie_breaker))
    
    not_ranked <- dplyr::filter(columns_cleaned, is.na(castaway_rank))
    ranked <- dplyr::filter(columns_cleaned, !is.na(castaway_rank))
    
    
    dplyr::bind_rows(
        not_ranked,
        ranked
    ) |> 
        dplyr::arrange(desc(season))
}


#' Add Historical Data
#' 
#' @param picks Picks data from recent seasons
add_historical_data <- function(picks) {
    dplyr::bind_rows(
        picks,
        dplyr::filter(historical_castaways, season < min(picks$season))
    )
}


#' Create Season Picks Data Frame
#' 
#' Main method for creating the season picks table with all transformations applied
#' 
#' @param all_data Named list of tabs and their data from the survivor googlesheet
#' @param augment_with_historical Logical, whether or not to add historical data.
#'  See `?historical_data` for more info
#' 
#' @export
create_season_picks <- function(all_data = NULL, augment_with_historical = TRUE) {
    if (is.null(all_data)) {
        all_data <- gs_get_all_data()
    }
    
    seasons_tbl <- process_seasons(
        participants = all_data$participants,
        castaways = all_data$castaways,
        picks = all_data$picks,
        seasons = all_data$seasons
    )
    
    picks <- 
        seasons_tbl |>
        calculate_castaway_fields() |>
        calculate_participant_fields()
    
    if (augment_with_historical) {
        picks <- add_historical_data(picks)
    }
    
    cleanup_season_picks(picks)
}


# participants table ----

#' Get Participant Picking Order
#' 
#' For each season, set the picking order. If no previous season, pick randomly.
#' If there's a new participant, they are placed last.
#' 
#' @param participants_enriched Participants data with full name joined in
#' @param season_picks Season picks dataset
set_participant_picking_order <- function(participants_enriched, season_picks) {
    valid_participants <- 
        dplyr::distinct(participants_enriched, season, participant_id, participant_full_name, picking_order) |> 
        split(~season) |> 
        purrr::map(
            ~ dplyr::distinct(dplyr::select(.x, -season))
        )
    
    season_rankings <- 
        season_picks |> 
        dplyr::filter(!is.na(participant_id)) |> 
        dplyr::distinct(season, participant_id, participant_rank) |> 
        dplyr::arrange(season, desc(participant_rank)) |> 
        dplyr::group_by(season) |> 
        dplyr::mutate(next_season_picking_order = max(participant_rank, na.rm = TRUE) + 1 - participant_rank) |> 
        dplyr::ungroup() |> 
        split(~season) |> 
        purrr::map(~ dplyr::select(.x, -c(season, participant_rank)))
    
    seasons_in_play <- unique(
        names(valid_participants),
        names(season_rankings)
    )
    
    seasons_in_play |> 
        purrr::map(
            ~ .set_participant_picking_order__szn(
                season_participants = valid_participants[[.x]],
                previous_season_rankings = season_rankings[[as.character(as.integer(.x) - 1)]]
            ) |> 
                dplyr::mutate(season = as.integer(.x))
        ) |> 
        dplyr::bind_rows() |> 
        dplyr::select(
            season, dplyr::starts_with("participant_"), dplyr::everything()
        )
}

.set_participant_picking_order__szn <- function(season_participants, previous_season_rankings) {
    if (all(!is.na(season_participants$picking_order))) {
        return(season_participants)
    }
    
    # if picking_order isn't set and there's no previous season, random order
    if (is.null(previous_season_rankings)) {
        picking_order_options <- seq_len(nrow(season_participants))
        season_picking_order <- 
            season_participants |> 
            dplyr::mutate(
                picking_order = sample(picking_order_options)
            )
        
        return(season_picking_order)
    }
    
    season_picking_order <- 
        season_participants |> 
        dplyr::left_join(previous_season_rankings, by = "participant_id") |> 
        dplyr::mutate(
            picking_order = dplyr::case_when(
                is.na(picking_order) ~ next_season_picking_order,
                TRUE ~ picking_order
            )
        )
    
    if (any(is.na(season_picking_order$picking_order))) {
        max_picking_order <- max(season_picking_order$picking_order, na.rm = TRUE)
        n_na <- sum(is.na(season_picking_order$picking_order))
        picking_order_adders <- seq_len(n_na)
        new_picking_orders <- max_picking_order + picking_order_adders
        
        season_picking_order <- 
            season_picking_order |> 
            dplyr::mutate(
                picking_order = dplyr::case_when(
                    is.na(picking_order) ~ new_picking_orders,
                    TRUE ~ picking_order
                )
            )
    }
    
    season_picking_order <-
        dplyr::mutate(
            season_picking_order,
            picking_order = rank(picking_order)
        ) |> 
        dplyr::arrange(picking_order) |> 
        dplyr::select(-next_season_picking_order)
    
    season_picking_order
}




.get_participants_enriched <- function(picks, participants) {
    picks |> 
        dplyr::left_join(
            participants,
            by = "participant_id"
        ) |> 
        .process_participant_name()
}

#' Create Participants Data Frame
#' 
#' Main method for creating the participants table with all transformations applied
#' 
#' @param all_data Named list of tabs and their data from the survivor googlesheet
#' 
#' @export
create_season_participants <- function(all_data) {
    season_picks <- create_season_picks(all_data, augment_with_historical = FALSE)
    participants_enriched <- .get_participants_enriched(
        picks = all_data$picks, 
        participants = all_data$participants
    )
    
    set_participant_picking_order(
        participants_enriched = participants_enriched,
        season_picks = season_picks
    )
}




