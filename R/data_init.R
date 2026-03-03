
# googlesheets auth ----

# options(gargle_oauth_cache = ".secrets")


#' Authenticate With Google Sheets
#' 
#' Tries multiple authentication methods in this order:
#' 1. Service account JSON file (local prod)
#' 2. OAuth cache (local dev)
#' 3. Base64-encoded service account from environment variable (CI/CD)
#' 
#' @param svc_acct_json_path Path to service account json. Defaults to `.secrets/gcp-service-account.json`
#' @param gargle_creds_env_name Name of env var containing base64 encoded JSON service account
#' @param cache Path to OAuth cache directory
#' @param email Email for OAuth authentication
#' @param deauth_mode Logical: if TRUE, run `gs4_deauth()` and exit
#' 
#' @export
gs_auth <- function(
    svc_acct_json_path = ".secrets/gcp-service-account.json",
    gargle_creds_env_name = "GCP_SERVICE_ACCOUNT_KEY_BASE64",
    cache = ".secrets",
    email = "elampsart@gmail.com",
    deauth_mode = FALSE
) {
    scopes <- c(
        # "https://www.googleapis.com/auth/spreadsheets",
        "https://www.googleapis.com/auth/spreadsheets.readonly",
        "https://www.googleapis.com/auth/drive.readonly"
    )
    
    if (deauth_mode) {
        cli::cli_alert_info("Running in deauth mode.")
        googlesheets4::gs4_deauth()
        return(invisible(NULL))
    }

    tryCatch({
        # --- Method 1: Service account JSON file ---
        if (file.exists(svc_acct_json_path)) {
            cli::cli_alert_info("Authenticating with method #1: Service account JSON file")
            googlesheets4::gs4_auth(path = svc_acct_json_path, scopes = scopes)
            cli::cli_alert_success("Authenticated via service account JSON file")
            return(invisible(NULL))
        }
        
        # --- Method 2: OAuth cache ---
        token_files <- list.files(cache, full.names = TRUE)
        if (length(token_files) && any(grepl("gargle", token_files))) {
            cli::cli_alert_info("Authenticating with method #2: OAuth token cache")
            googlesheets4::gs4_auth(cache = cache, email = email, scopes = scopes)
            cli::cli_alert_success("Authenticated via OAuth token cache")
            return(invisible(NULL))
        }

        # --- Method 3: Base64 encoded env var ---
        json_key_b64 <- Sys.getenv(gargle_creds_env_name)
        if (nzchar(json_key_b64)) {
            cli::cli_alert_info("Authenticating with method #3: base64 encoded service account key")
            json_key <- rawToChar(base64enc::base64decode(json_key_b64))
            service_account_info <- jsonlite::fromJSON(json_key)
            
            googlesheets4::gs4_auth(
                credentials = gargle::credentials_service_account(
                    info = service_account_info,
                    scopes = scopes
                ),
                scopes = scopes
            )
            cli::cli_alert_success("Authenticated via base64 encoded service account key")
            return(invisible(NULL))
        }

        # --- If all methods fail ---
        cli::cli_abort(c(
            "No valid credentials found.",
            ">" = paste0("Method #1: Service account JSON file at ", svc_acct_json_path),
            ">" = paste0("Method #2: OAuth token cache in ", cache),
            ">" = paste0("Method #3: Env var ", gargle_creds_env_name)
        ))

    }, error = function(e) {
        cli::cli_alert_danger("Authentication failed: {e$message}")
        stop("Google Sheets authentication failed. Check your credentials or environment configuration.")
    })
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
            dplyr::any_of("season_name"),
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


#' Get Season Names from survivoR Package
#'
#' Returns a data frame with `season` and `season_name` for all US seasons,
#' with the "Survivor: " prefix stripped. Seasons whose subtitle is just a
#' number (e.g. "Survivor: 49") are returned as NA.
#'
#' @keywords internal
.get_survivoR_season_names <- function() {
    survivoR::season_summary |>
        dplyr::filter(version == "US") |>
        dplyr::select(season, season_name) |>
        dplyr::mutate(
            season = as.integer(season),
            subtitle = sub("^Survivor:? *", "", season_name),
            season_name = dplyr::if_else(grepl("^[0-9]+$", subtitle), NA_character_, subtitle)
        ) |>
        dplyr::select(season, season_name)
}


#' Enrich Picks With Season Names
#'
#' Fills in `season_name` from the survivoR package where it is missing.
#' The Google Sheet value (already in the data) takes priority via coalesce.
#'
#' @param picks Picks data frame
#' @keywords internal
.enrich_with_season_names <- function(picks) {
    if (!"season_name" %in% names(picks)) {
        picks <- dplyr::mutate(picks, season_name = NA_character_)
    }

    survivoR_names <- tryCatch(
        .get_survivoR_season_names(),
        error = function(e) dplyr::tibble(season = integer(), season_name = character())
    )

    picks |>
        dplyr::left_join(survivoR_names, by = "season", suffix = c("", "_survivoR")) |>
        dplyr::mutate(season_name = dplyr::coalesce(season_name, season_name_survivoR)) |>
        dplyr::select(-dplyr::any_of("season_name_survivoR"))
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
        calculate_participant_fields() |>
        .enrich_with_season_names()

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
        dplyr::mutate(next_season_picking_order = max(c(-Inf, participant_rank), na.rm = TRUE) + 1 - participant_rank) |> 
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

# season logo URLs ----

#' Fetch Season Logo URLs from Fandom Wiki
#'
#' Makes a batch imageinfo API call to survivor.fandom.com to get CDN URLs for
#' each season's logo. The standard filename `US_SN_logo.png` is tried first.
#' For named seasons where that file doesn't exist (e.g. season 50), the
#' subtitle from `survivoR::season_summary` is used to derive the filename
#' (e.g. "In the Hands of the Fans" -> `In_the_Hands_of_the_Fans_Logo.png`).
#'
#' Returns a named list of season number (as string) -> CDN URL, suitable for
#' use directly in `shiny::img(src = ...)`.
#'
#' @param seasons Integer vector of season numbers to fetch. Defaults to all
#'   seasons in the global `season_picks`.
#'
#' @export
fetch_season_logo_urls <- function(seasons = unique(season_picks$season)) {
    fandom_api <- "https://survivor.fandom.com/api.php"
    seasons <- sort(as.integer(seasons))

    # --- Pass 1: standard US_SN_logo.png filenames ---
    standard_files <- paste0("File:US_S", seasons, "_logo.png")
    result1 <- .fandom_imageinfo(fandom_api, standard_files)
    if (is.null(result1)) return(list())

    urls <- list()
    missing_seasons <- integer(0)

    for (page in result1$query$pages) {
        # MediaWiki normalizes underscores to spaces in returned titles
        norm <- gsub(" ", "_", page$title)
        m <- regmatches(norm, regexpr("(?<=US_S)\\d+(?=_logo)", norm, perl = TRUE))
        if (!length(m)) next
        n <- as.integer(m)
        if (!is.null(page$imageinfo) && length(page$imageinfo) > 0) {
            urls[[as.character(n)]] <- page$imageinfo[[1]]$url
        } else {
            missing_seasons <- c(missing_seasons, n)
        }
    }

    # --- Pass 2: for named seasons, derive filename from survivoR season_name ---
    if (length(missing_seasons) > 0) {
        szn_names <- survivoR::season_summary[
            survivoR::season_summary$version == "US" &
                survivoR::season_summary$season %in% missing_seasons,
        ]

        fallback_files <- character(0)
        fallback_key   <- character(0)

        for (i in seq_len(nrow(szn_names))) {
            subtitle <- sub("^Survivor:?\\s*", "", szn_names$season_name[i])
            filename <- paste0("File:", gsub("\\s+", "_", subtitle), "_Logo.png")
            fallback_files <- c(fallback_files, filename)
            fallback_key   <- c(fallback_key, as.character(szn_names$season[i]))
        }

        names(fallback_key) <- fallback_files

        result2 <- .fandom_imageinfo(fandom_api, fallback_files)
        if (!is.null(result2)) {
            for (page in result2$query$pages) {
                norm <- gsub(" ", "_", page$title)
                szn <- fallback_key[[norm]]
                if (!is.null(szn) && !is.null(page$imageinfo) && length(page$imageinfo) > 0) {
                    urls[[szn]] <- page$imageinfo[[1]]$url
                }
            }
        }
    }

    urls
}

#' @keywords internal
.fandom_imageinfo <- function(api_base, file_titles) {
    titles_param <- paste(file_titles, collapse = "|")
    api_url <- paste0(
        api_base,
        "?action=query&prop=imageinfo&iiprop=url&format=json&titles=",
        utils::URLencode(titles_param, reserved = TRUE)
    )
    tryCatch(
        jsonlite::fromJSON(api_url, simplifyVector = FALSE),
        error = function(e) {
            cli::cli_alert_warning("Failed to fetch season logo URLs: {e$message}")
            NULL
        }
    )
}


#' Default Survivor Logo
#'
#' Returns the filename (or path) for the default Survivor logo, used when no
#' season-specific logo is available (e.g. on the Welcome tab or when "All Seasons"
#' is selected).
#'
#' @param include_path Logical; if `TRUE` returns the full relative path including
#'   the `www/` directory prefix. Defaults to `FALSE`, returning just the filename.
#'
#' @return A character string: the filename or path to the default logo.
#'
#' @export
default_survivor_logo <- function(include_path = FALSE) {
    file <- "survivor-logo.png"
    path <- "www"
    
    if (include_path) {
        path %//% file 
    }
    
    file
}


#' Create Participants Data Frame
#'
#' Main method for creating the participants table with all transformations applied
#'
#' @param all_data Named list of tabs and their data from the survivor googlesheet
#'
#' @export
create_season_participants <- function(all_data = NULL) {
    if (is.null(all_data)) {
        all_data <- gs_get_all_data()
    }
    
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




