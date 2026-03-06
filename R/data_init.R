
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


#' Get Season Names
#'
#' Two-pass lookup for season subtitle(s). The first pass checks the provided
#' `picks` data frame (Google Sheet data). The second pass falls back to
#' `survivoR::season_summary` for any seasons not found in the first pass.
#'
#' Returns a tibble with `season` (integer) and `season_name` (subtitle, with
#' the "Survivor: " prefix already stripped). Seasons with no known subtitle
#' are excluded.
#'
#' @param seasons Integer vector of season numbers to look up
#' @param picks Optional data frame with `season` and `season_name` columns
#'   used as the first-pass source. If `NULL`, uses the global `season_picks`.
#' @export
get_season_name_tbl <- function(seasons, picks = NULL) {
    seasons <- unique(as.integer(seasons))

    data_source <- if (!is.null(picks)) {
        picks
    } else if (exists("season_picks", envir = .GlobalEnv)) {
        season_picks
    } else {
        NULL
    }

    # First pass: picks data (Google Sheet)
    from_data <- dplyr::tibble(season = integer(), season_name = character())
    if (!is.null(data_source) && all(c("season", "season_name") %in% names(data_source))) {
        from_data <- data_source |>
            dplyr::filter(season %in% seasons, !is.na(season_name), nchar(season_name) > 0) |>
            dplyr::distinct(season, season_name) |>
            dplyr::mutate(season = as.double(season), season_name = as.character(season_name)) |>
            dplyr::group_by(season) |>
            dplyr::slice(1) |>
            dplyr::ungroup()
    }

    # Second pass: survivoR for remaining seasons
    remaining <- setdiff(seasons, from_data$season)
    if (length(remaining) > 0) {
        from_survivoR <- tryCatch(
            survivoR::season_summary |>
                dplyr::filter(version == "US", season %in% remaining) |>
                dplyr::transmute(
                    season = as.integer(season),
                    subtitle = sub("^Survivor:? *", "", season_name),
                    season_name = dplyr::if_else(grepl("^[0-9]+$", subtitle), NA_character_, subtitle)
                ) |>
                dplyr::select(season, season_name) |>
                dplyr::filter(!is.na(season_name)),
            error = function(e) dplyr::tibble(season = integer(), season_name = character())
        )
        from_data <- dplyr::bind_rows(from_data, from_survivoR)
    }

    from_data
}


#' Enrich Picks With Season Names
#'
#' Fills in `season_name` where it is missing using [get_season_name_tbl()]:
#' first from the Google Sheet data already in `picks`, then from
#' `survivoR::season_summary` as a fallback.
#'
#' @param picks Picks data frame
#' @keywords internal
.enrich_with_season_names <- function(picks) {
    if (!"season_name" %in% names(picks)) {
        picks <- dplyr::mutate(picks, season_name = NA_character_)
    }

    season_names <- get_season_name_tbl(unique(picks$season), picks = picks)

    picks |>
        dplyr::left_join(season_names, by = "season", suffix = c("", "_lookup")) |>
        dplyr::mutate(season_name = dplyr::coalesce(season_name, season_name_lookup)) |>
        dplyr::select(-dplyr::any_of("season_name_lookup"))
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


# season logo URLs ----

#' Fetch Season Logo URLs from Fandom Wiki
#'
#' Makes a batch imageinfo API call to survivor.fandom.com to get CDN URLs for
#' each season's logo. The standard filename `US_SN_logo.png` is tried first.
#' For named seasons where that file doesn't exist (e.g. season 50), the
#' subtitle from [get_season_name_tbl()] is used to derive the filename
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

    # --- Pass 2: for named seasons, derive filename from season name ---
    if (length(missing_seasons) > 0) {
        szn_names <- get_season_name_tbl(missing_seasons)

        fallback_files <- character(0)
        fallback_key   <- character(0)

        for (i in seq_len(nrow(szn_names))) {
            subtitle <- szn_names$season_name[i]
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



# castaway image URLs ----

.castaway_cache <- new.env(parent = emptyenv())


# Cache for nickname mappings (loaded once per session)
.nickname_cache <- new.env(parent = emptyenv())


#' Load Castaway Nickname Mappings from YAML
#'
#' Loads nickname mappings from the YAML configuration file.
#' Results are cached for the session.
#'
#' @return List of nickname mappings
#' @keywords internal
.load_nickname_mappings <- function() {
    # Check cache first
    if (exists("mappings", envir = .nickname_cache, inherits = FALSE)) {
        return(get("mappings", envir = .nickname_cache))
    }

    # Load from YAML file
    yaml_path <- system.file("extdata", "castaway_nicknames.yaml", package = "survivor")

    if (!file.exists(yaml_path)) {
        cli::cli_alert_warning("Nickname mapping file not found: {yaml_path}")
        return(list())
    }

    mappings <- yaml::read_yaml(yaml_path)

    # Cache the mappings
    assign("mappings", mappings, envir = .nickname_cache)

    mappings
}


#' Get Castaway Nickname for Image Lookup
#'
#' Maps full/database names to their commonly known show nicknames for image lookups.
#' Supports both global nicknames and season-specific overrides.
#'
#' @param name Castaway name from the database
#' @param season Season number (optional). If provided, will check for season-specific nickname first.
#' @return Nickname if one exists, otherwise NULL
#' @keywords internal
get_castaway_nickname <- function(name, season = NULL) {
    mappings <- .load_nickname_mappings()

    # Check if this castaway has a nickname mapping
    if (!name %in% names(mappings)) {
        return(NULL)
    }

    castaway_mapping <- mappings[[name]]

    # If season is provided and there's a season-specific mapping, use it
    if (!is.null(season) && !is.null(castaway_mapping[[as.character(season)]])) {
        return(castaway_mapping[[as.character(season)]])
    }

    # Otherwise use the default mapping
    if (!is.null(castaway_mapping[["default"]])) {
        return(castaway_mapping[["default"]])
    }

    # If neither exists, return NULL
    NULL
}


#' Fetch Castaway Thumbnail Image URLs for a Season
#'
#' Queries the Survivor Fandom wiki's MediaWiki `imageinfo` API for CDN
#' thumbnail URLs for all castaways in a season. Multiple filename candidates
#' are generated per castaway (quoted nickname, first-N-words, individual words)
#' and validated in a single batch API call. First valid match wins.
#'
#' No-break spaces (U+00A0) are normalized per-name during candidate generation
#' only; the original name is preserved as the lookup key so that callers using
#' names directly from `season_picks` can find the URL.
#'
#' Returns a named list of `castaway_name → CDN URL`.
#'
#' @param szn Season number
#' @export
fetch_castaway_image_urls <- function(szn) {
    fandom_api <- "https://survivor.fandom.com/api.php"
    szn <- as.integer(szn)

    castaways_in_season <- season_picks |>
        dplyr::filter(season == szn) |>
        dplyr::pull(castaway_name) |>
        unique()
    castaways_in_season <- castaways_in_season[!is.na(castaways_in_season)]
    # NOTE: do NOT normalize castaways_in_season here — original names must be
    # preserved as URL map keys so that formatters.R lookups match season_picks names.
    if (length(castaways_in_season) == 0) return(list())

    name_map  <- list()
    all_files <- character(0)

    for (orig_name in castaways_in_season) {
        # Normalize no-break space per-name for candidate generation only
        name <- gsub("\u00A0", " ", orig_name)

        candidates <- character(0)

        # Priority 0: known nickname mapping (e.g. 'Oscar Lusth' → "ozzy")
        # Pass season number to get season-specific nicknames for returning players
        # If a nickname exists, use ONLY that and skip all fallback logic
        mapped_nickname <- get_castaway_nickname(orig_name, season = szn)
        if (!is.null(mapped_nickname)) {
            candidates <- c(candidates, paste0("File:S", szn, "_", mapped_nickname, "_t.png"))
            # Also try without underscore for compound nicknames
            if (grepl("_", mapped_nickname)) {
                no_underscore <- gsub("_", "", mapped_nickname)
                candidates <- c(candidates, paste0("File:S", szn, "_", no_underscore, "_t.png"))
            }
            # Skip all other candidate generation - the YAML mapping is authoritative
        } else {
            # Only generate fallback candidates if NO nickname mapping exists

            # Priority 1: quoted nickname (e.g. 'Quintavius "Q" Burdette' → "q")
            if (grepl('"', name)) {
                m <- regmatches(name, regexpr('"([^"]+)"', name))
                if (length(m) > 0) {
                    nick <- tolower(gsub('"', "", m))
                    candidates <- c(candidates, paste0("File:S", szn, "_", nick, "_t.png"))
                }
            }

            # Clean the name to derive word tokens (same rules wiki applies to slugs)
            clean <- tolower(name)
            clean <- gsub("[.']", "", clean)          # remove periods and apostrophes
            clean <- gsub("-", "", clean)              # remove hyphens
            clean <- gsub("[^a-z0-9 ]", " ", clean)   # other special chars → space
            clean <- trimws(gsub("\\s+", " ", clean))
            words <- strsplit(clean, " ")[[1]]
            n     <- length(words)

            # Priority 2: first-N-words (underscore and concatenated, N=1..3)
            for (end in seq_len(min(n, 3))) {
                slug_u <- paste(words[seq_len(end)], collapse = "_")
                slug_c <- paste(words[seq_len(end)], collapse = "")
                candidates <- c(candidates, paste0("File:S", szn, "_", slug_u, "_t.png"))
                if (slug_c != slug_u) {
                    candidates <- c(candidates, paste0("File:S", szn, "_", slug_c, "_t.png"))
                }
            }

            # Priority 3: each individual word (catches e.g. "rob" from "Boston Rob Mariano")
            for (w in words) {
                candidates <- c(candidates, paste0("File:S", szn, "_", w, "_t.png"))
            }

            # Priority 4: first name + last initial (e.g. "kim_j" for Kim Johnson)
            if (n >= 2) {
                first <- words[1]
                last_initial <- substr(words[n], 1, 1)
                candidates <- c(candidates, paste0("File:S", szn, "_", first, "_", last_initial, "_t.png"))
                # Also try without underscore
                candidates <- c(candidates, paste0("File:S", szn, "_", first, last_initial, "_t.png"))

                # Also try last name only (some contestants known by last name)
                last <- words[n]
                candidates <- c(candidates, paste0("File:S", szn, "_", last, "_t.png"))
            }
        }

        for (f in unique(candidates)) {
            norm_f <- gsub(" ", "_", f)
            if (is.null(name_map[[norm_f]])) {
                name_map[[norm_f]] <- orig_name   # preserve original name as key
                all_files <- c(all_files, f)
            }
        }
    }

    # Batch query in chunks of 40 to stay under the API 50-title limit
    urls <- list()
    chunks <- split(all_files, ceiling(seq_along(all_files) / 40))
    for (chunk in chunks) {
        result <- .fandom_imageinfo(fandom_api, chunk)
        if (is.null(result)) next
        for (page in result$query$pages) {
            if (is.null(page$imageinfo) || length(page$imageinfo) == 0) next
            norm_title <- gsub(" ", "_", page$title)
            castaway   <- name_map[[norm_title]]
            if (!is.null(castaway) && is.null(urls[[castaway]])) {
                urls[[castaway]] <- page$imageinfo[[1]]$url
            }
        }
    }
    urls
}


#' Get Castaway Image URLs for a Season (Cached)
#'
#' Returns castaway thumbnail image URLs from a package-level cache if
#' available, otherwise calls [fetch_castaway_image_urls()] and caches the
#' result for future calls within the session.
#'
#' @param szn Season number
#' @export
get_castaway_image_urls <- function(szn) {
    szn_char <- as.character(szn)
    if (exists(szn_char, envir = .castaway_cache, inherits = FALSE)) {
        return(get(szn_char, envir = .castaway_cache))
    }
    urls <- fetch_castaway_image_urls(szn)
    assign(szn_char, urls, envir = .castaway_cache)
    urls
}


#' Fetch and Cache Castaway Image URLs for All Seasons in Parallel
#'
#' Uses `furrr::future_map` with a multisession plan to concurrently fetch
#' castaway image URLs for every season, then merges into a single named list
#' suitable for the "All Seasons" table view.
#'
#' @return Named list of castaway_name -> CDN URL across all seasons
#' @export
fetch_all_seasons_image_urls <- function() {
    seasons <- sort(unique(season_picks$season[!is.na(season_picks$season)]))

    uncached <- seasons[!vapply(
        as.character(seasons),
        function(s) exists(s, envir = .castaway_cache, inherits = FALSE),
        logical(1)
    )]

    if (length(uncached) > 0) {
        n_workers <- round(future::availableCores()*.8) # use 80% of the systems cores
        oplan <- future::plan("multisession", workers = n_workers)
        on.exit(future::plan(oplan), add = TRUE)

        sp <- season_picks  # capture for worker export
        results <- furrr::future_map(
            uncached,
            fetch_castaway_image_urls,
            .options = furrr::furrr_options(
                seed = NULL,
                globals = list(
                    season_picks = sp,
                    get_castaway_nickname = get_castaway_nickname,
                    .fandom_imageinfo = .fandom_imageinfo,
                    .castaway_cache = .castaway_cache
                )
            )
        )

        for (i in seq_along(uncached)) {
            assign(as.character(uncached[[i]]), results[[i]], envir = .castaway_cache)
        }
    }

    # Merge all seasons from cache into one flat named list
    merged <- list()
    for (s in as.character(seasons)) {
        merged <- c(merged, get(s, envir = .castaway_cache, inherits = FALSE))
    }
    merged
}







