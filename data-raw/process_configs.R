


# LIBRARIES ----
library(dplyr)
library(purrr)
library(ezextras)


# FUNCTIONS ----

#' Process Season Config
#' 
#' Process season config into a data frame
#' 
#' @param config Season config file parsed using `yaml::read_yaml`
#' @param people Config file of people in the pool parsed using `yaml::read_yaml`
process_season_config <- function(config, people) {
    season <- config$season
    picks <- config$picks
    castaways <- config$castaways
    
    picks_tbl <- tibble::tibble(
        season = season,
        person_id = names(picks),
        castaway_id = unlist(picks, use.names = FALSE)
    ) %>%
    dplyr::left_join(
        people,
        by = "person_id"
    ) %>%
    dplyr::select(season, starts_with("person"), castaway_id)
    
    castaway_tbl <- 
        purrr::imap_dfr(
            castaways,
            ~ tibble::tibble(
                castaway_id = .y,
                castaway_name = .x$full_name,
                castaway_finish_day = .x$finish$day %||% NA_integer_,
                castaway_finish_tie_breaker = .x$finish$tie_breaker %||% NA_integer_,
                castaway_finish_finalist = .x$finish$finalist %||% NA_integer_
            )
        )
    
    combined <- 
        picks_tbl %>%
        dplyr::left_join(
            castaway_tbl,
            by = "castaway_id"
        ) %>%
        dplyr::select(season, dplyr::starts_with("person"), dplyr::starts_with("castaway"))
    
    not_picked <- 
        dplyr::filter(castaway_tbl, castaway_id %notin% unique(combined$castaway_id)) %>%
        dplyr::mutate(season = season)
    
    dplyr::bind_rows(combined, not_picked)
}


#' Process All Seasons
#' 
#' Process each seasons config file with `process_season_config` and bind
#' all seasons together
#' 
#' @param config_path Path to the config files, defaults to the right path
process_seasons <- function(config_path = "data-raw/configs") {
    seasons_config_path <- config_path %//% "seasons"
    castaway_files <- list.files(seasons_config_path, recursive = TRUE)
    
    pool_party <- yaml::read_yaml(config_path %//% "pool_party.yml")
    people <- pool_party$people
    people_lookup <- 
        purrr::imap_dfr(
            people,
            ~ tibble::tibble(
                person_id = .y,
                person_first = .x$first,
                person_last = .x$last
            )
        )
    
    season_picks <- purrr::map_dfr(
        castaway_files, 
        ~ process_season_config(
            config = yaml::read_yaml(file.path(seasons_config_path , .x)),
            people = people_lookup
        )
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
    seasons_tbl %>%
        dplyr::arrange(dplyr::desc(season)) %>%
        dplyr::group_by(season) %>%
        dplyr::mutate(
            castaway_day_with_tie_breaker = dplyr::if_else(is.na(castaway_finish_tie_breaker), castaway_finish_day, castaway_finish_day + 1/(1+castaway_finish_tie_breaker)),
            castaway_rank = dplyr::if_else(is.na(castaway_day_with_tie_breaker), NA, 1 + dplyr::n() - rank(castaway_day_with_tie_breaker)),
            castaway_eliminated = dplyr::if_else(!is.na(castaway_finish_day), TRUE, FALSE),
            castaway_finish_placement = dplyr::if_else(
                is.na(castaway_finish_finalist),
                castaway_rank,
                castaway_finish_finalist
            ),
            sole_survivor = dplyr::if_else(castaway_finish_placement == 1, TRUE, FALSE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(castaway_day_with_tie_breaker, castaway_finish_tie_breaker, castaway_rank, castaway_finish_finalist))
}

#' Calculate Payments & Rank
#' 
#' Calculates the payments and rank for the people in the pool party
#' 
#' @param seasons_tbl Seasons table obtained from `process_seasons` and 
#' processed by `calculate_castaway_fields`
#' @param cost_per_day Integer, number of dollars to multiply the day difference by
#' @param sole_survivor_bonus Integer, number of dollars to add to each persons cost
#' if one of the picks was the sole survivor
calculate_person_fields <- function(seasons_tbl, cost_per_day, sole_survivor_bonus) {
    picked <- dplyr::filter(seasons_tbl, !is.na(person_id))
    unpicked <- dplyr::filter(seasons_tbl, is.na(person_id))
    
    payments_applied <- 
        split(picked, ~season) %>%
        purrr::map_dfr(
            ~ .get_season_payments(
                picks = .x, 
                cost_per_day = cost_per_day, 
                sole_survivor_bonus = sole_survivor_bonus
            ) %>% .get_person_rank()
        )
    
    dplyr::bind_rows(
        payments_applied,
        unpicked
    )
}

.get_season_payments <- function(picks, cost_per_day, sole_survivor_bonus) {
    if (!all(picks$castaway_eliminated)) {
        return(dplyr::mutate(picks, person_payment = NA_integer_))
    }
    
    winner_n_days <- max(picks$castaway_finish_day)
    season_with_payments <-
        picks %>%
        dplyr::mutate(
            person_winner = castaway_finish_day == max(castaway_finish_day),
            person_payment = cost_per_day * (winner_n_days - castaway_finish_day)
        )
    
    if (any(picks$sole_survivor, na.rm = TRUE)) {
        season_with_payments <- dplyr::mutate(season_with_payments, person_payment = person_payment + sole_survivor_bonus)
    }
    
    season_with_payments <-
        season_with_payments %>%
        dplyr::mutate(
            person_payment = dplyr::if_else(
                person_winner,
                0,
                person_payment
            ),
            person_payment = dplyr::if_else(
                person_winner,
                sum(person_payment),
                person_payment
            )
        )
    
    season_with_payments
}

.get_person_rank <- function(picks) {
    n_people = nrow(picks)
    
    picks %>%
        dplyr::mutate(
            person_rank = dplyr::if_else(is.na(castaway_finish_placement), NA, 1 + n_people - rank(castaway_finish_day))
        )
}


#' Cleanup Season Picks Dataset
#' 
#' Cleanup the season picks dataset after processing has been applied. This 
#' includes removing unused fields and arranging columns and rows
#' 
#' @param season_picks Season picks table after processing is applied
cleanup_season_picks <- function(season_picks) {
    season_picks %>%
        dplyr::arrange(dplyr::desc(season)) %>%
        dplyr::select(
            season,
            dplyr::starts_with("person_"),
            dplyr::starts_with("castaway_"),
            dplyr::everything()
        )
}


#' Create Season Picks Data Frame
#' 
#' Main method for creating the season picks table with all transformations applied
#' 
#' @param config_path Path to the config files, defaults to the right path
#' @param cost_per_day Integer, number of dollars owed per day using the difference
#' in days compared to the person who's castaway lasted the longest
#' @param sole_survivor_bonus Integer, number of dollars to add to each persons 
#' payment if the winners pick was the sole survivor
create_season_picks <- function(config_path = "data-raw/configs", cost_per_day = 1, sole_survivor_bonus = 5) {
    seasons_tbl <- process_seasons(config_path = config_path)
    
    seasons_tbl %>%
        calculate_castaway_fields() %>%
        calculate_person_fields(
            cost_per_day = cost_per_day, 
            sole_survivor_bonus = sole_survivor_bonus
        ) %>%
        cleanup_season_picks()
}


# EXEC ----
season_picks <- create_season_picks(
    config_path = "data-raw/configs",
    cost_per_day = 1,
    sole_survivor_bonus = 5
)

# WHEN THIS TABLE IS UPDATED: update the data.R doc string
usethis::use_data(season_picks, overwrite = TRUE)
# sinew::makeOxygen(season_picks)
