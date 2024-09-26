


# LIBRARIES ----
library(dplyr)
library(purrr)
library(ezextras)


# FUNCTIONS ----
process_config <- function(config, people) {
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
                castaway_finish_inverse_placement = .x$finish$inverse_placement %||% NA_integer_
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


# EXEC ----
config_path <- "data-raw/configs/seasons"
castaway_files <- list.files(config_path, recursive = TRUE)

pool_party <- yaml::read_yaml("data-raw/configs/pool_party.yml")
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
    ~ process_config(
        config = yaml::read_yaml(file.path(config_path, .x)),
        people = people_lookup
    )
) %>%
    dplyr::arrange(dplyr::desc(season)) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
        eliminated = dplyr::if_else(!is.na(castaway_finish_day), TRUE, FALSE),
        castaway_finish_placement = 1 + (dplyr::n() - castaway_finish_inverse_placement),
        sole_survivor = dplyr::if_else(castaway_finish_placement == 1, TRUE, FALSE)
    ) %>%
    dplyr::ungroup()


# WHEN THIS TABLE IS UPDATED: update the data.R doc string
usethis::use_data(season_picks, overwrite = TRUE)
# sinew::makeOxygen(season_picks)
