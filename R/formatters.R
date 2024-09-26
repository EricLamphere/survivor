

#' Format Season Picks Table
#' 
#' Selects, renames, and formats relevant columns for the app
#' 
#' @param szn Season number. Defaults to `default_season()`
format_picks_table <- function(szn = default_season()) {
    picks <- get_season_picks(szn = szn)
    
    processed <- 
        picks %>%
        dplyr::mutate(
            person_last = dplyr::case_when(
                person_last == "last_name_unknown" ~ NA_character_,
                TRUE ~ person_last
            )
        ) %>%
        dplyr::transmute(
            Peeps = trimws(person_first %&% " " %&% person_last),
            Castaway = castaway_name,
            `Day Voted Out` = castaway_finish_day,
            Rank = scales::ordinal(castaway_finish_placement),
            `Sole Survivor` = dplyr::case_when(
                sole_survivor ~ glue::glue("{emoji::emoji('tada')}{emoji::emoji('star')} WINNER {emoji::emoji('star')}{emoji::emoji('tada')}"),
                !is.na(`Day Voted Out`) ~ glue::glue("{emoji::emoji('x')}"),
                TRUE ~ NA_character_
            )
        ) %>%
        dplyr::mutate(
            dplyr::across(
                dplyr::everything(),
                ~ dplyr::if_else(is.na(.x), "-", as.character(.x))
            )
        )
    
    formatted <-
        processed %>%
        formattable::formattable(
            # list(
            #     Peeps = formattable::formatter("span", style = x ~ formattable::style(
            #         `background-color` = "gray",
            #         display = "block",
            #         padding = "0px"
            #     ))
            # )
        )
    
    formatted
}