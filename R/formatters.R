

#' Format Season Picks Table
#' 
#' Selects, renames, and formats relevant columns for the app
#' 
#' @param szn Season number. Defaults to `default_season()`
#' @param picks_only Logical, whether or not to only show castaways that have 
#'  been picked by a contestant
#' @param search Character, text used to search the names of castaways
#' 
#' @export
format_picks_table <- function(szn = default_season(), picks_only = FALSE, search = "Everyone") {
    picks <- get_season_picks(szn = szn, picked = picks_only)
    
    display_fields_added <- 
        picks %>%
        dplyr::mutate(
            person_last = dplyr::case_when(
                person_last == "last_name_unknown" ~ NA_character_,
                TRUE ~ person_last
            ),
            Season = season,
            Peeps = ifelse(
                !is.na(person_winner) & person_winner,
                glue::glue("{emoji::emoji('star')} {trimws(person_first %&% ' ' %&% person_last)} {emoji::emoji('star')}"),
                trimws(person_first %&% " " %&% person_last)
            ),
            `Pool Rank` = scales::ordinal(person_rank),
            Castaway = ifelse(
                !is.na(sole_survivor) & sole_survivor, 
                glue::glue("{emoji::emoji('star')} {castaway_name} {emoji::emoji('star')}"),
                castaway_name
            ),
            `Day Voted Out` = castaway_finish_day,
            `Castaway Rank` = scales::ordinal(castaway_finish_placement),
            `Sole Survivor` = dplyr::case_when(
                sole_survivor ~ glue::glue("{emoji::emoji('tada')} WINNER {emoji::emoji('tada')}"),
                !is.na(`Day Voted Out`) ~ glue::glue("{emoji::emoji('x')}"),
                TRUE ~ NA_character_
            )
        )
    
    tmp_filt_picks_only <- dplyr::filter(display_fields_added, !is.na(person_id))
    # if all picked castaways have been voted out (or won), show pricing field
    # next to the participants name
    if (all(!is.na(tmp_filt_picks_only$castaway_finish_day))) {
        display_fields_added <-
            display_fields_added %>%
            dplyr::mutate(
                `What You Owe` = scales::dollar(person_payment)
            ) %>%
            dplyr::select(Season, Peeps, `What You Owe`, dplyr::everything())
    }
    
    if (search != "Everyone") {
        display_fields_added <-
            display_fields_added %>%
            dplyr::filter(Castaway %~% search)
    }
    
    final_cleaning <-
        display_fields_added %>%
        dplyr::mutate(
            dplyr::across(
                dplyr::everything(),
                ~ dplyr::if_else(is.na(.x), "-", as.character(.x))
            )
        ) %>%
        dplyr::select(-dplyr::any_of(colnames(season_picks))) # remove non-display columns
    
    formatted <-
        final_cleaning %>%
        formattable::formattable(
            list(
                Peeps = picks_formatter(`Pool Rank` == "1st"),
                `Pool Rank` = picks_formatter(`Pool Rank` == "1st"),
                `What You Owe` = picks_formatter(`Pool Rank` == "1st"),
                Castaway = formattable::formatter(
                    "span", 
                    style = ~ formattable::style(
                        color = dplyr::case_when(
                            `Castaway Rank` == "1st" ~ "green",
                            `Day Voted Out` != "-" ~ "red",
                            TRUE ~ "black"
                        )
                        # "text-decoration" = ifelse(x %in% eliminated$cast, "line-through", NA)
                    )
                ),
                `Castaway Rank` = picks_formatter(`Castaway Rank` == "1st"),
                `Sole Survivor` = picks_formatter(`Castaway Rank` == "1st")
            )
        )
    
    formatted
}



#' Format Picks Table Cells
#' 
#' Formats cells in the picks table given the row meets certain conditions. For
#' example, text is green if there is a winner
#' 
#' @param condition The condition to be evaluated to determine which color to use
#' @param color_yes If the condition evaluates to TRUE, use this color
#' @param color_no If the condition evaluates to FALSE, use this color
#' 
#' @export
picks_formatter <- function(condition, color_yes = "green", color_no = "black") {
    condition <- substitute(condition)
    formattable::formatter(
        "span", 
        style = ~ formattable::style(
            color = dplyr::if_else(eval(condition), "green", "black")
            # "text-decoration" = ifelse(x %in% eliminated$cast, "line-through", NA)
        )
    )
}




