

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
            color = dplyr::if_else(eval(condition), color_yes, color_no)
        )
    )
}


#' Get Column Formats
#' 
#' Column formats for formattable table
#' 
#' @param data Picks data that's ready for formatting
get_column_formats <- function(data) {
    columns <- colnames(data)
    participant_formats <- list(
        Participant = picks_formatter(`Pool Rank` == "1st"),
        `Pool Rank` = picks_formatter(`Pool Rank` == "1st"),
        `$$$` = picks_formatter(`Pool Rank` == "1st", color_no = "red")
    )
    castaway_formats <- list(
        Castaway = formattable::formatter(
            "span", 
            style = ~ formattable::style(
                color = dplyr::case_when(
                    `Castaway Rank` == "1st" ~ "green",
                    `Day Voted Out` != "-" ~ "red",
                    TRUE ~ "black"
                )
            )
        ),
        `Castaway Rank` = picks_formatter(`Castaway Rank` == "1st"),
        `Sole Survivor` = picks_formatter(`Castaway Rank` == "1st")
    )
    
    if ("Pool Rank" %in% columns) {
        all_formats <- append(participant_formats, castaway_formats)
    } else {
        all_formats <- castaway_formats
    }
    
    all_formats
}


#' Format Season Picks Table
#' 
#' Selects, renames, and formats relevant columns for the app
#' 
#' @param szn Season number. Defaults to `default_season()`
#' @param picks_only Logical, whether or not to only show castaways that have 
#'  been picked by a contestant
#' 
#' @export
format_picks_table <- function(szn = default_season(), picks_only = FALSE) {
    picks <- get_season_picks(szn = szn, picked = picks_only)
    
    # data driven flags
    show_participant_fields <- any(!is.na(picks$participant_id))
    show_money_fields <- all(!is.na(
        dplyr::filter(picks, !is.na(participant_id)) |> 
            dplyr::pull(castaway_finish_day)
    ))
    
    display_fields_added <- 
        picks |>
        dplyr::mutate(
            Season = season,
            Participant = ifelse(
                !is.na(participant_winner) & participant_winner,
                glue::glue("{emoji::emoji('star')} {participant_full_name} {emoji::emoji('star')}"),
                participant_full_name
            ),
            `Pool Rank` = scales::ordinal(participant_rank),
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
    
    
    # data driven filters
    if (show_money_fields) {
        display_fields_added <-
            display_fields_added |>
            dplyr::mutate(
                `$$$` = scales::dollar(participant_payment)
            ) |>
            dplyr::select(Season, Participant, `$$$`, dplyr::everything())
    }
    
    if (!show_participant_fields) {
        display_fields_added <-
            display_fields_added |> 
            dplyr::select(-dplyr::starts_with('participant_'), -Participant, -`Pool Rank`, -`$$$`)
    }
    
    
    # clean and format
    final_cleaning <-
        display_fields_added |>
        dplyr::mutate(
            dplyr::across(
                dplyr::everything(),
                ~ dplyr::if_else(is.na(.x), "-", as.character(.x))
            )
        ) |>
        dplyr::select(-dplyr::any_of(colnames(season_picks))) # remove non-display columns
    
    formatted <-
        final_cleaning |>
        formattable::formattable(
            get_column_formats(final_cleaning)
        )
    
    formattable::as.datatable(
        formatted,
        escape = FALSE,
        options = list(
            scrollX = TRUE, 
            iDisplayLength = 25
        ),
        rownames = FALSE
    )
}






