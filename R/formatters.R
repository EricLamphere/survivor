

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
#' Formats data for the UI picks table
#' 
#' @param szn Season number. Defaults to `default_season()`
#' @param picks_only Logical, whether or not to only show castaways that have 
#'  been picked by a contestant
#' 
#' @export
format_picks_table <- function(szn = default_season(), picks_only = FALSE) {
    picks_data <- .picks_table__base_data(szn = szn, picks_only = picks_only)
    
    formatted <-
        picks_data |>
        formattable::formattable(
            get_column_formats(picks_data)
        )
    
    formattable::as.datatable(
        formatted,
        escape = FALSE,
        # options reference: https://datatables.net/reference/option/
        options = list(
            scrollX = TRUE, 
            iDisplayLength = 20, # max number of castaways per season
            lengthMenu = c(10, 20, 50, 100)
        ),
        rownames = FALSE
    )
}


#' Base Data for Season Picks Table
#' 
#' Cleans up columns in season_picks table in preparation for formatting
#' 
#' @param szn Season number. Defaults to `default_season()`
#' @param picks_only Logical, whether or not to only show castaways that have 
#'  been picked by a contestant
.picks_table__base_data <- function(szn = default_season(), picks_only = FALSE) {
    picks <- get_season_picks(szn = szn, picked = picks_only)
    
    # data driven flags
    show_participant_fields <- any(!is.na(picks$participant_id))
    show_money_fields <- all(!is.na(
        dplyr::filter(picks, !is.na(participant_id)) |> 
            dplyr::pull(castaway_day)
    ))
    show_season_field <- szn == all_seasons_label()
    
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
                glue::glue("{emoji::emoji('tada')} {castaway_name} {emoji::emoji('tada')}"),
                castaway_name
            ),
            `Day Voted Out` = castaway_day,
            `Castaway Rank` = scales::ordinal(castaway_rank)
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
    
    if (!show_season_field) {
        display_fields_added <- 
            display_fields_added |> 
            dplyr::select(-Season)
    }
    
    
    # clean and format
    display_fields_added |>
        dplyr::mutate(
            dplyr::across(
                dplyr::everything(),
                ~ dplyr::if_else(is.na(.x), "-", as.character(.x))
            )
        ) |>
        dplyr::select(-dplyr::any_of(colnames(season_picks))) # remove non-display columns
}




format_winners_data <- function() {
    winners <- .winners_widgets__base_data()
    
    purrr::map(
        split(winners, ~participant_id),
        ~ .create_winner_tile(.x)
    )
}


.create_winner_tile <- function(winner_row) {
    shinydashboard::infoBox(
        title = winner_row$participant_id,
        value = winner_row$participant_name,
        subtitle = glue::glue(
            "
            {winner_row$n_seasons_won} seasons won: {winner_row$`Seasons Won`}
            "
        )
    )
}


#' Format Winners Data
#' 
#' Gets table of winners and summarizes it for use by `format_winners_data`
.winners_widgets__base_data <- function() {
    winners <- get_pool_winners_table()
    
    winners_summarized <-
        winners |> 
        dplyr::group_by(participant_id, participant_full_name) |> 
        dplyr::summarise(
            n_seasons_won = dplyr::n(),
            `Money Won` = sum(participant_payment),
            `Seasons Won` = paste0(season, collapse = ", "),
            `Sole Survivor Picks` = sum(sole_survivor, na.rm = TRUE),
            .groups = "keep"
        ) |> 
        dplyr::ungroup() |> 
        dplyr::arrange(desc(n_seasons_won), desc(`Money Won`))
}






