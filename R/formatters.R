

# Season Picks Table ----

#' Format Season Picks Table
#' 
#' Formats data for the UI picks table
#' 
#' @param szn Season number. Defaults to `default_season()`
#' @param picks_only Logical, whether or not to only show castaways that have 
#'  been picked by a contestant
#' 
#' @export
ui_create_picks_table <- function(szn = default_season(), picks_only = FALSE) {
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




# Winners Top Tiles ----

#' Create Winners Tiles
#' 
#' Creates info boxes for each winner
#' 
#' @export
ui_create_winners_tiles <- function() {
    winners <- .winners_widgets__base_data()
    
    purrr::map(
        seq_len(nrow(winners)),
        ~ .create_winner_tile(winners[.x, ])
    )
}



.create_winner_tile <- function(winner_row) {
    shinydashboard::infoBox(
        color = "green",
        icon = shiny::icon("star"),
        title = shiny::tags$b(winner_row$participant_full_name),
        value = shiny::span(winner_row$`Money Won`, style="color:green"),
        subtitle = glue::glue(
            "
            {winner_row$n_seasons_won} {ifelse(winner_row$n_seasons_won != 1, 'seasons', 'season')} won ({winner_row$`Seasons Won`})
             with {winner_row$sole_survivor_picks} sole survivor {ifelse(winner_row$sole_survivor_picks != 1, 'picks', 'pick')}
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
            sole_survivor_picks = sum(sole_survivor, na.rm = TRUE),
            money_won = sum(participant_payment),
            seasons_won = paste0(rev(season), collapse = ", "),
            .groups = "keep"
        ) |> 
        dplyr::ungroup() |> 
        dplyr::arrange(desc(n_seasons_won), desc(money_won)) |> 
        dplyr::mutate(
            money_won_emoji = dplyr::case_when(
                dplyr::row_number() == 1 ~ emoji::emoji("money_bag"),
                dplyr::row_number() == 2 ~ emoji::emoji("money_with_wings"),
                dplyr::row_number() == 3 ~ emoji::emoji("dollar"),
                TRUE ~ emoji::emoji("coin")
            ),
            `Money Won` = paste0(scales::dollar(money_won), " ", money_won_emoji),
            `Seasons Won` = seasons_won,
            `Sole Survivor Picks` = dplyr::if_else(
                sole_survivor_picks > 0,
                glue::glue("{emoji::emoji('tada')} {sole_survivor_picks} {emoji::emoji('tada')}"),
                as.character(sole_survivor_picks)
            ),
        )
    
    winners_summarized
}

# Gain Loss Plot ----

#' Create Gain/Loss Plot
#' 
#' Creates bar plot of each participants net gains or losses
#' 
#' @export
ui_create_gain_loss_plot <- function() {
    totals <- .gain_loss__base_data()
    
    base_plot <- 
        totals |> 
        ggplot2::ggplot(
            ggplot2::aes(
                x = factor(participant_full_name, levels = rev(participant_full_name)),
                y = participant_payment,
                fill = bar_color
            )
        ) +
        ggplot2::geom_bar(stat = 'identity') +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(labels = scales::dollar) +
        ggplot2::scale_fill_identity() +
        ggplot2::geom_text(
            ggplot2::aes(
                label = scales::dollar(participant_payment),
                y = 0, 
                x = participant_full_name, 
                vjust = ifelse(participant_payment >= 0, 0.5, 0.5),
                hjust = ifelse(participant_payment >= 0, 1.15, -0.15)
            )
        ) + 
        ggplot2::labs(
            x = "Participant",
            y = "Net Gain/Loss",
            title = glue::glue("Net Gains & Losses {emoji::emoji('money_with_wings')}")
        )
    
    base_plot
}



.gain_loss__base_data <- function() {
    totals <- 
        get_season_picks(szn = all_seasons_label(), picked = TRUE) |> 
        dplyr::group_by(participant_id, participant_full_name) |> 
        dplyr::summarise(
            participant_payment = sum(
                dplyr::if_else(
                    as.integer(participant_rank) == 1, 
                    participant_payment,
                    -participant_payment
                ),
                na.rm = TRUE
            )
        ) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(
            bar_color = dplyr::case_when(
                participant_payment > 0 ~ "green",
                participant_payment == 0 ~ "black",
                participant_payment < 0 ~ "red"
            )
        ) |> 
        dplyr::arrange(dplyr::desc(participant_payment))
    
    totals
}


# Season Pots Plot ----

#' Create Season Pots Plot
#' 
#' Creates season pots plot to show the pot for each season
#' 
#' @export
ui_create_season_pots_plot <- function() {
    season_pots <- .season_pot__base_data()
    
    season_pots |> 
        ggplot2::ggplot(
            ggplot2::aes(
                x = grp,
                y = participant_payment,
                fill = "forestgreen"
            )
        ) +
        ggplot2::geom_bar(stat = 'identity') +
        ggplot2::scale_y_continuous(labels = scales::dollar) +
        ggplot2::scale_fill_manual(values = "forestgreen") +
        ggplot2::coord_flip() +
        ggplot2::geom_text(
            ggplot2::aes(
                label = scales::dollar(participant_payment),
                x = grp,
                hjust = -0.05
            )
        ) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::labs(
            x = "Season & Winner",
            y = "Pot $$$",
            title = glue::glue("Season Pots {emoji::emoji('honey_pot')}")
        )
}

.season_pot__base_data <- function() {
    get_pool_winners_table() |> 
        dplyr::mutate(
            grp = factor(
                glue::glue(
                    "Season {season} 
                    Winner: {participant_full_name}"
                ), 
                ordered = TRUE
            )
        )
}






