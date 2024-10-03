


# LIBRARIES ----
library(dplyr)
library(purrr)
library(ezextras)
library(survivoR)
library(snakecase)

# FUNCTIONS ----
#' Get Historical Castaway Data
#' 
#' Pulls historical data from the `survivoR` package and formats it into
#' the same format as this apps custom data
#' 
#' @param start_season First season we don't have custom data for
#' 
#' @importFrom survivoR castaways
#' 
#' @export
pull_historical_castaways <- function(start_season = 43) {
    survivoR::castaways |> 
        dplyr::filter(version == "US" & season <= start_season & !is.na(result_number)) |> 
        dplyr::group_by(season) |> 
        dplyr::transmute(
            season,
            castaway_id = snakecase::to_snake_case(castaway),
            castaway_name = full_name,
            castaway_day = day,
            castaway_rank = result_number,
            castaway_eliminated = TRUE,
            sole_survivor = result_number == 1
        ) |>
        dplyr::group_by(season, castaway_id) |> 
        dplyr::mutate(
            castaway_id = dplyr::case_when(
                dplyr::n() > 1 ~ snakecase::to_snake_case(castaway_name),
                TRUE ~ castaway_id
            )
        ) |> 
        dplyr::ungroup() |> 
        dplyr::arrange(desc(season), desc(castaway_rank))
}


# EXEC ----
historical_castaways <- pull_historical_castaways()
usethis::use_data(historical_castaways, overwrite = TRUE)
# sinew::makeOxygen(historical_data)

season_picks <- create_season_picks(
    using = "googlesheets",
    config_path = "data-raw/configs",
    augment_with_historical = FALSE
)
usethis::use_data(season_picks, overwrite = TRUE)
# sinew::makeOxygen(season_picks)
