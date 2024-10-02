

#' @title Season Picks
#' @description Picks across all seasons recorded in this repo
#' @format A data frame with 72 rows and 13 variables:
#' \describe{
#'   \item{\code{season}}{integer Season number}
#'   \item{\code{participant_id}}{character The ID for the participant in the pool}
#'   \item{\code{participant_first}}{character First name of the participant in the pool}
#'   \item{\code{participant_last}}{character Last name of the participant in the pool}
#'   \item{\code{participant_full_name}}{character First and last name of the participant in the pool}
#'   \item{\code{participant_winner}}{logical Whether or not the participant won the pool}
#'   \item{\code{participant_payment}}{integer The amount of money owed for the losers, or the amount of money won by the winner} 
#'   \item{\code{participant_rank}}{double Rank based on whos castaway lasted the longest} 
#'   \item{\code{castaway_id}}{character ID for the castaway}
#'   \item{\code{castaway_name}}{character Full name of the castaway}
#'   \item{\code{castaway_finish_day}}{integer The day the castaway was voted out}
#'   \item{\code{castaway_eliminated}}{logical Whether or not survivor has been eliminated}
#'   \item{\code{castaway_finish_placement}}{double Rank based on when castaway was voted out}
#'   \item{\code{sole_survivor}}{logical Whether or not the castaway was the sole survivor} 
#'}
#' @details Season configs processed into a table
"season_picks"


#' @title Historical Castaways
#' @description Castaways from earlier seasons before we started our pool
#' @format A data frame with 785 rows and 7 variables:
#' \describe{
#'   \item{\code{season}}{integer Season number}
#'   \item{\code{castaway_id}}{character ID for the castaway}
#'   \item{\code{castaway_name}}{character Full name of the castaway}
#'   \item{\code{castaway_finish_day}}{integer The day the castaway was voted out}
#'   \item{\code{castaway_eliminated}}{logical Whether or not survivor has been eliminated}
#'   \item{\code{castaway_finish_placement}}{double Rank based on when castaway was voted out}
#'   \item{\code{sole_survivor}}{logical Whether or not the castaway was the sole survivor} 
#'}
#' @details Season configs processed into a table
"historical_castaways"