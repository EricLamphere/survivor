
#' App Server
#' 
#' Server for the survivor app
#' 
#' @param input List of inputs
#' @param output List of outputs
#' 
#' @export
server <- function(input, output) {
    
    # Inputs ----
    season_input <- shiny::reactive({
        input$season
    })
    
    picks_only_input <- shiny::reactive({
        input$picks_only
    })
    
    # Outputs ----
    ## table & data refresh ----
    refresh_data <- shiny::reactive({
        counter <<- counter + 1
        if (counter > 1) {
            all_data <- gs_get_all_data()
            season_picks <<- create_season_picks(all_data)
            season_participants <<- create_season_participants(all_data)
        }
    }) |> 
        shiny::bindEvent(
            input$refresh_data,
            ignoreNULL = FALSE
        )
    
    output$season_label <- shiny::renderText({
        season <- season_input()
        if (season == all_seasons_label()) {
            season
        } else {
            paste0("Season ", season)
        }
    })
    
    output$formatted_picks_table <- DT::renderDataTable(
        expr = {
            refresh_data()
            ui_create_picks_table(
                szn = season_input(),
                picks_only = picks_only_input()
            )
        }
    )
    
    output$picking_order <- formattable::renderFormattable({
        refresh_data()
        ui_create_picking_order(szn = season_input())
    })
    
    ## bottom 3 tiles ----
    output$last_voted_out_box <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            "Voted out", 
            last_voted_out(szn = season_input()), 
            icon = shiny::icon("user"),
            color = "red"
        )
    })
    
    output$castaways_remaining_box <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            "Castaways remaining", 
            get_castaways_remaining(szn = season_input()), 
            icon = shiny::icon("fire"),
            color = "orange"
        )
    })
    
    output$season_wiki_box <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            "Wiki Link", 
            shiny::tags$a(
                href = make_season_wiki_link(szn = season_input()),
                glue::glue(
                    "Season {force_season_number(season_input())}"
                )
            ), 
            icon = shiny::icon("link"),
            color = "aqua"
        )
    })
    
    ## pool winner & sole survivor tiles ----
    pool_winner_pick <- shiny::reactive({get_pool_winner(szn = season_input())})
    sole_survivor <- shiny::reactive({get_sole_survivor(szn = season_input())})
    season_started <- shiny::reactive({season_has_started(szn = season_input())})
    
    # enable refresh if the default season has no pool winner
    output$pool_winner_exists <- shiny::reactive(!is.null(pool_winner_pick()))
    shiny::outputOptions(output, 'pool_winner_exists', suspendWhenHidden = FALSE)
    
    # enable refresh if the default season has no sole survivor
    output$sole_survivor_exists <- shiny::reactive(!is.null(sole_survivor()))
    shiny::outputOptions(output, 'sole_survivor_exists', suspendWhenHidden = FALSE)
    
    # enable refresh if the default season has no sole survivor
    output$season_has_started <- shiny::reactive(season_started())
    shiny::outputOptions(output, 'season_has_started', suspendWhenHidden = FALSE)
    
    
    output$pool_winner_box <- shinydashboard::renderInfoBox({
        pool_winner_pick <- pool_winner_pick()
        winner_pick <- unname(pool_winner_pick)
        pool_winner <- names(pool_winner_pick)
        sole_survivor <- sole_survivor()
        if (!is.null(sole_survivor) && !is.null(pool_winner_pick) && pool_winner_pick == sole_survivor) {
            winner_pick <- glue::glue("{emoji::emoji('tada')} {winner_pick} {emoji::emoji('tada')} -- Sole survivor bonus!!")
        }
        shinydashboard::infoBox(
            glue::glue("Season {force_season_number(season_input())} pool winner"), 
            glue::glue("{emoji::emoji('star')} {pool_winner} {emoji::emoji('star')}"),
            subtitle = paste0(winner_pick),
            icon = shiny::icon("star"),
            color = "green",
            fill = TRUE
        )
    })
    
    output$sole_survivor_box <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            glue::glue("Season {force_season_number(season_input())} sole survivor"), 
            glue::glue("{emoji::emoji('tada')} {sole_survivor()} {emoji::emoji('tada')}"),
            icon = shiny::icon("fire"),
            color = "lime"
        )
    })
    
    ## total gain/loss plot ----
    output$gain_loss_plot <- shiny::renderPlot({ ui_create_gain_loss_plot() })
    
    ## season pots plot ----
    output$season_pots_plot <- shiny::renderPlot({ ui_create_season_pots_plot() })
}
