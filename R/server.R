
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
    refresh_data <- shiny::reactive({
        counter <<- counter + 1
        if (counter > 1) {
            season_picks <<- create_season_picks("googlesheets")
        }
    }) %>%
        shiny::bindEvent(
            input$refresh_data,
            ignoreNULL = FALSE
        )
    
    output$formatted_picks_table <- DT::renderDataTable(
        expr = {
            refresh_data()
            format_picks_table(
                szn = season_input(),
                picks_only = picks_only_input()
            )  
        }
    )
    
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
                    "Season {ifelse(season_input() == all_seasons_label(), default_season(), season_input())}"
                )
            ), 
            icon = shiny::icon("link"),
            color = "aqua"
        )
    })
}
