
#' App Server
#' 
#' Server for the survivor app
#' 
#' @param input List of inputs
#' @param output List of outputs
server <- function(input, output) {
    
    season_input <- shiny::reactive({
        input$season
    })
    
    output$formatted_picks_table <- formattable::renderFormattable({
        format_picks_table(szn = season_input())
    })
}
