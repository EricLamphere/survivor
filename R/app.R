

#' Run Survivor App
#' 
#' Starts the survivor app
#' 
#' @export
survivor_app <- function() {
    shiny::shinyApp(
        ui = ui, 
        server = server
    )
}
