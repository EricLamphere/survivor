

#' Run Survivor App
#' 
#' Starts the survivor app
#' 
#' @export
run_app <- function() {
    shiny::shinyApp(
        ui = ui, 
        server = server
    )  
}
