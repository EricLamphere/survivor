

#' Run Survivor App
#' 
#' Starts the survivor app
survivor <- function() {
    # Run the application 
    shiny::shinyApp(
        ui = ui, 
        server = server
    )  
}
