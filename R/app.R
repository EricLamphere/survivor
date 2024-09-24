

#' Run Survivor App
#' 
#' Starts the survivor app
survivor <- function() {
    # Run the application 
    shinyApp(
        ui = ui, 
        server = server
    )  
}
