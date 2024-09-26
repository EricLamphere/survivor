

#' Run Survivor App
#' 
#' Starts the survivor app
jeff <- function() {
    shiny::shinyApp(
        ui = ui, 
        server = server
    )  
}
