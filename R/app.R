

#' Run Survivor App
#' 
#' Starts the survivor app
run <- function() {
    shiny::shinyApp(
        ui = ui, 
        server = server
    )  
}
