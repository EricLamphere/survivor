

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

source("src/ui.R")
source("src/server.R")

# Run the application 
shinyApp(ui = ui, server = server)
