

#' App UI
#'
#' UI of the survivor app
ui <- function(input) {
    header <- shinydashboard::dashboardHeader(title = glue::glue("Survivor Pool"),
                                              titleWidth = 350)
    
    
    sidebar <- shinydashboard::dashboardSidebar(
        width = 350,
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Welcome", tabName = "Welcome!"),
            shinydashboard::menuItem("Picks", tabName = "Picks")
        )
    )
    
    body <- shinydashboard::dashboardBody(
        theme = bslib::bs_theme(bootswatch = "darkly"),
        
        shinydashboard::tabItems(
            # WELCOME
            shinydashboard::tabItem(
                "Welcome!",
                shiny::p(
                    "Welcome! This site is still in development, but once it's up and
                running it will be the one stop shop for picking castaways and
                keeping track of days lasted."
                ),
                shiny::h2("Rules"),
                shiny::p(
                    "* Everyone picks a player at the end of the first episode
                    * All picks must be completed before the start of the second episode
                    * We use NFL draft rules, meaning the winner of the previous season picks last, etc.
                        * Amendment #1: New people are placed at the bottom of the list after the previous seasons winner in a first come first serve fashion
                * Whosever person lasts the longest wins
                * The loser pays $1 per day for each additional day the winner lasted compared to the player they chose
                * If the winners player is the sole survivor, then each loser must pay an additional $5 to the winner
                * If two players survivors are voted off the same day, the player who picked first in the current season will pick second in the following season"
                ),
                shiny::h2("Thank you!"),
                shiny::span(
                    "Please let me know if you have any trouble with the website. I'll 
                be adding features and refining the design over time to expand 
                functionality and improve your experience",
                    style = "font-family: 'times'; font-si20pt"
                ),
                shiny::div(
                    shiny::img(
                        src = "jeff-probst-rollercoaster.gif",
                        align = "center",
                        height = "900px",
                        width = "900px"
                    ),
                    style = "text-align: center;"
                )
            ),
            
            # PICKS
            shinydashboard::tabItem(
                "Picks",
                shiny::selectInput("season", "Season", choices = list_seasons()),
                shiny::h2("Score Board by Week"),
                formattable::formattableOutput("scoreboard")
            )
        )
    )
    
    shinydashboard::dashboardPage(header, sidebar, body)
}