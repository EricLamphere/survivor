

#' App UI
#'
#' UI of the survivor app
#' 
#' @param input Shiny input
#' 
#' @export
ui <- function(input) {
    header <- shinydashboard::dashboardHeader(
        title = glue::glue("Survivor Pool"),
        titleWidth = 350
    )
    
    
    sidebar <- shinydashboard::dashboardSidebar(
        width = 350,
        shinydashboard::sidebarMenu(
            id = "sidebar_id",
            shinydashboard::menuItem(
                glue::glue("Welcome! {emoji::emoji('wave')}"), 
                tabName = "welcome"
            ),
            shinydashboard::menuItem(
                glue::glue("Standings {emoji::emoji('1st_place_medal')}"), 
                tabName = "standings"
            ),
            shiny::conditionalPanel(
                'input.sidebar_id == "standings"',
                shiny::selectInput(
                    "season", 
                    "Season", 
                    choices = c(all_seasons_label(), list_seasons()),
                    selected = default_season()
                ),
                shiny::textInput(
                    "search", 
                    "Search castaways", 
                    value = "Everyone"
                ),
                shiny::checkboxInput(
                    "picks_only",
                    shiny::tags$b("Only show our picks"),
                    value = FALSE
                )
            )
        )
    )
    
    body <- shinydashboard::dashboardBody(
        theme = bslib::bs_theme(bootswatch = "darkly"),
        
        shinydashboard::tabItems(
            # WELCOME
            shinydashboard::tabItem(
                "welcome",
                shiny::h2("Welcome!"),
                shiny::p(
                    "Welcome! This site is still in development, but once it's up and
                running it will be the one stop shop for picking castaways and
                keeping track of days lasted."
                ),
                shiny::br(),
                shiny::h2("Rules"),
                shiny::tags$li("Everyone picks a player at the end of the first episode"),
                shiny::tags$ul(
                    shiny::tags$li("All picks must be completed before the start of the second episode"),
                    shiny::tags$li("We use NFL draft rules, meaning the winner of the previous season picks last, etc."),
                    shiny::tags$ul(shiny::tags$li("Amendment #1: New people are placed at the bottom of the list after the previous seasons winner in a first come first serve fashion"))
                ),
                shiny::tags$li("Whosever person lasts the longest wins"),
                shiny::tags$li("The loser pays $1 per day for each additional day the winner lasted compared to the player they chose"),
                shiny::tags$li("If the winners player is the sole survivor, then each loser must pay an additional $5 to the winner"),
                shiny::tags$li("If two players survivors are voted off the same day, the player who picked first in the current season will pick second in the following season"),
                shiny::br(),
                shiny::h2("Thank you!"),
                shiny::span(
                    "Please let me know if you have any trouble with the website. I'll 
                be adding features and refining the design over time to expand 
                functionality and improve your experience",
                    style = "font-family: 'times'; font-si20pt"
                ),
                shiny::div(
                    shiny::img(
                        src = "jeff-probst-surprised.gif",
                        align = "center",
                        height = "450px",
                        width = "600px"
                    ),
                    style = "text-align: center;"
                )
            ),
            
            # PICKS
            shinydashboard::tabItem(
                "standings",
                shiny::fluidRow(
                    shinydashboard::infoBoxOutput("last_voted_out_box"),
                    shinydashboard::infoBoxOutput("castaways_remaining_box")
                ),
                shiny::h2("Standings"),
                formattable::formattableOutput("formatted_picks_table", width = "900px")
            )
        )
    )
    
    shinydashboard::dashboardPage(header, sidebar, body)
}