

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
                    "Welcome! This site is still in development so bear with me as I get things up and running. 
                    The plan is to use this site as a one stop shop for everything related to the survivor pool.
                    
                    "
                ),
                shiny::br(),
                shiny::h2("Rules"),
                shiny::tags$li("Everyone picks a player at the end of the first episode"),
                shiny::tags$ul(
                    shiny::tags$li("All picks must be completed before the start of the second episode"),
                    shiny::tags$li("We use NFL draft rules, meaning the winner of the previous season picks last, etc."),
                    shiny::tags$ul(shiny::tags$li("Amendment #1: New people are placed at the bottom of the list after the previous seasons winner in a first come first serve fashion"))
                ),
                shiny::tags$li("Whosevers castaway lasts the longest wins"),
                shiny::tags$li("The loser pays $1 per day for each additional day the winner lasted compared to the player they chose"),
                shiny::tags$li("If the winners player is the sole survivor, then each loser must pay an additional $5 to the winner"),
                shiny::tags$li("If two players survivors are voted off the same day, the player who picked first in the current season will pick second in the following season"),
                shiny::br(),
                shiny::h2("Enjoy!"),
                shiny::p(
                    "Please let me know if you have any trouble with the website. I'll 
                    be adding features and refining the design over time to expand 
                    functionality and improve your experience"
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
                shiny::h2("Last Week on Survivor"),
                shiny::fluidRow(
                    shinydashboard::infoBoxOutput("last_voted_out_box"),
                    shinydashboard::infoBoxOutput("castaways_remaining_box"),
                    shinydashboard::infoBoxOutput("season_wiki_box")
                ),
                shiny::fluidRow(
                    shiny::tags$style(
                        '#standings :is(th, td) {padding: 0;}'
                    ),
                    shinydashboard::box(
                        title = "Standings", 
                        status = "primary", 
                        solidHeader=TRUE, 
                        shiny::div(
                            DT::dataTableOutput("formatted_picks_table"),
                            style = "width = 80%"
                        ),
                        width = "80%"
                    )
                )
            )
        )
    )
    
    shinydashboard::dashboardPage(header, sidebar, body)
}