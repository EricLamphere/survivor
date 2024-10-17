




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
                    selected = default_season(), 
                    # selector looks better with this set to TRUE, but works better on mobile with FALSE
                    selectize = FALSE 
                ),
                shiny::checkboxInput(
                    "picks_only",
                    shiny::tags$b("Only show our picks"),
                    value = FALSE
                )
            ),
            shinydashboard::menuItem(
                glue::glue("Winners Club {emoji::emoji('star')}"), 
                tabName = "winners"
            )
        )
    )
    
    body <- shinydashboard::dashboardBody(
        theme = bslib::bs_theme(bootswatch = "darkly"),
        # shinyWidgets::setBackgroundImage(
        #     src = "survivor-logo.png",
        #     shinydashboard = TRUE
        # ),
        
        shinydashboard::tabItems(
            # WELCOME ----
            shinydashboard::tabItem(
                "welcome",
                shiny::div(
                    shiny::img(
                        src = "survivor-logo.png",
                        align = "right",
                        height = "35%",
                        width = "35%"
                    ),
                    style = "display: inline-block; position:absolute; right:90px; size: relative"
                ),
                shiny::br(),
                shiny::h2("Welcome!"),
                shiny::column(
                    9,
                    shiny::p(glue::glue(
                        "This is the site for all things related to our survivor pool. Read the rules below, pick your castaway, and win some {paste0(rep(emoji::emoji('money_with_wings'), 3), collapse = '')}!"
                    ))
                ),
                shiny::column(3, ""),
                shiny::br(),
                shiny::br(),
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
                shiny::h2("Layout"),
                shiny::p("You should see a little hamburger menu in the top left. Click on that menu and a side panel 
                         should appear with a few different tabs you can click on. See below for a short description 
                         on what you can expect from each tab"),
                shiny::h4("Welcome"),
                shiny::tags$li("The welcome page for the site (what you're reading right now)"),
                shiny::tags$li("Rules for the pool - read these!"),
                shiny::h4("Standings"),
                shiny::tags$li("Tiles with information relevant to the last episode and a link to the current seasons wiki page"),
                shiny::tags$li("A searchable table with all time up to date standings"),
                shiny::tags$ul(
                    shiny::tags$li("Includes all of our picks and how much you owe (or won) at the end of the season"),
                    shiny::tags$li("Full historical data back through season 1")
                ),
                shiny::h4("More to come..."),
                shiny::p("Here are a few things I'm planning on adding that haven't made it in yet"),
                shiny::tags$li("A new tab showing some stats on participants and how we've done over time"),
                shiny::tags$ul(
                    shiny::tags$li("Winners board"),
                    shiny::tags$li("Cash payout by season"),
                    shiny::tags$li("etc."),
                ),
                shiny::tags$li("Pictures of castaways in the current season"),
                shiny::tags$li("A system for picking castaways"),
                shiny::br(),
                shiny::h2("Developer Notes"),
                shiny::p(
                    "Please let me know if you have any trouble with the website. I'll 
                    be adding features and refining the design over time to expand 
                    functionality and improve your experience. Here are a few things to
                    keep in mind if you notice any funkiness while using the site:"
                ),
                shiny::tags$li("This site was built on completely open source code, so everything was free.
                               The server I'm using is free too, and everything free comes with some limitations:"),
                shiny::tags$ul(
                    shiny::tags$li("The server only has 25 hours of uptime per month and will go down
                                   If we exceed that limit. If that happens, I can send updates manually from my 
                                   local version if needed"),
                    shiny::tags$li("The server has memory and CPU usage limits which restricts what we can
                                   do with the app (e.g. storing pictures of castaways for more than 1 season)"),
                ),
                shiny::tags$li("I use Google Sheets for storing data about castaways and picks. 
                               In order for the site to stay up to date with the most recent data, I need
                               to use the Google Sheets API to query data at load time. This API is fairly stable, 
                               but it's easy to throttle and sometimes fails a few times before succeeding. 
                               If you're noticing slow load times, that's probably why"),
                shiny::br(),
                shiny::h2("Enjoy!"),
                shiny::div(
                    shiny::img(
                        src = "jeff-probst-surprised.gif",
                        align = "center",
                        height = "80%",
                        width = "80%"
                    ),
                    style = "text-align: center;"
                )
            ),
            
            # PICKS ----
            shinydashboard::tabItem(
                "standings",
                shiny::h1(shiny::textOutput("season_label")),
                shiny::h3("Last Week on Survivor"),
                shiny::fluidRow(
                    shiny::conditionalPanel(
                        condition = 'output.pool_winner_exists',
                        shinydashboard::infoBoxOutput("pool_winner_box", width = 6)
                    ),
                    shiny::conditionalPanel(
                        condition = 'output.sole_survivor_exists',
                        shinydashboard::infoBoxOutput("sole_survivor_box", width = 6)
                    )
                ),
                shiny::fluidRow(
                    shinydashboard::infoBoxOutput("last_voted_out_box"),
                    shinydashboard::infoBoxOutput("castaways_remaining_box"),
                    shinydashboard::infoBoxOutput("season_wiki_box")
                ),
                shiny::actionButton(inputId = "refresh_data", label = "Refresh data"),
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
            ),
            
            # WINNERS CLUB ----
            shinydashboard::tabItem(
                "winners",
                shiny::p("coming soon...")
            )
        )
    )
    
    shinydashboard::dashboardPage(header, sidebar, body)
}