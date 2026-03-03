
#' App Server
#' 
#' Server for the survivor app
#' 
#' @param input List of inputs
#' @param output List of outputs
#' 
#' @export
server <- function(input, output, session) {
    
    # Inputs ----
    season_input <- shiny::reactive({
        input$season
    })
    
    picks_only_input <- shiny::reactive({
        input$picks_only
    })
    
    # Outputs ----
    ## table & data refresh ----
    refresh_data <- shiny::reactive({
        counter <<- counter + 1
        if (counter > 1) {
            all_data <- gs_get_all_data()
            season_picks <<- create_season_picks(all_data)
            season_participants <<- create_season_participants(all_data)
        }
    }) |> 
        shiny::bindEvent(
            input$refresh_data,
            ignoreNULL = FALSE
        )
    
    output$season_logo <- shiny::renderUI({
        if (season_input() == all_seasons_label()) {
            logo_url <- default_survivor_logo()
        } else {
            szn <- as.character(force_season_number(season_input()))
            logo_url <- season_logo_urls[[szn]]
        }
        if (is.null(logo_url)) return(NULL)
        shiny::div(
            shiny::tags$img(src = logo_url, referrerpolicy = "no-referrer", style = "max-height: 150px;")
        )
    })

    output$season_label <- shiny::renderText({
        refresh_data()
        get_season_label(season_input())
    })
    
    output$formatted_picks_table <- DT::renderDataTable(
        expr = {
            refresh_data()
            ui_create_picks_table(
                szn = season_input(),
                picks_only = picks_only_input()
            )
        }
    )
    
    output$picking_order <- formattable::renderFormattable({
        refresh_data()
        ui_create_picking_order(szn = season_input())
    })
    
    ## bottom 3 tiles ----
    output$last_voted_out_box <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            "Voted out", 
            last_voted_out(szn = season_input()), 
            icon = shiny::icon("user"),
            color = "red"
        )
    })
    
    output$castaways_remaining_box <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            "Castaways remaining", 
            get_castaways_remaining(szn = season_input()), 
            icon = shiny::icon("fire"),
            color = "orange"
        )
    })
    
    output$season_wiki_box <- shinydashboard::renderInfoBox({
        szn <- season_input()
        if (szn == all_seasons_label()) {
            szn_label <- "Survivor"
        } else {
            szn_label <- glue::glue("Season {force_season_number(szn)}")
        }
        shinydashboard::infoBox(
            "Links",
            shiny::tagList(
                shiny::tags$a(
                    href = make_season_wiki_link(szn = szn),
                    target = "_blank",
                    glue::glue("Wiki: {szn_label}")
                ),
                shiny::tags$br(),
                shiny::tags$a(
                    href = make_season_fandom_link(szn = szn),
                    target = "_blank",
                    glue::glue("Fandom: {szn_label}")
                )
            ),
            icon = shiny::icon("link"),
            color = "aqua"
        )
    })
    
    ## pool winner & sole survivor tiles ----
    pool_winner_pick <- shiny::reactive({get_pool_winner(szn = season_input())})
    sole_survivor <- shiny::reactive({get_sole_survivor(szn = season_input())})
    season_started <- shiny::reactive({season_has_started(szn = season_input())})
    
    # enable refresh if the default season has no pool winner
    output$pool_winner_exists <- shiny::reactive(!is.null(pool_winner_pick()))
    shiny::outputOptions(output, 'pool_winner_exists', suspendWhenHidden = FALSE)
    
    # enable refresh if the default season has no sole survivor
    output$sole_survivor_exists <- shiny::reactive(!is.null(sole_survivor()))
    shiny::outputOptions(output, 'sole_survivor_exists', suspendWhenHidden = FALSE)
    
    # enable refresh if the default season has no sole survivor
    output$season_has_started <- shiny::reactive(season_started())
    shiny::outputOptions(output, 'season_has_started', suspendWhenHidden = FALSE)
    
    
    output$pool_winner_box <- shinydashboard::renderInfoBox({
        pool_winner_pick <- pool_winner_pick()
        winner_pick <- unname(pool_winner_pick)
        pool_winner <- names(pool_winner_pick)
        sole_survivor <- sole_survivor()
        if (!is.null(sole_survivor) && !is.null(pool_winner_pick) && pool_winner_pick == sole_survivor) {
            winner_pick <- glue::glue("{emoji::emoji('tada')} {winner_pick} {emoji::emoji('tada')} -- Sole survivor bonus!!")
        }
        shinydashboard::infoBox(
            glue::glue("Season {force_season_number(season_input())} pool winner"), 
            glue::glue("{emoji::emoji('star')} {pool_winner} {emoji::emoji('star')}"),
            subtitle = paste0(winner_pick),
            icon = shiny::icon("star"),
            color = "green",
            fill = TRUE
        )
    })
    
    output$sole_survivor_box <- shinydashboard::renderInfoBox({
        ss      <- sole_survivor()
        szn_num <- force_season_number(season_input())
        url <- if (!is.null(ss)) get_castaway_image_urls(szn_num)[[ss]] else NULL
        shinydashboard::infoBox(
            glue::glue("Season {szn_num} sole survivor"),
            shiny::tagList(
                if (!is.null(url)) shiny::tags$img(
                    src = url,
                    referrerpolicy = "no-referrer",
                    style = "height: 40px; vertical-align: middle; margin-right: 6px;"
                ),
                glue::glue("{emoji::emoji('tada')} {ss} {emoji::emoji('tada')}")
            ),
            icon = shiny::icon("fire"),
            color = "lime"
        )
    })
    
    ## total gain/loss plot ----
    output$gain_loss_plot <- shiny::renderPlot({ ui_create_gain_loss_plot() })
    
    ## season pots plot ----
    output$season_pots_plot <- shiny::renderPlot({ ui_create_season_pots_plot() })

    ## season navigation buttons ----
    output$season_nav_buttons <- shiny::renderUI({
        if (season_input() == all_seasons_label()) return(NULL)

        all_szns <- sort(list_seasons())
        idx <- which(all_szns == as.integer(season_input()))
        prev_szn <- if (idx > 1) all_szns[idx - 1] else NULL
        next_szn <- if (idx < length(all_szns)) all_szns[idx + 1] else NULL

        shiny::fluidRow(
            style = "margin-top: 12px;",
            shiny::column(6,
                if (!is.null(prev_szn)) shiny::actionButton(
                    "nav_prev",
                    label = shiny::tagList(shiny::icon("arrow-left"), get_season_label(prev_szn))
                )
            ),
            shiny::column(6, style = "text-align: right;",
                if (!is.null(next_szn)) shiny::actionButton(
                    "nav_next",
                    label = shiny::tagList(get_season_label(next_szn), shiny::icon("arrow-right"))
                )
            )
        )
    })

    shiny::observeEvent(input$nav_prev, {
        all_szns <- sort(list_seasons())
        idx <- which(all_szns == as.integer(season_input()))
        if (idx > 1) shiny::updateSelectInput(session, "season", selected = all_szns[idx - 1])
    })

    shiny::observeEvent(input$nav_next, {
        all_szns <- sort(list_seasons())
        idx <- which(all_szns == as.integer(season_input()))
        if (idx < length(all_szns)) shiny::updateSelectInput(session, "season", selected = all_szns[idx + 1])
    })
}
