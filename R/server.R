

#' App Server
#' 
#' Server for the survivor app
#' 
#' @param input List of inputs
#' @param output List of outputs
server <- function(input, output, session) {
    
    season_input <- reactive({
        input$season
    })
    
    formatted_table <- 
        lapply(1:weekInput(), weekly_score, x = picks) %>%
        reduce(left_join) %>%
        rowwise() %>%
        mutate(Score = sum(c_across(starts_with("epi_score"))),
               tot_remain = as.integer(tot_remain)) %>%
        ungroup() %>% # No longer rowwise
        mutate(mvpbonus = ifelse(MVP == winner, 30, 0),
               winneradd = if_else(str_detect(fullteam, 
                                              pattern = winner),30,0),
               secondadd = if_else(str_detect(fullteam, 
                                              pattern = second),20,0),
               thirdadd = if_else(str_detect(fullteam, 
                                             pattern = third),10,0),
               top3bonus = winneradd + secondadd + thirdadd) %>% 
        mutate(Score = as.integer(Score + top3bonus + mvpbonus)) %>%
        rename(`Remaining Survivors` = tot_remain,
               `MVP Bonus` = mvpbonus,
               `Top 3 Bonuses` = top3bonus) %>%
        mutate(Place = dense_rank(desc(Score))) %>%
        arrange(Place, Contestant) %>% 
        select(Place, Contestant, Score, MVP,
               starts_with("Pick"), ends_with("bonus"), `Remaining Survivors`) %>%
        formattable(list(MVP = elimformatter,
                         Pick2 = elimformatter,
                         Pick3 = elimformatter,
                         Pick4 = elimformatter,
                         Pick5 = elimformatter,
                         Name = formatter("span", style = x ~ style(font.style = "italic")),
                         Place = formatter("span", style = x ~ style(font.style = "bold"))))
    
    
    picks <- renderFormattable(picks_table)
    
    # Popular Picks Chart
    output$Popular <- renderPlot(popular)
}
