# Survivor Pool

Survivor pool application for choosing survivors, calculating winnings, and looking at historical records

The app can be found [here](https://ericlamphere.shinyapps.io/survivor/) - Enjoy!

![](www/jeff-shocked.gif)

## Getting Started

There are a couple things needed to run the code in this repo:

1.  Grant access to a googlesheet. You can do this in a few ways
    1.  **Local & GH deployment** (best method): Set up a service account and create a json key with access to the googlesheets API. stored in `.secrets/gcp-service-account.json`
    2.  **Local deployment only**: Follow the instructions [here](https://cemyilmaz185.medium.com/how-to-setup-non-interactive-authentication-with-googlesheets4-package-on-r-a0baa5ff8ab0) to cache an auth token and use that every time.
        -   To use this authentication method, pass `NULL` to the `gs_auth` `path` parameter
    3.  **No security**: Make your googlesheet public
        -   To avoid authenticating with googlesheets, pass `TRUE` to the `gs_auth` `deauth_mode` parameter
2.  Add data to your googlesheet with the following tabs and fields:
    1.  **castaways**
        -   `season` - Season number
        -   `castaway_id` - Unique name for castaway
        -   `castaway_name` - Full name of castaway
        -   `castaway_day` - Day castaway was voted out
        -   `castaway_tie_breaker` - Tie breaker for castaways voted out on the same day. e.g. if castaway_A was voted out first, then castaway_B, the tie breaker field would be 1 for castaway_B and 2 for castaway_A
    2.  **picks**
        -   `season` - Season number
        -   `participant_id` - Unique name for participant
        -   `castaway_id` - Unique name for castaway
        -   `picking_order` - If needed for the first season, hard code picking order. Otherwise, picking order is determined automatically
        -   `participant_has_paid` - Boolean, whether or not the participant has paid at the end of the season
    3.  **season**
        -   `season` - Season number
        -   `cost_per_day` - Cost per day difference (see Rules in app)
        -   `sole_survivor_bonus` - Sole survivor bonus (see Rules in app)
    4.  **participants**
        -   `participant_id` - Unique name for participant
        -   `participant_first` - Participant first name
        -   `participant_last` - Participant last name

## How to Update Data

1.  When a survivor is voted out, update the relevant fields in the [survivor google sheet](https://docs.google.com/spreadsheets/d/1-lTGtzfeH4_Fq0hq6p5WsC760F3x0Ir4TbN7gLMu2UM/edit?gid=1597185590#gid=1597185590)
2.  Click the "Refresh data" button

## Dev Resources

-   [survivoR Package](https://github.com/doehm/survivoR)
-   [Shiny Tables](https://clarewest.github.io/blog/post/making-tables-shiny/)
-   [Mastering Shiny - Packages](https://mastering-shiny.org/scaling-packaging.html#extra-steps)
-   [Glyphicons](https://getbootstrap.com/docs/3.3/components/#glyphicons)
-   [Shiny Dashboard](https://rstudio.github.io/shinydashboard/structure.html)
