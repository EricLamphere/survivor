# Survivor Pool
Survivor pool application for choosing survivors, calculating winnings, and looking at historical records


## Rules
* Everyone picks a player at the end of the first episode
    * All picks must be completed before the start of the second episode
    * We use NFL draft rules, meaning the winner of the previous season picks last, etc.
        * Amendment #1: New people are placed at the bottom of the list after the previous seasons winner in a first come first serve fashion
* Whosever person lasts the longest wins
* The loser pays $1 per day for each additional day the winner lasted compared to the player they chose
* If the winners player is the sole survivor, then each loser must pay an additional $5 to the winner
* If two players survivors are voted off the same day, the player who picked first in the current season will pick second in the following season


## Update Data
1. When a survivor is voted out, update the relevant fields in the config file in `data-raw/configs/seasons/`
2. Run the `data-raw/process_configs.R` script manually

## Dev Resources
* [Mastering Shiny - Packages](https://mastering-shiny.org/scaling-packaging.html#extra-steps)
* [Another survivor app](https://github.com/ErichDenk/survivor/tree/main?tab=readme-ov-file)
* [Using data in R package](https://grasshoppermouse.github.io/posts/2017-10-18-put-your-data-in-an-r-package/)