

# Survivor Pool Config Files
* `pool_party.yml` - ID, first name, and last name of all participants in the survivor pool
    * If a participants last name is unknown, set it to `last_name_unknown` so it's processed properly
* `seasons/*/castaways.yml` - This is the main config file that contains the season number, the picks for the season, and the details about the castaways including their ID, full name, the day they were voted out, and tie breaker fields:
    * The `tie_breaker` field acts as the tie breaker for people voted out on the same day during the regular season
    * The `finalist` field serves two purposes:
        1. Acts as a tie breaker for finalists (1 = first, 2 = second, 3 = third, etc.)
        2. It's an easy way to determine who the finalists were in a season, rather than calculating it using the day they were voted out