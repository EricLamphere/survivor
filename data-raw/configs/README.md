

# DEPRECATED: Survivor Pool Config Files
NOTE: the config files have been deprecated in favor of google sheets (live updates, no deployments)

* `pool_party.yml` - ID, first name, and last name of all participants in the survivor pool
    * If a participants last name is unknown, set it to `last_name_unknown` so it's processed properly
* `seasons/*/castaways.yml` - This is the main config file that contains the season number, the picks for the season, and the details about the castaways including their ID, full name, the day they were voted out, and tie breaker fields:
    * The `tie_breaker` field acts as the tie breaker for people voted out on the same day during the regular season