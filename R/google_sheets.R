

# options(gargle_oauth_cache = ".secrets")


#' Authenticate With Google Sheets
#' 
#' @param cache Where the secret is stored
#' @param email Email to authenticate with
#' @param deauth_mode Whether or not to run only with [googlesheets4::gs4_deauth()]
gs_auth <- function(cache = ".secrets", email = "elampsart@gmail.com", deauth_mode = FALSE) {
    googlesheets4::gs4_deauth()
    
    if (!deauth_mode) {
        googlesheets4::gs4_auth(cache = cache, email = email)  
    }
}


#' Get Survivor Spreadsheet
#' 
#' Uses [googlesheets4::gs4_get()] to get nested data frame of sheets and tabs
gs_srvivor_spreadsheet <- function() {
    url <- "https://docs.google.com/spreadsheets/d/1-lTGtzfeH4_Fq0hq6p5WsC760F3x0Ir4TbN7gLMu2UM"
    googlesheets4::gs4_get(url)
}