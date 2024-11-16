

cli::cli_alert_info("Authenticating with shinyapps.io...")
rsconnect::setAccountInfo(
    name = Sys.getenv("SHINYAPPS_ACCOUNT_NAME"), 
    token = Sys.getenv("SHINYAPPS_TOKEN"),
    secret = Sys.getenv("SHINYAPPS_SECRET")
)


cli::cli_alert_info("Authenticating with shinyapps.io...")
rsconnect::deployApp(
    appName = "survivor",
    appDir = "./"
)