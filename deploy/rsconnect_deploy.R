

rsconnect::setAccountInfo(
    name = Sys.getenv("RS_CONNECT_ACCOUNT_NAME"), 
    token = Sys.getenv("RS_CONNECT_TOKEN"), 
    secret = Sys.getenv("RS_CONNECT_SECRET")
)
rsconnect::deployApp()
