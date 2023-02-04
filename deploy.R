rsconnect::setAccountInfo(
    name = Sys.getenv("name"),
    token = Sys.getenv("token"),
    secret = Sys.getenv("secret")
)

rsconnect::deployApp()