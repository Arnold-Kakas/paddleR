.paddle_env <- new.env(parent = emptyenv())

# Initialize with live as default
.paddle_env$mode <- "live"
.paddle_env$base_url <- "https://api.paddle.com"
