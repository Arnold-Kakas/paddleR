#' Set Paddle Mode
#'
#' This function sets the mode for Paddle API requests, either "sandbox" or "live".
#'
#' @param mode A character string indicating the mode to set. Options are "sandbox" or "live". Defaults to "live".
#'
#' @returns The base URL for Paddle API requests based on the selected mode.
#' @export
#'
#' @examples
#' set_paddle_mode("sandbox")
set_paddle_mode <- function(mode = c("live", "sandbox")) {
  mode <- match.arg(mode)

  .paddle_env$mode <- mode
  .paddle_env$base_url <- switch(
    mode,
    "live"    = "https://api.paddle.com",
    "sandbox" = "https://sandbox-api.paddle.com"
  )

  invisible(.paddle_env$base_url)
}

#' Get Paddle Mode
#' This function retrieves the current mode set for Paddle API requests.
#' @returns The current mode for Paddle API requests, either "live" or "sandbox".
#'
#' @export
get_paddle_mode <- function() .paddle_env$mode

#' Get Paddle URL
#' This function retrieves the base URL for Paddle API requests based on the current mode.
#'
#' @returns The base URL for Paddle API requests.
#'
#' @export
get_paddle_url  <- function() .paddle_env$base_url
