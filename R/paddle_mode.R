#' Set Paddle Mode
#' This function sets the mode for Paddle API requests, either "sandbox" or "live".
#'
#' @param mode A character string indicating the mode to set. Options are "sandbox" or "live". Defaults to "live".
#'
#' @returns The base URL for Paddle API requests based on the selected mode.
#' @export
#'
#' @examplesIf paddle_has_token()
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
#' @examplesIf paddle_has_token()
#' get_paddle_mode()  # should return "live" by default
get_paddle_mode <- function() .paddle_env$mode

#' Get Paddle API Key Based on Current Mode
#'
#' Resolves the Paddle API key based on environment variables and the selected mode (live or sandbox).
#'
#' @returns A character string representing the Paddle API key.
#' @export
#' @examplesIf paddle_has_token()
#' get_paddle_key()  # returns token for current mode

get_paddle_key <- function() {
  mode <- get_paddle_mode()
  if (!mode %in% c("live", "sandbox")) {
    stop("`paddle.mode` must be either 'live' or 'sandbox'")
  }

  if (!is.null(Sys.getenv("PADDLE_KEY"))) {
    return(Sys.getenv("PADDLE_KEY"))
  }

  key_env_var <- switch(mode,
                        live = "PADDLE_KEY_LIVE",
                        sandbox = "PADDLE_KEY_SANDBOX"
  )

  key <- Sys.getenv(key_env_var)
  if (identical(key, "")) {
    stop(sprintf("Environment variable `%s` must be set to authenticate with Paddle", key_env_var))
  }

  return(key)
}


#' Get Paddle URL
#' This function retrieves the base URL for Paddle API requests based on the current mode.
#'
#' @returns The base URL for Paddle API requests.
#'
#' @export
#' @examplesIf paddle_has_token()
#' get_paddle_url()  # returns token for current mode
get_paddle_url  <- function() .paddle_env$base_url

#' Check if Paddle API Token is Available
#' This function checks whether a Paddle API key is available in the environment,
#' either as `PADDLE_KEY` or `PADDLE_KEY_LIVE` / `PADDLE_KEY_SANDBOX`.
#'
#' @returns Logical `TRUE` if a token is found, `FALSE` otherwise.
#' @keywords internal
#' @export
#' @examples paddle_has_token()
paddle_has_token <- function() {
  nzchar(Sys.getenv("PADDLE_KEY")) ||
    nzchar(Sys.getenv("PADDLE_KEY_LIVE")) ||
    nzchar(Sys.getenv("PADDLE_KEY_SANDBOX"))
}
