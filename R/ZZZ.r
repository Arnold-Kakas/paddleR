# --------------------------------------------------
# Helper Functions
# --------------------------------------------------

#' Null coalescing operator
#'
#' Returns `a` if not NULL, otherwise returns `b`.
#'
#' @param a First value.
#' @param b Second value used if `a` is NULL.
#'
#' @return Either `a` or `b`.
#' @export
#' @name %||%
#' @rdname null_coalescing_operator
#' @usage a \%||\% b
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}


#' Handle Paddle API Response
#'
#' Parses and returns the response if successful, otherwise stops with an error message.
#'
#' @param resp Response object from `httr2::req_perform()`
#'
#' @return Parsed JSON content from response, or throws error if HTTP status is not 2xx.
#' @export
handle_paddle_response <- function(resp) {
  if (!httr2::resp_is_error(resp)) {
    return(httr2::resp_body_json(resp))
  }

  # Try to extract error message safely
  content <- try(httr2::resp_body_json(resp), silent = TRUE)

  #last_resp <- httr2::last_response() |> httr2::resp_body_json()

  msg <- if (inherits(content, "try-error")) {
    paste("Paddle API request failed with status", httr2::resp_status(resp))
  } else {
    paste0(
      "Paddle API error: ",
      content$error$detail %||% "Unknown error",
      "\nStatus code: ", httr2::resp_status(resp)
    )
  }

  stop(msg, call. = FALSE)
}


#' Make a POST request to Paddle API
#' @param link The API endpoint URL.
#' @param body The body of the request, typically a list to be converted to JSON.
#' @return Parsed JSON response from the API.

post <- function(link, body) {
    res <- httr2::request(link) |>
      httr2::req_auth_bearer_token(Sys.getenv("PADDLE_KEY")) |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    handle_paddle_response(res)
}


#' Make a POST request to Paddle API without body
#' @param link The API endpoint URL.
#' @param body The body of the request, typically a list to be converted to JSON.
#' @return Parsed JSON response from the API.

post_excl_body <- function(link) {
  res <- httr2::request(link) |>
    httr2::req_auth_bearer_token(Sys.getenv("PADDLE_KEY")) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  handle_paddle_response(res)
}

#' Make a GET request to Paddle API
#' @param link The API endpoint URL.
#' @return Parsed JSON response from the API.

get <- function(link) {
  res <- httr2::request(link) |>
    httr2::req_auth_bearer_token(Sys.getenv("PADDLE_KEY")) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  handle_paddle_response(res)
}

#' Make a DELETE request to Paddle API
#' @param link The API endpoint URL.
#' @param body The body of the request, typically a list to be converted to JSON.
#' @return Parsed JSON response from the API.

update <- function(link, body) {
  res <- httr2::request(link) |>
    httr2::req_auth_bearer_token(Sys.getenv("PADDLE_KEY")) |>
    httr2::req_body_json(body) |>
    httr2::req_method("PATCH") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  handle_paddle_response(res)
}
