# --------------------------------------------------
# Helper Functions
# --------------------------------------------------
#' Handle Paddle API Response
#'
#' Parses and returns the response if successful, otherwise stops with an error message.
#'
#' @param resp Response object from `httr2::req_perform()`
#'
#' @returns Parsed JSON content from response, or throws error if HTTP status is not 2xx.
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
#' @returns Parsed JSON response from the API.

post <- function(link, body) {
    res <- httr2::request(link) |>
      httr2::req_auth_bearer_token(get_paddle_key()) |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    handle_paddle_response(res)
}


#' Make a POST request to Paddle API without body
#' @param link The API endpoint URL.
#' @returns Parsed JSON response from the API.

post_excl_body <- function(link) {
  res <- httr2::request(link) |>
    httr2::req_auth_bearer_token(get_paddle_key()) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  handle_paddle_response(res)
}

#' Make a GET request to Paddle API
#' @param link The API endpoint URL.
#' @returns Parsed JSON response from the API.

get <- function(link) {
  res <- httr2::request(link) |>
    httr2::req_auth_bearer_token(get_paddle_key()) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  handle_paddle_response(res)
}

#' Make a DELETE request to Paddle API
#' @param link The API endpoint URL.
#' @param body The body of the request, typically a list to be converted to JSON.
#' @returns Parsed JSON response from the API.

update <- function(link, body) {
  res <- httr2::request(link) |>
    httr2::req_auth_bearer_token(get_paddle_key()) |>
    httr2::req_body_json(body) |>
    httr2::req_method("PATCH") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  handle_paddle_response(res)
}


#' Drop NULL values from a list
#' @param x A list from which to remove NULL values.
#' @returns A list with NULL values removed.
drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}
