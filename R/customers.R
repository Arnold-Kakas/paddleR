# --------------------------------------------------
# Customers
# --------------------------------------------------

#' Create a Customer.
#'
#' If successful, your response includes a copy of the new customer entity.
#'
#' @param email Email address for the customer. Required.
#' @param name Full name of the customer. Optional.
#' @param custom_data Named list of custom metadata. Optional.
#' @param locale Locale string (IETF BCP 47). Optional, defaults to "en".
#'
#' @return A data frame with the new customer info.
#'
#' @export
paddle_create_customer <- function(email,
                                   name = NULL,
                                   custom_data = NULL,
                                   locale = NULL) {
  if (missing(email) || !nzchar(email)) {
    stop("`email` is required and must be a non-empty string.", call. = FALSE)
  }

  body <- list(email = email)

  if (!is.null(name))        body$name <- name
  if (!is.null(custom_data)) body$custom_data <- custom_data
  if (!is.null(locale))      body$locale <- locale

  print(body)

  url <- paste0(get_paddle_url(), "/customers")
  post(url, body)
}

#' Retrieve Paddle Customers
#'
#' Fetches a paginated list of customers. By default, only active customers are returned.
#'
#' @param email Vector of email addresses to match exactly. Optional.
#' @param id Vector of Paddle customer IDs. Optional.
#' @param status Vector of statuses to filter by (`"active"` or `"archived"`). Optional.
#' @param after Return customers after this Paddle ID for pagination. Optional.
#' @param order_by Order results by `"id[ASC]"` or `"id[DESC]"`. Optional.
#' @param per_page Number of results per page (1–200). Optional, defaults to 50.
#' @param search Search term (matches ID, name, email). Optional.
#'
#' @return A list with customer data and pagination info.
#' @export
paddle_list_customers <- function(email = NULL,
                                     id = NULL,
                                     status = NULL,
                                     after = NULL,
                                     order_by = NULL,
                                     per_page = NULL,
                                     search = NULL) {
  query <- list()

  if (!is.null(email))     query$email     <- paste(email, collapse = ",")
  if (!is.null(id))        query$id        <- paste(id, collapse = ",")
  if (!is.null(status))    query$status    <- paste(status, collapse = ",")
  if (!is.null(after))     query$after     <- after
  if (!is.null(order_by))  query$order_by  <- order_by
  if (!is.null(per_page))  query$per_page  <- per_page
  if (!is.null(search))    query$search    <- search

  base_url <- get_paddle_url()
  url <- httr2::url_modify(paste0(base_url, "/customers"), query = query)

  get(url)
}

#' Update a Paddle Customer
#'
#' Updates an existing customer using their Paddle ID.
#'
#' @param customer_id Paddle customer ID (required).
#' @param name Full name of the customer. Optional (can be `NULL` to remove).
#' @param email Email address of the customer. Optional.
#' @param status Status of the customer ("active" or "archived"). Optional.
#' @param custom_data Named list of custom metadata. Optional (can be `NULL` to remove).
#' @param locale Locale string (IETF BCP 47). Optional.
#'
#' @return A list with the updated customer info.
#' @export
paddle_update_customer <- function(customer_id,
                                   name = NULL,
                                   email = NULL,
                                   status = NULL,
                                   custom_data = NULL,
                                   locale = NULL) {
  if (missing(customer_id) || !nzchar(customer_id)) {
    stop("`customer_id` is required and must be a non-empty string.", call. = FALSE)
  }

  body <- list()

  if (!is.null(name))        body$name <- name
  if (!is.null(email))       body$email <- email
  if (!is.null(status))      body$status <- status
  if (!missing(custom_data)) body$custom_data <- custom_data  # allow NULL
  if (!is.null(locale))      body$locale <- locale

  url <- paste0(get_paddle_url(), "/customers/", customer_id)

  update(url, body)
}

#' Generate an Authentication Token for a Paddle Customer
#'
#' Creates a temporary authentication token for a customer.
#' The token allows Paddle.js to present saved payment methods at checkout.
#'
#' @param customer_id Paddle customer ID (required).
#'
#' @return A list containing the auth token and expiry details.
#' @export
paddle_generate_auth_token <- function(customer_id) {
  if (missing(customer_id) || !nzchar(customer_id)) {
    stop("`customer_id` is required and must be a non-empty string.", call. = FALSE)
  }

  url <- paste0(get_paddle_url(), "/customers/", customer_id, "/auth-token")

  post_excl_body(url)  # no body, just POST the path
}
