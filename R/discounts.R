# --------------------------------------------------
# Products
# --------------------------------------------------

#' Retrieve Paddle Discounts
#'
#' Fetches a paginated list of discounts from the Paddle API. By default, only active discounts are returned.
#'
#' @param id Character vector of discount IDs (e.g., `"dsc_xxx"`). Optional.
#' @param code Character vector of discount codes. Optional.
#' @param status Character vector of discount statuses (one of `"active"`, `"archived"`). Optional.
#' @param mode Character. Filter discounts by mode (one of `"standard"`, `"custom"`). Optional.
#' @param after Character. Return entities after the specified Paddle ID (used for pagination). Optional.
#' @param order_by Character. Ordering string (e.g., `"id[ASC]"` or `"created_at[DESC]"`). Optional.
#' @param per_page Integer. Number of results per page (max 200). Optional, defaults to 50.
#'
#' @return A list containing discount data and pagination metadata.
#' @export
paddle_list_discounts <- function(id = NULL,
                                  code = NULL,
                                  status = NULL,
                                  mode = NULL,
                                  after = NULL,
                                  order_by = NULL,
                                  per_page = NULL) {

  if (!is.null(order_by)) {
    valid_order_fields <- c("id", "created_at")
    valid_directions <- c("[ASC]", "[DESC]")
    valid_order_by <- as.vector(outer(valid_order_fields, valid_directions, paste0))
    if (!order_by %in% valid_order_by) {
      stop(sprintf(
        "`order_by` must be one of: %s",
        paste(valid_order_by, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(status)) {
    valid_statuses <- c("active", "archived")
    if (any(!status %in% valid_statuses)) {
      stop(sprintf(
        "`status` must be one of: %s",
        paste(valid_statuses, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(mode)) {
    valid_modes <- c("standard", "custom")
    if (any(!mode %in% valid_modes)) {
      stop(sprintf(
        "`mode` must be one of: %s",
        paste(valid_modes, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(per_page)) {
    if (!is.numeric(per_page) || per_page < 1 || per_page > 200) {
      stop("`per_page` must be between 1 and 200.", call. = FALSE)
    }
  }

  query <- list()

  if (!is.null(id))       query$id       <- paste(id, collapse = ",")
  if (!is.null(code))     query$code     <- paste(code, collapse = ",")
  if (!is.null(status))   query$status   <- paste(status, collapse = ",")
  if (!is.null(mode))     query$mode     <- mode
  if (!is.null(after))    query$after    <- after
  if (!is.null(order_by)) query$order_by <- order_by
  if (!is.null(per_page)) query$per_page <- per_page

  url <- httr2::url_modify(paste0(get_paddle_url(), "/discounts"), query = query)

  get(url)
}


#' Create a Paddle Discount
#'
#' Sends a POST request to the Paddle API to create a new discount.
#'
#' @param amount Character. Amount to discount by. Required.
#' @param description Character. Internal description of the discount. Required.
#' @param type Character. Type of discount (`"percentage"`, `"flat"`, or `"flat_per_seat"`). Required.
#' @param enabled_for_checkout Logical. Can be used at checkout? Optional.
#' @param code Character. Optional promo code (letters/numbers, max 32 characters).
#' @param mode Character. Filter discounts by mode (one of `"standard"`, `"custom"`). Optional.
#' @param currency_code Character. Required for `"flat"` or `"flat_per_seat"`. Optional.
#' @param recur Logical. Should it recur for subscriptions? Optional.
#' @param maximum_recurring_intervals Integer. Number of times to recur. Optional.
#' @param usage_limit Integer. Maximum redemptions. Optional.
#' @param restrict_to Character vector of product or price IDs. Optional.
#' @param expires_at Character. RFC 3339 datetime string. Optional.
#' @param custom_data Named list. Custom metadata. Optional.
#'
#' @return A list containing the created discount and metadata.
#' @export
paddle_create_discount <- function(amount,
                                   description,
                                   type,
                                   enabled_for_checkout = NULL,
                                   code = NULL,
                                   mode = NULL,
                                   currency_code = NULL,
                                   recur = NULL,
                                   maximum_recurring_intervals = NULL,
                                   usage_limit = NULL,
                                   restrict_to = NULL,
                                   expires_at = NULL,
                                   custom_data = NULL) {

  # Check required parameters
  if (missing(amount) || missing(description) || missing(type)) {
    stop("`amount`, `description`, and `type` are required fields.", call. = FALSE)
  }

  if (!type %in% c("percentage", "flat", "flat_per_seat")) {
    stop("`type` must be one of: 'percentage', 'flat', 'flat_per_seat'", call. = FALSE)
  }

  if (type %in% c("flat", "flat_per_seat") && is.null(currency_code)) {
    stop("`currency_code` is required for flat or flat_per_seat discount types.", call. = FALSE)
  }

  if (!is.null(code) && (!grepl("^[A-Za-z0-9]{1,32}$", code))) {
    stop("`code` must be alphanumeric and up to 32 characters.", call. = FALSE)
  }


  if (!is.null(mode)) {
    valid_modes <- c("standard", "custom")
    if (any(!mode %in% valid_modes)) {
      stop(sprintf(
        "`mode` must be one of: %s",
        paste(valid_modes, collapse = ", ")
      ), call. = FALSE)
    }
  }

  body <- list(
    amount = amount,
    description = description,
    type = type
  )

  if (!is.null(enabled_for_checkout))          body$enabled_for_checkout <- tolower(enabled_for_checkout)
  if (!is.null(code))                          body$code <- code
  if (!is.null(mode))                          body$mode <- mode
  if (!is.null(currency_code))                 body$currency_code <- currency_code
  if (!is.null(recur))                         body$recur <- tolower(recur)
  if (!is.null(maximum_recurring_intervals))   body$maximum_recurring_intervals <- maximum_recurring_intervals
  if (!is.null(usage_limit))                   body$usage_limit <- usage_limit
  if (!is.null(restrict_to))                   body$restrict_to <- restrict_to
  if (!is.null(expires_at))                    body$expires_at <- expires_at
  if (!is.null(custom_data))                   body$custom_data <- custom_data

  url <- paste0(get_paddle_url(), "/discounts")

  post(url, body)
}

#' Update a Paddle Discount
#'
#' Updates an existing discount by ID via the Paddle API.
#'
#' @param discount_id Character. The Paddle discount ID (e.g., "dsc_123"). Required.
#' @param status Character vector of discount statuses (one of `"active"`, `"archived"`). Optional.
#' @param description Character. Internal description. Optional.
#' @param enabled_for_checkout Logical. Available for checkout? Optional.
#' @param code Character. Discount code. Optional.
#' @param type Character. Type of discount (`"percentage"`, `"flat"`, or `"flat_per_seat"`). Optional.
#' @param mode Character. Filter discounts by mode (one of `"standard"`, `"custom"`). Optional.
#' @param amount Character. Discount amount. Optional.
#' @param currency_code Character. Required for "flat"/"flat_per_seat". Optional.
#' @param recur Logical. Repeating discount? Optional.
#' @param maximum_recurring_intervals Integer. Number of repeats. Optional.
#' @param usage_limit Integer. Max redemptions. Optional.
#' @param restrict_to Character vector. Product or price IDs. Optional.
#' @param expires_at Character. RFC 3339 datetime string. Optional.
#' @param custom_data Named list. Custom metadata. Optional.
#'
#' @return A list containing the updated discount and metadata.
#' @export
paddle_update_discount <- function(discount_id,
                                   status = NULL,
                                   description = NULL,
                                   enabled_for_checkout = NULL,
                                   code = NULL,
                                   type = NULL,
                                   mode = NULL,
                                   amount = NULL,
                                   currency_code = NULL,
                                   recur = NULL,
                                   maximum_recurring_intervals = NULL,
                                   usage_limit = NULL,
                                   restrict_to = NULL,
                                   expires_at = NULL,
                                   custom_data = NULL) {

  if (missing(discount_id) || !is.character(discount_id) || nchar(discount_id) == 0) {
    stop("`discount_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(type) && !type %in% c("percentage", "flat", "flat_per_seat")) {
    stop("`type` must be one of: 'percentage', 'flat', 'flat_per_seat'", call. = FALSE)
  }

  if (!is.null(code) && (!grepl("^[A-Za-z0-9]{1,32}$", code))) {
    stop("`code` must be alphanumeric and up to 32 characters.", call. = FALSE)
  }

  if (!is.null(type) && type %in% c("flat", "flat_per_seat") && is.null(currency_code)) {
    stop("`currency_code` is required when type is 'flat' or 'flat_per_seat'", call. = FALSE)
  }


  if (!is.null(status)) {
    valid_statuses <- c("active", "archived")
    if (any(!status %in% valid_statuses)) {
      stop(sprintf(
        "`status` must be one of: %s",
        paste(valid_statuses, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(mode)) {
    valid_modes <- c("standard", "custom")
    if (any(!mode %in% valid_modes)) {
      stop(sprintf(
        "`mode` must be one of: %s",
        paste(valid_modes, collapse = ", ")
      ), call. = FALSE)
    }
  }

  body <- list()
  if (!is.null(status))                      body$status <- status
  if (!is.null(description))                 body$description <- description
  if (!is.null(enabled_for_checkout))        body$enabled_for_checkout <- tolower(enabled_for_checkout)
  if (!is.null(code))                        body$code <- code
  if (!is.null(type))                        body$type <- type
  if (!is.null(mode))                        body$mode <- mode
  if (!is.null(amount))                      body$amount <- amount
  if (!is.null(currency_code))               body$currency_code <- currency_code
  if (!is.null(recur))                       body$recur <- tolower(recur)
  if (!is.null(maximum_recurring_intervals)) body$maximum_recurring_intervals <- maximum_recurring_intervals
  if (!is.null(usage_limit))                 body$usage_limit <- usage_limit
  if (!is.null(restrict_to))                 body$restrict_to <- restrict_to
  if (!is.null(expires_at))                  body$expires_at <- expires_at
  if (!is.null(custom_data))                 body$custom_data <- custom_data

  url <- paste0(get_paddle_url(), "/discounts/", discount_id)

  update(url, body)
}

