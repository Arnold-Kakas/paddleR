# --------------------------------------------------
# Transactions
# --------------------------------------------------

#' List Paddle Transactions
#'
#' Retrieves a paginated list of transactions from the Paddle API.
#'
#' @param after Character. Paddle ID cursor for pagination. Optional.
#' @param id Character vector of transaction IDs. Optional.
#' @param customer_id Character vector of customer IDs. Optional.
#' @param subscription_id Character vector of subscription IDs or `"null"` to fetch unlinked. Optional.
#' @param invoice_number Character vector of invoice numbers. Optional.
#' @param origin Character vector of origins. Optional.
#' @param status Character vector of statuses. Must be one of `"draft"`, `"ready"`, `"billed"`, `"paid"`, `"completed"`, `"canceled"`, `"past_due"`. Optional.
#' @param collection_mode Character. Must be one of `"automatic"` or `"manual"`. Optional.
#' @param billed_at Character. RFC 3339 datetime or filter (e.g., "billed_at[LT]=2023-01-01T00:00:00Z"). Optional.
#' @param created_at Character. Same format as billed_at. Optional.
#' @param updated_at Character. Same format as billed_at. Optional.
#' @param order_by Character. Must be one of `billed_at[ASC|DESC]`, `created_at[ASC|DESC]`, `id[ASC|DESC]`, `updated_at[ASC|DESC]`. Optional.
#' @param include Character vector. Must be one of `"address"`, `"adjustments"`, `"adjustments_totals"`, `"available_payment_methods"`, `"business"`, `"customer"`, `"discount"`. Optional.
#' @param per_page Integer. Max results per page (max 200). Optional.
#'
#' @return A list containing transactions and pagination metadata.
#' @export
paddle_list_transactions <- function(after = NULL,
                                     id = NULL,
                                     customer_id = NULL,
                                     subscription_id = NULL,
                                     invoice_number = NULL,
                                     origin = NULL,
                                     status = NULL,
                                     collection_mode = NULL,
                                     billed_at = NULL,
                                     created_at = NULL,
                                     updated_at = NULL,
                                     order_by = NULL,
                                     include = NULL,
                                     per_page = NULL) {

  if (!is.null(order_by)) {
    valid_fields <- c("billed_at", "created_at", "id", "updated_at")
    valid_order_by <- as.vector(outer(valid_fields, c("[ASC]", "[DESC]"), paste0))
    if (!order_by %in% valid_order_by) {
      stop(sprintf("`order_by` must be one of: %s", paste(valid_order_by, collapse = ", ")), call. = FALSE)
    }
  }

  if (!is.null(status)) {
    valid_status <- c("draft", "ready", "billed", "paid", "completed", "canceled", "past_due")
    if (any(!status %in% valid_status)) {
      stop(sprintf("`status` must be one of: %s", paste(valid_status, collapse = ", ")), call. = FALSE)
    }
  }

  if (!is.null(collection_mode)) {
    if (!collection_mode %in% c("automatic", "manual")) {
      stop("`collection_mode` must be one of: 'automatic', 'manual'", call. = FALSE)
    }
  }

  if (!is.null(include)) {
    valid_include <- c("address", "adjustments", "adjustments_totals",
                       "available_payment_methods", "business", "customer", "discount")
    if (any(!include %in% valid_include)) {
      stop(sprintf("`include` must be one of: %s", paste(valid_include, collapse = ", ")), call. = FALSE)
    }
  }

  query <- list()
  if (!is.null(after))           query$after <- after
  if (!is.null(id))              query$id <- paste(id, collapse = ",")
  if (!is.null(customer_id))     query$customer_id <- paste(customer_id, collapse = ",")
  if (!is.null(subscription_id)) query$subscription_id <- paste(subscription_id, collapse = ",")
  if (!is.null(invoice_number))  query$invoice_number <- paste(invoice_number, collapse = ",")
  if (!is.null(origin))          query$origin <- paste(origin, collapse = ",")
  if (!is.null(status))          query$status <- paste(status, collapse = ",")
  if (!is.null(collection_mode)) query$collection_mode <- collection_mode
  if (!is.null(billed_at))       query$billed_at <- billed_at
  if (!is.null(created_at))      query$created_at <- created_at
  if (!is.null(updated_at))      query$updated_at <- updated_at
  if (!is.null(order_by))        query$order_by <- order_by
  if (!is.null(include))         query$include <- paste(include, collapse = ",")
  if (!is.null(per_page))        query$per_page <- per_page

  url <- httr2::url_modify(paste0(get_paddle_url(), "/transactions"), query = query)

  get(url)
}


#' Create a Transaction
#'
#' Creates a new transaction in the Paddle API.
#'
#' @param items List of item objects. Each must contain `price_id` (Character, required) and `quantity` (integer, required).
#' @param status Character. Optional. Must be `"billed"` or skipped to let Paddle set the status.
#' @param customer_id Character. Optional. Paddle customer ID (e.g., "ctm_123").
#' @param address_id Character. Optional. Paddle address ID (e.g., "add_456"). Requires `customer_id`.
#' @param business_id Character. Optional. Paddle business ID. Requires `customer_id`.
#' @param custom_data Named list. Optional structured metadata.
#' @param currency_code Character. Optional. Must be `"USD"`, `"EUR"`, or `"GBP"` if `collection_mode = "manual"`.
#' @param collection_mode Character. Optional. Must be one of `"automatic"` or `"manual"`. Defaults to `"automatic"`.
#' @param discount_id Character. Optional. Paddle discount ID.
#' @param billing_details List. Required if `collection_mode = "manual"`. May include:
#'   - `payment_terms` → list with `interval` (Character, must be one of `"day"`, `"week"`, `"month"`, `"year"`) and `frequency` (integer)
#'   - `enable_checkout` (logical)
#'   - `purchase_order_number` (Character)
#'   - `additional_information` (Character or null)
#' @param billing_period List with `starts_at` and `ends_at` in RFC 3339 format. Optional.
#' @param checkout List with `url` (Character) to override payment link. Optional.
#' @param include Character vector. Must be one of `"address"`, `"adjustments"`, `"adjustments_totals"`, `"available_payment_methods"`, `"business"`, `"customer"`, `"discount"`. Optional.
#'
#' @return A list containing the created transaction and metadata.
#' @export
paddle_create_transaction <- function(items,
                                      status = NULL,
                                      customer_id = NULL,
                                      address_id = NULL,
                                      business_id = NULL,
                                      custom_data = NULL,
                                      currency_code = NULL,
                                      collection_mode = NULL,
                                      discount_id = NULL,
                                      billing_details = NULL,
                                      billing_period = NULL,
                                      checkout = NULL,
                                      include = NULL) {
  # Validate required fields
  if (missing(items) || !is.list(items) || length(items) == 0) {
    stop("`items` must be a non-empty list.", call. = FALSE)
  }

  for (item in items) {
    if (!is.list(item) || !("price_id" %in% names(item)) || !("quantity" %in% names(item))) {
      stop("Each item must be a list with `price_id` and `quantity`.", call. = FALSE)
    }
    if (!is.character(item$price_id) || !nzchar(item$price_id)) {
      stop("`price_id` in each item must be a non-empty string.", call. = FALSE)
    }
    if (!is.numeric(item$quantity) || item$quantity <= 0) {
      stop("`quantity` in each item must be a positive number.", call. = FALSE)
    }
  }

  # Optional: enforce status
  if (!is.null(status) && status != "billed") {
    stop("`status` must be 'billed' or omitted (defaults to 'ready' or 'draft').", call. = FALSE)
  }

  # collection_mode
  if (!is.null(collection_mode) && !collection_mode %in% c("automatic", "manual")) {
    stop("`collection_mode` must be one of: 'automatic', 'manual'.", call. = FALSE)
  }

  if (!is.null(currency_code) && collection_mode == "manual") {
    if (!currency_code %in% c("USD", "EUR", "GBP")) {
      stop("`currency_code` must be one of: 'USD', 'EUR', 'GBP' when `collection_mode` is 'manual'.", call. = FALSE)
    }
  }

  # if manual, currency_code must be set
  if (!is.null(collection_mode) && collection_mode == "manual" && is.null(currency_code)) {
    stop("`currency_code` is required and must be one of: 'USD', 'EUR', 'GBP' when `collection_mode` is 'manual'.", call. = FALSE)
  }

  # if manual, billing_details must be set
  if (!is.null(collection_mode) && collection_mode == "manual" && is.null(billing_details)) {
    stop("`billing_details` is required when `collection_mode` is 'manual'.", call. = FALSE)
  }

  if (!is.null(billing_details)) {
    if (!is.list(billing_details)) stop("`billing_details` must be a list.", call. = FALSE)
    if (!"payment_terms" %in% names(billing_details)) {
      stop("`billing_details` must include `payment_terms`.", call. = FALSE)
    }
    pt <- billing_details$payment_terms
    if (!is.list(pt) || !"interval" %in% names(pt) || !"frequency" %in% names(pt)) {
      stop("`payment_terms` must be a list with `interval` and `frequency`.", call. = FALSE)
    }
  }

  # include validation
  if (!is.null(include)) {
    valid_include <- c("address", "adjustments", "adjustments_totals",
                       "available_payment_methods", "business", "customer", "discount")
    if (any(!include %in% valid_include)) {
      stop(sprintf("`include` must be one of: %s", paste(valid_include, collapse = ", ")), call. = FALSE)
    }
  }

  body <- list(items = items)
  if (!is.null(status))           body$status <- status
  if (!is.null(customer_id))      body$customer_id <- customer_id
  if (!is.null(address_id))       body$address_id <- address_id
  if (!is.null(business_id))      body$business_id <- business_id
  if (!is.null(custom_data))      body$custom_data <- custom_data
  if (!is.null(currency_code))    body$currency_code <- currency_code
  if (!is.null(collection_mode))  body$collection_mode <- collection_mode
  if (!is.null(discount_id))      body$discount_id <- discount_id
  if (!is.null(billing_details))  body$billing_details <- billing_details
  if (!is.null(billing_period))   body$billing_period <- billing_period
  if (!is.null(checkout))         body$checkout <- checkout

  query <- list()
  if (!is.null(include)) query$include <- paste(include, collapse = ",")

  url <- httr2::url_modify(paste0(get_paddle_url(), "/transactions"), query = query)

  post(url, body)
}

#' Update a Transaction
#'
#' Updates a transaction by its ID. Only transactions with status `draft` or `ready` can be updated.
#'
#' @param transaction_id Character. Required. Paddle ID of the transaction (e.g. `"txn_abc123"`).
#' @param status Character. Optional. `"billed"` or `"canceled"`.
#' @param customer_id, address_id, business_id Character. Optional Paddle IDs.
#' @param custom_data Named list. Optional.
#' @param currency_code Character. Optional. `"USD"`, `"EUR"`, or `"GBP"` if `collection_mode = "manual"`.
#' @param collection_mode Character. Optional. `"automatic"` or `"manual"`.
#' @param discount_id Character. Optional.
#' @param billing_details List. Optional, see API.
#' @param payment_terms List with `interval` and `frequency`. Shortcut to fill `billing_details$payment_terms`.
#' @param billing_period List with `starts_at` and `ends_at` in RFC 3339 format. Optional.
#' @param items List of objects with `price_id` and `quantity`. Optional.
#' @param checkout_url Character. Optional.
#' @param include Character vector of related entities to return. Optional.
#'
#' @return A list with updated transaction data and metadata.
#' @export
paddle_update_transaction <- function(transaction_id,
                                      status = NULL,
                                      customer_id = NULL,
                                      address_id = NULL,
                                      business_id = NULL,
                                      custom_data = NULL,
                                      currency_code = NULL,
                                      collection_mode = NULL,
                                      discount_id = NULL,
                                      billing_details = NULL,
                                      payment_terms = NULL,
                                      billing_period = NULL,
                                      items = NULL,
                                      checkout_url = NULL,
                                      include = NULL) {
  if (!is.character(transaction_id) || !nzchar(transaction_id))
    stop("`transaction_id` must be a non-empty string.", call. = FALSE)

  if (!is.null(status) && !status %in% c("billed", "canceled"))
    stop("`status` must be one of: 'billed', 'canceled'", call. = FALSE)

  if (!is.null(collection_mode) && !collection_mode %in% c("automatic", "manual"))
    stop("`collection_mode` must be 'automatic' or 'manual'", call. = FALSE)

  if (!is.null(currency_code) && collection_mode == "manual" &&
      !currency_code %in% c("USD", "EUR", "GBP"))
    stop("When `collection_mode` is 'manual', `currency_code` must be USD, EUR, or GBP.", call. = FALSE)

  if (!is.null(collection_mode) && collection_mode == "manual" && is.null(currency_code))
    stop("`currency_code` is required when `collection_mode = 'manual'`", call. = FALSE)

  if (!is.null(items)) {
    if (!is.list(items) || any(!vapply(items, function(it) all(c("price_id", "quantity") %in% names(it)), logical(1))))
      stop("Each item must be a list with `price_id` and `quantity`.", call. = FALSE)
  }

  if (!is.null(include)) {
    valid_include <- c("address", "adjustments", "adjustments_totals",
                       "available_payment_methods", "business", "customer", "discount")
    if (!all(include %in% valid_include)) {
      stop("`include` must only contain valid entities (see docs).", call. = FALSE)
    }
  }

  if (!is.null(payment_terms)) {
    if (!is.list(payment_terms) || !"interval" %in% names(payment_terms) || !"frequency" %in% names(payment_terms)) {
      stop("`payment_terms` must be a list with `interval` and `frequency`.", call. = FALSE)
    }
    billing_details <- billing_details %||% list()
    billing_details$payment_terms <- payment_terms
  }

  drop_nulls <- function(x) {
    x[!vapply(x, is.null, logical(1))]
  }

  body <- drop_nulls(list(
    status = status,
    customer_id = customer_id,
    address_id = address_id,
    business_id = business_id,
    custom_data = custom_data,
    currency_code = currency_code,
    collection_mode = collection_mode,
    discount_id = discount_id,
    billing_details = billing_details,
    billing_period = billing_period,
    items = items,
    checkout = if (!is.null(checkout_url)) list(url = checkout_url) else NULL
  ))

  query <- if (!is.null(include)) list(include = paste(include, collapse = ",")) else NULL

  url <- httr2::url_modify(paste0(get_paddle_url(), "/transactions/", transaction_id), query = query)

  update(url, body)
}

#' Preview a transaction
#'
#' Sends a preview request to Paddle to simulate a transaction without creating it.
#'
#' @param items List of lists. Required. Each must include `price_id` and `quantity`.
#' @param customer_id Character. Optional. Customer ID prefixed with "ctm_".
#' @param currency_code Character. Optional. Must be valid ISO 4217 code.
#' @param discount_id Character. Optional. Discount ID prefixed with "dsc_".
#' @param ignore_trials Logical. Optional. If TRUE, disables trial discounts.
#' @param customer_ip_address Character. Optional. Valid IPv4 or IPv6.
#' @param address Named list. Optional. Must include `country_code` (2-letter) and optionally `postal_code`.
#'
#' @return A list with previewed transaction data.
#' @export
paddle_preview_transaction <- function(items,
                                       customer_id = NULL,
                                       currency_code = NULL,
                                       discount_id = NULL,
                                       ignore_trials = NULL,
                                       customer_ip_address = NULL,
                                       address = NULL) {
  # Validate `items`
  if (!is.list(items) || length(items) == 0) {
    stop("`items` must be a non-empty list.", call. = FALSE)
  }
  for (it in items) {
    if (!is.list(it) || !"price_id" %in% names(it) || !"quantity" %in% names(it)) {
      stop("Each item must be a list with `price_id` and `quantity`.", call. = FALSE)
    }
    if (!is.character(it$price_id) || !nzchar(it$price_id)) {
      stop("`price_id` must be a non-empty string.", call. = FALSE)
    }
    if (!is.numeric(it$quantity) || it$quantity <= 0) {
      stop("`quantity` must be a positive number.", call. = FALSE)
    }
  }

  if (!is.null(customer_id) && (!is.character(customer_id) || !nzchar(customer_id))) {
    stop("`customer_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(currency_code) && !currency_code %in% c("USD", "EUR", "GBP")) {
    stop("`currency_code` must be one of: 'USD', 'EUR', 'GBP'.", call. = FALSE)
  }

  if (!is.null(discount_id) && (!is.character(discount_id) || !nzchar(discount_id))) {
    stop("`discount_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(ignore_trials) && !is.logical(ignore_trials)) {
    stop("`ignore_trials` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(customer_ip_address) &&
      !grepl("^\\d{1,3}(\\.\\d{1,3}){3}$|^[a-fA-F0-9:]+$", customer_ip_address)) {
    stop("`customer_ip_address` must be a valid IP address.", call. = FALSE)
  }

  if (!is.null(address)) {
    if (!is.list(address) || !"country_code" %in% names(address)) {
      stop("`address` must be a list with at least `country_code`.", call. = FALSE)
    }
    if (!grepl("^[A-Z]{2}$", address$country_code)) {
      stop("`country_code` in `address` must be a 2-letter ISO country code.", call. = FALSE)
    }
  }

  body <- drop_nulls(list(
    items = items,
    customer_id = customer_id,
    currency_code = currency_code,
    discount_id = discount_id,
    ignore_trials = ignore_trials,
    customer_ip_address = customer_ip_address,
    address = address
  ))

  post(paste0(get_paddle_url(), "/transactions/preview"), body)
}

# Not yet implemented in the package:
# Preview a transaction
# post
# https://api.paddle.com/transactions/preview
# Get a PDF invoice for a transaction
# get
# https://api.paddle.com/transactions/{transaction_id}/invoice
# Revise a billed or completed transaction
# post
# https://api.paddle.com/transactions/{transaction_id}/revise
