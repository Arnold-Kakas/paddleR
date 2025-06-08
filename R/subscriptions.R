# --------------------------------------------------
# subscriptions
# --------------------------------------------------

#' List subscriptions
#'
#' Retrieves a paginated list of subscriptions from the Paddle API.
#'
#' @param id,customer_id,address_id,price_id Character vectors. Optional filters.
#' @param status Character vector. Optional. Must be one of `"active"`, `"cancelled"`, `"past_due"`, `"paused"`, `"trialing"`.
#' @param scheduled_change_action Character vector. Optional. Must be one of `"cancel"`, `"pause"`, `"resume"`.
#' @param after Character. Optional. Return entities after the specified Paddle ID when working with paginated endpoints.
#' @param order_by Character. Optional. Must be in the form "id[ASC]" or "id[DESC]".
#' @param per_page Integer. Optional. Max 200, defaults to 50.
#' @param collection_mode Character. Optional. Must be one of `"automatic"`, `"manual"`.
#'
#' @return A list with subscription data and pagination metadata.
#' @export
paddle_list_subscriptions <- function(id = NULL,
                                      customer_id = NULL,
                                      address_id = NULL,
                                      price_id = NULL,
                                      status = NULL,
                                      scheduled_change_action = NULL,
                                      collection_mode = NULL,
                                      after = NULL,
                                      order_by = NULL,
                                      per_page = NULL) {

  if (!is.null(per_page)) {
    if (!is.numeric(per_page) || per_page < 1 || per_page > 200) {
      stop("`per_page` must be between 1 and 200.", call. = FALSE)
    }
  }

  if (!is.null(order_by) && !grepl("^id\\[(ASC|DESC)\\]$", order_by)) {
    stop("`order_by` must be 'id[ASC]' or 'id[DESC]'.", call. = FALSE)
  }

  if (!is.null(collection_mode) && !collection_mode %in% c("automatic", "manual")) {
    stop("`collection_mode` must be 'automatic' or 'manual'.", call. = FALSE)
  }

  if (!is.null(status) && !status %in% c("active", "cancelled", "past_due", "paused", "trialing")) {
    stop("`status` must be 'active', 'cancelled', 'past_due', 'paused' or 'trialing'.", call. = FALSE)
  }

  if (!is.null(scheduled_change_action) && !scheduled_change_action %in% c("cancel", "pause", "resume")) {
    stop("`scheduled_change_action` must be 'cancel', 'pause' or 'resume'.", call. = FALSE)
  }

  # Convert vectors to comma-separated strings
  to_csv <- function(x) if (!is.null(x)) paste(x, collapse = ",") else NULL

  query <- drop_nulls(list(
    id = to_csv(id),
    customer_id = to_csv(customer_id),
    address_id = to_csv(address_id),
    price_id = to_csv(price_id),
    status = to_csv(status),
    scheduled_change_action = to_csv(scheduled_change_action),
    collection_mode = collection_mode,
    after = after,
    order_by = order_by,
    per_page = per_page
  ))

  url <- httr2::url_modify(paste0(get_paddle_url(), "/subscriptions"), query = query)

  get(url)
}

#' Update a subscription
#'
#' Updates a subscription using its ID. Required for changes like billing date, items, or proration.
#'
#' @param subscription_id Character. Required. The ID of the subscription (e.g. "sub_abc123").
#' @param customer_id Character. Optional. Paddle customer ID.
#' @param address_id Character. Optional. Paddle address ID.
#' @param business_id Character or NULL. Optional.
#' @param currency_code Character. Optional. Supported: "USD", "EUR", "GBP" (for manual collection).
#' @param next_billed_at Character. Optional. RFC 3339 datetime string.
#' @param discount List or NULL. Optional. Must include `id` (string) and `effective_from` (string, must be one of `"immediately"` or `"next_billing_period`), or NULL to remove.
#' @param collection_mode Character. Optional. One of: `"automatic"`, `"manual"`.
#' @param billing_details List or NULL. Required if `collection_mode` is "manual", NULL if changing collection_mode to automatic. Must include `enable_checkout` (boolean), `purchase_order_number` (string), `payment_terms` (list with `interval` (`day`, `week`, `month` or `year`) and `frequency` (integer)) and optional `additional_information` (string)
#' @param items List of item lists. Optional. Each must include `price_id` (string) and `quantity` (numeric). If updating an existing item and not changing the quantity, you may omit quantity.
#' @param proration_billing_mode Character. Required when making changes that impact billing. Must be one of:
#'        `"prorated_immediately"`, `"prorated_next_billing_period"`, `"full_immediately"`, `"full_next_billing_period"` and `"do_not_bill"`.
#' @param on_payment_failure Character. Optional. Must be one of: `"prevent_change"`, `"allow_change"`.
#' @param custom_data Named list or NULL. Optional.
#' @param scheduled_change NULL. Set to NULL to remove a scheduled change.
#'
#' @return A list with updated subscription entity and metadata.
#' @export
paddle_update_subscription <- function(
    subscription_id,
    customer_id = NULL,
    address_id = NULL,
    business_id = NULL,
    currency_code = NULL,
    next_billed_at = NULL,
    discount = NULL,
    collection_mode = NULL,
    billing_details = NULL,
    items = NULL,
    proration_billing_mode = NULL,
    on_payment_failure = NULL,
    custom_data = NULL,
    scheduled_change = NULL
) {
  if (missing(subscription_id) || !is.character(subscription_id) || !nzchar(subscription_id)) {
    stop("`subscription_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(collection_mode) && !collection_mode %in% c("automatic", "manual")) {
    stop("`collection_mode` must be 'automatic' or 'manual'.", call. = FALSE)
  }

  if (!is.null(currency_code) && !currency_code %in% c("USD", "EUR", "GBP")) {
    stop("`currency_code` must be one of: 'USD', 'EUR', 'GBP'.", call. = FALSE)
  }

  if (!is.null(discount)) {
    if (!is.list(discount) || !all(c("id", "effective_from") %in% names(discount))) {
      stop("`discount` must be a list with `id` and `effective_from`, or NULL to remove.", call. = FALSE)
    }
  }

  if (!is.null(billing_details)) {
    if (!is.list(billing_details)) {
      stop("`billing_details` must be a list.", call. = FALSE)
    }
    if ("payment_terms" %in% names(billing_details)) {
      pt <- billing_details$payment_terms
      if (!is.list(pt) || !"interval" %in% names(pt) || !"frequency" %in% names(pt)) {
        stop("`payment_terms` must be a list with `interval` and `frequency`.", call. = FALSE)
      }
    }
  }

  if (!is.null(items)) {
    if (!is.list(items)) {
      stop("`items` must be a list of lists.", call. = FALSE)
    }
    for (item in items) {
      if (!is.list(item) || is.null(item$price_id)) {
        stop("Each item must include at least `price_id`.", call. = FALSE)
      }
    }
  }

  if (!is.null(proration_billing_mode)) {
    allowed_proration <- c(
      "prorated_immediately",
      "prorated_next_billing_period",
      "full_immediately",
      "full_next_billing_period",
      "do_not_bill"
    )
    if (!proration_billing_mode %in% allowed_proration) {
      stop(sprintf("`proration_billing_mode` must be one of: %s", paste(allowed_proration, collapse = ", ")), call. = FALSE)
    }
  } else if (!is.null(items) || !is.null(next_billed_at)) {
    stop("`proration_billing_mode` is required when updating items or next_billed_at.", call. = FALSE)
  }

  if (!is.null(on_payment_failure) && !on_payment_failure %in% c("prevent_change", "allow_change")) {
    stop("`on_payment_failure` must be 'prevent_change' or 'allow_change'.", call. = FALSE)
  }

  body <- drop_nulls(list(
    customer_id = customer_id,
    address_id = address_id,
    business_id = business_id,
    currency_code = currency_code,
    next_billed_at = next_billed_at,
    discount = discount,
    collection_mode = collection_mode,
    billing_details = billing_details,
    items = items,
    proration_billing_mode = proration_billing_mode,
    on_payment_failure = on_payment_failure,
    custom_data = custom_data,
    scheduled_change = scheduled_change
  ))

  update(
    link = paste0(get_paddle_url(), "/subscriptions/", subscription_id),
    body = body
  )
}

#' Preview an update to a subscription
#'
#' Previews an update to a subscription without applying those changes.
#'
#' @param subscription_id Character. Required. Paddle ID of the subscription (e.g. "sub_abc123").
#' @param customer_id Character. Optional. Paddle customer ID.
#' @param address_id Character. Optional. Paddle address ID.
#' @param business_id Character or NULL. Optional.
#' @param currency_code Character. Optional. Supported: "USD", "EUR", "GBP" (for manual collection).
#' @param next_billed_at Character. Optional. RFC 3339 datetime string.
#' @param discount List or NULL. Optional. Must include `id` (string) and `effective_from` (string, must be one of `"immediately"` or `"next_billing_period`), or NULL to remove.
#' @param collection_mode Character. Optional. One of: `"automatic"`, `"manual"`.
#' @param billing_details List or NULL. Required if `collection_mode` is "manual", NULL if changing collection_mode to automatic. Must include `enable_checkout` (boolean), `purchase_order_number` (string), `payment_terms` (list with `interval` (`day`, `week`, `month` or `year`) and `frequency` (integer)) and optional `additional_information` (string)
#' @param items List of item lists. Optional. Each must include `price_id` (string) and `quantity` (numeric). If updating an existing item and not changing the quantity, you may omit quantity.
#' @param proration_billing_mode Character. Required when making changes that impact billing. Must be one of:
#'        `"prorated_immediately"`, `"prorated_next_billing_period"`, `"full_immediately"`, `"full_next_billing_period"` and `"do_not_bill"`.
#' @param on_payment_failure Character. Optional. Must be one of: `"prevent_change"`, `"allow_change"`.
#' @param custom_data Named list or NULL. Optional.
#' @param scheduled_change NULL. Set to NULL to remove a scheduled change.
#'
#' @return A list containing subscription preview and transaction impact.
#' @export
paddle_preview_subscription_update <- function(
    subscription_id,
    customer_id = NULL,
    address_id = NULL,
    business_id = NULL,
    currency_code = NULL,
    next_billed_at = NULL,
    discount = NULL,
    collection_mode = NULL,
    billing_details = NULL,
    items = NULL,
    proration_billing_mode = NULL,
    on_payment_failure = NULL,
    custom_data = NULL,
    scheduled_change = NULL
) {
  if (missing(subscription_id) || !is.character(subscription_id) || !nzchar(subscription_id)) {
    stop("`subscription_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(collection_mode) && !collection_mode %in% c("automatic", "manual")) {
    stop("`collection_mode` must be 'automatic' or 'manual'.", call. = FALSE)
  }

  if (!is.null(currency_code) && !currency_code %in% c("USD", "EUR", "GBP")) {
    stop("`currency_code` must be one of: 'USD', 'EUR', 'GBP'.", call. = FALSE)
  }

  if (!is.null(discount)) {
    if (!is.list(discount) || !all(c("id", "effective_from") %in% names(discount))) {
      stop("`discount` must be a list with `id` and `effective_from`, or NULL to remove.", call. = FALSE)
    }
  }

  if (!is.null(billing_details)) {
    if (!is.list(billing_details)) {
      stop("`billing_details` must be a list.", call. = FALSE)
    }
    if ("payment_terms" %in% names(billing_details)) {
      pt <- billing_details$payment_terms
      if (!is.list(pt) || !"interval" %in% names(pt) || !"frequency" %in% names(pt)) {
        stop("`payment_terms` must be a list with `interval` and `frequency`.", call. = FALSE)
      }
    }
  }

  if (!is.null(items)) {
    if (!is.list(items)) {
      stop("`items` must be a list of lists.", call. = FALSE)
    }
    for (item in items) {
      if (!is.list(item) || is.null(item$price_id)) {
        stop("Each item must include at least `price_id`.", call. = FALSE)
      }
    }
  }

  if (!is.null(proration_billing_mode)) {
    allowed_modes <- c(
      "prorated_immediately",
      "prorated_next_billing_period",
      "full_immediately",
      "full_next_billing_period",
      "do_not_bill"
    )
    if (!proration_billing_mode %in% allowed_modes) {
      stop(sprintf("`proration_billing_mode` must be one of: %s", paste(allowed_modes, collapse = ", ")), call. = FALSE)
    }
  } else if (!is.null(items) || !is.null(next_billed_at)) {
    stop("`proration_billing_mode` is required when updating items or next_billed_at.", call. = FALSE)
  }

  if (!is.null(on_payment_failure) && !on_payment_failure %in% c("prevent_change", "allow_change")) {
    stop("`on_payment_failure` must be 'prevent_change' or 'allow_change'.", call. = FALSE)
  }

  body <- drop_nulls(list(
    customer_id = customer_id,
    address_id = address_id,
    business_id = business_id,
    currency_code = currency_code,
    next_billed_at = next_billed_at,
    discount = discount,
    collection_mode = collection_mode,
    billing_details = billing_details,
    items = items,
    proration_billing_mode = proration_billing_mode,
    on_payment_failure = on_payment_failure,
    custom_data = custom_data,
    scheduled_change = scheduled_change
  ))

  update(
    link = paste0(get_paddle_url(), "/subscriptions/", subscription_id, "/preview"),
    body = body
  )
}

#' Get a transaction to update payment method
#'
#' Returns a transaction that can be passed to a checkout to update payment details.
#' Only for subscriptions where `collection_mode` is `automatic`.
#'
#' Depending on subscription status:
#' - For `past_due`, returns the most recent failed transaction.
#' - For `active`, creates a new zero-amount transaction.
#'
#' @param subscription_id Character. Required. Paddle ID of the subscription (e.g. "sub_abc123").
#'
#' @return A list containing transaction details for payment method update.
#' @export
paddle_get_update_payment_transaction <- function(subscription_id) {
  if (missing(subscription_id) || !is.character(subscription_id) || !nzchar(subscription_id)) {
    stop("`subscription_id` must be a non-empty string.", call. = FALSE)
  }

  get(
    link = paste0(get_paddle_url(), "/subscriptions/", subscription_id, "/update-payment-method-transaction")
  )
}

#' Preview a one-time charge for a subscription
#'
#' Previews a one-time charge for a subscription without billing it.
#' Used to estimate the result of a charge for non-recurring items.
#'
#' @param subscription_id Character. Required. The Paddle subscription ID (e.g. "sub_abc123").
#' @param effective_from Character. Required. When the one-time charge should be billed (RFC 3339 format).
#' @param items List of item lists. Optional. Each must include `price_id` (string) and `quantity` (numeric). If updating an existing item and not changing the quantity, you may omit quantity.
#' @param on_payment_failure Character. Optional. Must be one of: `"prevent_change"`, `"allow_change"`.
#'
#' @return A list with preview of immediate and next transactions.
#' @export
paddle_preview_one_time_charge <- function(
    subscription_id,
    effective_from,
    items,
    on_payment_failure = NULL
) {
  if (missing(subscription_id) || !is.character(subscription_id) || !nzchar(subscription_id)) {
    stop("`subscription_id` must be a non-empty string.", call. = FALSE)
  }

  if (missing(effective_from) || !is.character(effective_from) || !nzchar(effective_from)) {
    stop("`effective_from` must be a non-empty RFC 3339 datetime string.", call. = FALSE)
  }

  if (missing(items) || !is.list(items)) {
    stop("`items` must be a non-empty list of charge items.", call. = FALSE)
  }

  for (item in items) {
    if (!is.list(item) || is.null(item$price_id) || is.null(item$quantity)) {
      stop("Each item must include `price_id` and `quantity`.", call. = FALSE)
    }
  }

  if (!is.null(on_payment_failure) && !on_payment_failure %in% c("prevent_change", "allow_change")) {
    stop("`on_payment_failure` must be one of: 'prevent_change', 'allow_change'.", call. = FALSE)
  }

  body <- drop_nulls(list(
    effective_from = effective_from,
    items = items,
    on_payment_failure = on_payment_failure
  ))

  post(
    link = paste0(get_paddle_url(), "/subscriptions/", subscription_id, "/charge/preview"),
    body = body
  )
}


#' Activate a trialing subscription
#'
#' Activates a trialing subscription using its ID. Only automatically-collected subscriptions
#' with status = "trialing" can be activated.
#'
#' This triggers an immediate charge and recalculates billing dates from activation time.
#'
#' @param subscription_id Character. Required. The Paddle subscription ID (e.g. "sub_abc123").
#'
#' @return A list with updated subscription entity and metadata.
#' @export
paddle_activate_trial_subscription <- function(subscription_id) {
  if (missing(subscription_id) || !is.character(subscription_id) || !nzchar(subscription_id)) {
    stop("`subscription_id` must be a non-empty string.", call. = FALSE)
  }

  post_excl_body(
    link = paste0(get_paddle_url(), "/subscriptions/", subscription_id, "/activate")
  )
}
