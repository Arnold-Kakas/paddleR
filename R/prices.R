# --------------------------------------------------
# Products
# --------------------------------------------------

#' Retrieve a List of Paddle Prices
#'
#' Fetches a paginated list of price entities from Paddle. Filters are available
#' for ID, product, status, recurrence, and ordering. Optionally includes related products.
#'
#' @param id Character vector of Paddle price IDs (e.g., "pri_abc123"). Optional.
#' @param product_id Character vector of Paddle product IDs. Optional.
#' @param status Character vector of status filters (e.g., `"active"`, `"archived"`). Optional.
#' @param include Character vector of related entities - default `NULL` or `"product"`. Optional.
#' @param order_by String specifying sort field and direction (e.g., "id[ASC]"). Valid fields for ordering: `billing_cycle.frequency`, `billing_cycle.interval`, `id`, `product_id`, `quantity.maximum`, `quantity.minimum`, `status`, `tax_mode`, `unit_price.amount`, and `unit_price.currency_code`. Valid directions `"[ASC]"` and `"[DESC]"` Optional.
#' @param per_page Number of results per page (max 200). Optional, defaults to 50.
#' @param after Return entities after the specified Paddle ID when working with paginated endpoints. Optional.
#' @param recurring Logical: TRUE to return recurring prices, FALSE for one-time. Optional.
#' @param type Character. Type of item (one of `"standard"` and `"custom"`). Optional.
#'
#' @return A list of price entities and pagination metadata.
#' @export
#'
#' @examplesIf paddle_has_token()
#' set_paddle_mode("sandbox")
#' result <- paddle_list_prices()
paddle_list_prices <- function(id = NULL,
                               product_id = NULL,
                               status = NULL,
                               include = NULL,
                               order_by = NULL,
                               per_page = NULL,
                               after = NULL,
                               recurring = NULL,
                               type = NULL) {

  if (!is.null(order_by)) {
    valid_order_entities <- c("billing_cycle.frequency[ASC]",
                              "billing_cycle.interval[ASC]",
                              "id[ASC]",
                              "product_id[ASC]",
                              "quantity.maximum[ASC]",
                              "quantity.minimum[ASC]",
                              "status[ASC]",
                              "tax_mode[ASC]",
                              "unit_price.amount[ASC]",
                              "unit_price.currency_code[ASC]",
                              "billing_cycle.frequency[DESC]",
                              "billing_cycle.interval[DESC]",
                              "id[DESC]",
                              "product_id[DESC]",
                              "quantity.maximum[DESC]",
                              "quantity.minimum[DESC]",
                              "status[DESC]",
                              "tax_mode[DESC]",
                              "unit_price.amount[DESC]",
                              "unit_price.currency_code[DESC]")
    if (!order_by %in% valid_order_entities) {
      stop(sprintf(
        "`order_by` must be one of: %s",
        paste(valid_order_entities, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(type)) {
    valid_types <- c("standard", "custom")
    if (!type %in% valid_types) {
      stop(sprintf(
        "`type` must be one of: %s",
        paste(valid_types, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(status)) {
    valid_status <- c("active", "archived")
    if (!status %in% valid_status) {
      stop(sprintf(
        "`status` must be one of: %s",
        paste(valid_status, collapse = ", ")
      ), call. = FALSE)
    }
  }

  query <- list()

  if (!is.null(id))         query$id         <- paste(id, collapse = ",")
  if (!is.null(product_id)) query$product_id <- paste(product_id, collapse = ",")
  if (!is.null(status))     query$status     <- paste(status, collapse = ",")
  if (!is.null(include))    query$include    <- paste(include, collapse = ",")
  if (!is.null(order_by))   query$order_by   <- order_by
  if (!is.null(per_page))   query$per_page   <- per_page
  if (!is.null(after))      query$after      <- after
  if (!is.null(recurring))  query$recurring  <- tolower(as.character(recurring))
  if (!is.null(type))       query$type       <- type

  url <- httr2::url_modify(
    paste0(get_paddle_url(), "/prices"),
    query = query
  )

  get(url)
}

#' Create a Paddle Price
#'
#' Creates a new price for a product. The price defines how much customers pay, how often, and under what terms.
#'
#' @param description Internal description for your team. Required.
#' @param product_id ID of the product this price belongs to. Required.
#' @param unit_price A list with `amount` (string, lowest denomination, e.g. for 10 USD write 1000 (lowest denomination = cents)) and `currency_code` (3-letter ISO). Required.
#' @param type Type of item (one of `"standard"` and `"custom"`). Optional. Defaults to `"standard"`.
#' @param name Name of this price (shown at checkout/invoices). Optional.
#' @param billing_cycle List with `frequency` and `interval` (one of `"day"`, `"week"`, `"month"`, `"year"`). Optional. Omit for one-time pricing.
#' @param trial_period List with `frequency` and `interval` (one of `"day"`, `"week"`, `"month"`, `"year"`). Optional. Requires `billing_cycle`.
#' @param tax_mode One of `"account_setting"`, `"external"`, `"internal"`. Optional.
#' @param unit_price_overrides A list of overrides with `country_codes` (Supported two-letter ISO 3166-1 alpha-2 country code) and `unit_price` (same list as in unit_price parameter). Optional.
#' @param quantity List with `minimum` and `maximum` quantity limits. Optional.
#' @param custom_data Named list of custom metadata. Optional.
#'
#' @returns A list representing the created price entity and metadata.
#' @export
#' @examplesIf paddle_has_token()
#' set_paddle_mode("sandbox")
#' result <- paddle_create_price(
#'  description = "Standard monthly subscription",
#'  product_id = "pro_123",
#'  unit_price = list(
#'  amount = "1000",  # 10.00 USD
#'  currency_code = "USD"
#'  )
#' )
paddle_create_price <- function(description,
                                product_id,
                                unit_price,
                                type = NULL,
                                name = NULL,
                                billing_cycle = NULL,
                                trial_period = NULL,
                                tax_mode = NULL,
                                unit_price_overrides = NULL,
                                quantity = NULL,
                                custom_data = NULL) {
  if (missing(description) || !nzchar(description)) {
    stop("`description` is required and must be a non-empty string.", call. = FALSE)
  }

  if (missing(product_id) || !nzchar(product_id)) {
    stop("`product_id` is required and must be a non-empty string.", call. = FALSE)
  }

  if (missing(unit_price) || !is.list(unit_price) ||
      is.null(unit_price$amount) || is.null(unit_price$currency_code)) {
    stop("`unit_price` must be a list with `amount` and `currency_code`.", call. = FALSE)
  }

  if (!is.null(type)) {
    valid_types <- c("standard", "custom")
    if (!type %in% valid_types) {
      stop(sprintf(
        "`type` must be one of: %s",
        paste(valid_types, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(tax_mode)) {
    valid_tax_modes <- c("account_setting", "external", "internal")
    if (!tax_mode %in% valid_tax_modes) {
      stop(sprintf(
        "`tax_mode` must be one of: %s",
        paste(valid_tax_modes, collapse = ", ")
      ), call. = FALSE)
    }
  }

  body <- list(
    description = description,
    product_id = product_id,
    unit_price = unit_price
  )

  if (!is.null(type))                 body$type                  <- type
  if (!is.null(name))                 body$name                  <- name
  if (!is.null(billing_cycle))        body$billing_cycle         <- billing_cycle
  if (!is.null(trial_period))         body$trial_period          <- trial_period
  if (!is.null(tax_mode))             body$tax_mode              <- tax_mode
  if (!is.null(unit_price_overrides)) body$unit_price_overrides  <- unit_price_overrides
  if (!is.null(quantity))             body$quantity              <- quantity
  if (!is.null(custom_data))          body$custom_data           <- custom_data

  url <- paste0(get_paddle_url(), "/prices")

  post(url, body)
}


#' Update a Paddle Price
#'
#' Updates an existing price entity in Paddle using its ID.
#'
#' @param id ID of the price. Required.
#' @param description Internal description for your team. Required.
#' @param unit_price A list with `amount` (string, lowest denomination, e.g. for 10 USD write 1000 (lowest denomination = cents)) and `currency_code` (3-letter ISO). Required.
#' @param type Type of item (one of `"standard"` and `"custom"`). Optional. Defaults to `"standard"`.
#' @param name Name of this price (shown at checkout/invoices). Optional.
#' @param billing_cycle List with `frequency` and `interval` (one of `"day"`, `"week"`, `"month"`, `"year"`). Optional. Omit for one-time pricing.
#' @param trial_period List with `frequency` and `interval` (one of `"day"`, `"week"`, `"month"`, `"year"`). Optional. Requires `billing_cycle`.
#' @param tax_mode One of `"account_setting"`, `"external"`, `"internal"`. Optional.
#' @param unit_price_overrides A list of overrides with `country_codes` (Supported two-letter ISO 3166-1 alpha-2 country code) and `unit_price` (same list as in unit_price parameter). Optional.
#' @param quantity List with `minimum` and `maximum` quantity limits. Optional.
#' @param custom_data Named list of custom metadata. Optional.
#' @param status Status of the price (one of `"active"`, `"archived"`). Optional.
#'
#' @returns A list representing the updated price entity.
#' @export
#' @examplesIf paddle_has_token()
#' set_paddle_mode("sandbox")
#' result <- paddle_update_price(
#'   id = "pri_123",
#'   name = "Updated Price Name"
#' )
paddle_update_price <- function(id,
                                description = NULL,
                                type = NULL,
                                name = NULL,
                                billing_cycle = NULL,
                                trial_period = NULL,
                                tax_mode = NULL,
                                unit_price = NULL,
                                unit_price_overrides = NULL,
                                quantity = NULL,
                                status = NULL,
                                custom_data = NULL) {
  if (missing(id) || !nzchar(id)) {
    stop("`id` is required and must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(type)) {
    valid_types <- c("standard", "custom")
    if (!type %in% valid_types) {
      stop(sprintf(
        "`type` must be one of: %s",
        paste(valid_types, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(tax_mode)) {
    valid_tax_modes <- c("account_setting", "external", "internal")
    if (!tax_mode %in% valid_tax_modes) {
      stop(sprintf(
        "`tax_mode` must be one of: %s",
        paste(valid_tax_modes, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(status)) {
    valid_status <- c("active", "archived")
    if (!status %in% valid_status) {
      stop(sprintf(
        "`status` must be one of: %s",
        paste(valid_status, collapse = ", ")
      ), call. = FALSE)
    }
  }

  body <- list()

  if (!missing(description))           body$description           <- description
  if (!missing(type))                  body$type                  <- type
  if (!missing(name))                  body$name                  <- name
  if (!missing(billing_cycle))         body$billing_cycle         <- billing_cycle
  if (!missing(trial_period))          body$trial_period          <- trial_period
  if (!missing(tax_mode))              body$tax_mode              <- tax_mode
  if (!missing(unit_price))            body$unit_price            <- unit_price
  if (!missing(unit_price_overrides))  body$unit_price_overrides  <- unit_price_overrides
  if (!missing(quantity))              body$quantity              <- quantity
  if (!missing(status))                body$status                <- status
  if (!missing(custom_data))           body$custom_data           <- custom_data

  url <- paste0(get_paddle_url(), "/prices/", id)

  update(url, body)
}
