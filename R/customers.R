# --------------------------------------------------
# Customers
# --------------------------------------------------

#' Create a Customer.
#'
#' If successful, your response includes a copy of the new customer entity.
#'
#' @param email Character. Email address for the customer. Required.
#' @param name Character. Full name of the customer. Optional.
#' @param custom_data Named list of custom metadata. Optional.
#' @param locale Character. Locale string (IETF BCP 47). Optional, defaults to "en".
#'
#' @returns A data frame with the new customer info.
#'
#' @export
#' @examples
#' \dontrun{ # would result in error if customer already exists
#' set_paddle_mode("sandbox")
#' paddle_create_customer(
#'   email = "test@example.com",
#'   name = "Test User",
#'   custom_data = list(plan = "demo"),
#'   locale = "en-US"
#' )
#' }
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
#' @param id Character vector of Paddle customer IDs. Optional.
#' @param status Characte vector of statuses to filter by (`"active"` or `"archived"`). Optional.
#' @param after Character. Return entities after the specified Paddle ID when working with paginated endpoints. Optional.
#' @param order_by Character. Order results by `"id[ASC]"` or `"id[DESC]"`. Optional.
#' @param per_page Integer. Number of results per page (1–200). Optional, defaults to 50.
#' @param search Character. Search term (one of `"id"`, `"name"`, `"email"`). Optional.
#'
#' @returns A list with customer data and pagination info.
#' @export
#' @examplesIf paddle_has_token()
#' set_paddle_mode("sandbox")
#' result <- paddle_list_customers()

paddle_list_customers <- function(email = NULL,
                                     id = NULL,
                                     status = NULL,
                                     after = NULL,
                                     order_by = NULL,
                                     per_page = NULL,
                                     search = NULL) {

  if (!is.null(search)) {
    search_terms <- c("id", "name", "email")
    if (!search %in% search_terms) {
      stop(sprintf(
        "`search` must be one of: %s",
        paste(search_terms, collapse = ", ")
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

  if (!is.null(order_by)) {
    valid_orders <- c("id[ASC]", "id[DESC]")
    if (!order_by %in% valid_orders) {
      stop(sprintf(
        "`order_by` must be one of: %s",
        paste(valid_orders, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(per_page)) {
    if (!is.numeric(per_page) || per_page < 1 || per_page > 200) {
      stop("`per_page` must be between 1 and 200.", call. = FALSE)
    }
  }

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
#' @param id Character. Paddle customer ID (required).
#' @param name Character. Full name of the customer. Optional (can be `NULL` to remove).
#' @param email Character. Email address of the customer. Optional.
#' @param status Character. Status of the customer (`"active"` or `"archived"`). Optional.
#' @param custom_data Named list of custom metadata. Optional (can be `NULL` to remove).
#' @param locale Character. Locale string (IETF BCP 47). Optional.
#'
#' @returns A list with the updated customer info.
#' @export
#' @examples
#' \dontrun{ # needs existing customer key
#' set_paddle_mode("sandbox")
#' # Requires a valid id
#' result <- paddle_update_customer(
#'   id = "ctm_123",
#'   name = "Updated Name",
#'   status = "active"
#' )
#' }

paddle_update_customer <- function(id,
                                   name = NULL,
                                   email = NULL,
                                   status = NULL,
                                   custom_data = NULL,
                                   locale = NULL) {
  if (missing(id) || !nzchar(id)) {
    stop("`id` is required and must be a non-empty string.", call. = FALSE)
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

  if (!is.null(name))        body$name <- name
  if (!is.null(email))       body$email <- email
  if (!is.null(status))      body$status <- status
  if (!missing(custom_data)) body$custom_data <- custom_data  # allow NULL
  if (!is.null(locale))      body$locale <- locale

  url <- paste0(get_paddle_url(), "/customers/", id)

  update(url, body)
}

#' Generate an Authentication Token for a Paddle Customer
#'
#' Creates a temporary authentication token for a customer.
#' The token allows Paddle.js to present saved payment methods at checkout.
#'
#' @param id Paddle customer ID (required).
#'
#' @returns A list containing the auth token and expiry details.
#' @export
#' @examples
#' \dontrun{ # needs existing customer key
#' set_paddle_mode("sandbox")
#' result <- paddle_generate_auth_token("ctm_123")
#' }

paddle_generate_auth_token <- function(id) {
  if (missing(id) || !nzchar(id)) {
    stop("`id` is required and must be a non-empty string.", call. = FALSE)
  }

  url <- paste0(get_paddle_url(), "/customers/", id, "/auth-token")

  post_excl_body(url)  # no body, just POST the path
}


#' List Credit Balances for a Customer
#'
#' Retrieves credit balances across all currencies for a specific customer from the Paddle API.
#'
#' @param id Character. Paddle customer ID (e.g., "ctm_123"). Required.
#' @param currency_code Optional character vector of ISO 4217 currency codes to filter results.
#'
#' @return A list of credit balances by currency with available, reserved, and used totals.
#' @export
#' @examples
#' \dontrun{ # needs existing customer key
#' set_paddle_mode("sandbox")
#' result <- paddle_list_credit_balances(
#'   id = "ctm_123",
#'   currency_code = c("USD", "EUR")
#' )
#' }
paddle_list_credit_balances <- function(id, currency_code = NULL) {
  if (missing(id) || !is.character(id) || nchar(id) == 0) {
    stop("`id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(currency_code)) {
    valid_pattern <- "^[A-Z]{3}$"
    invalid <- currency_code[!grepl(valid_pattern, currency_code)]
    if (length(invalid)) {
      stop(sprintf("Invalid currency code(s): %s", paste(invalid, collapse = ", ")), call. = FALSE)
    }
  }

  query <- list()
  if (!is.null(currency_code)) {
    query$currency_code <- paste(currency_code, collapse = ",")
  }

  url <- httr2::url_modify(
    paste0(get_paddle_url(), "/customers/", id, "/credit-balances"),
    query = query
  )

  get(url)
}

# --------------------------------------------------
# Customer's Addresses
# --------------------------------------------------

#' List Addresses for a Customer
#'
#' Fetches a paginated list of addresses associated with a given customer from the Paddle API.
#' By default, only active addresses are returned.
#'
#' @param id Character. Paddle customer ID (e.g., "ctm_abc123"). Required.
#' @param address_id Character vector of address IDs (e.g., "add_123"). Optional.
#' @param status Character vector. Optional. Valid: "active", "archived".
#' @param after Character. Paddle ID to start listing after (for pagination). Optional.
#' @param order_by Character. One of `"id[ASC]"`, `"id[DESC]"`. Optional.
#' @param per_page Integer. Number of results per page (max 200). Optional.
#' @param search Character. Search query across most address fields. Optional. Cannot match `status`, `created_at`, or `updated_at`.
#'
#' @returns A list containing address data and pagination metadata.
#' @export
#' @examples
#' \dontrun{ # needs existing customer key
#' set_paddle_mode("sandbox")
#' result <- paddle_list_customer_addresses(id = "ctm_123")
#' }
paddle_list_customer_addresses <- function(id,
                                           address_id = NULL,
                                           status = NULL,
                                           after = NULL,
                                           order_by = NULL,
                                           per_page = NULL,
                                           search = NULL) {

  if (missing(id) || !is.character(id) || nchar(id) == 0) {
    stop("`id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(order_by)) {
    valid_order_by <- c("id[ASC]", "id[DESC]")
    if (!order_by %in% valid_order_by) {
      stop(sprintf("`order_by` must be one of: %s", paste(valid_order_by, collapse = ", ")), call. = FALSE)
    }
  }

  if (!is.null(status)) {
    valid_status <- c("active", "archived")
    if (any(!status %in% valid_status)) {
      stop(sprintf("`status` must be one of: %s", paste(valid_status, collapse = ", ")), call. = FALSE)
    }
  }

  query <- list()

  if (!is.null(address_id))       query$address_id       <- paste(address_id, collapse = ",")
  if (!is.null(status))   query$status   <- paste(status, collapse = ",")
  if (!is.null(after))    query$after    <- after
  if (!is.null(order_by)) query$order_by <- order_by
  if (!is.null(per_page)) query$per_page <- per_page
  if (!is.null(search))   query$search   <- search  # Server will ignore disallowed fields

  url <- httr2::url_modify(
    paste0(get_paddle_url(), "/customers/", id, "/addresses"),
    query = query
  )

  get(url)
}

#' Create an Address for a Customer
#'
#' Creates a new address for a given customer in the Paddle API.
#'
#' @param id Character. Paddle customer ID (e.g., "ctm_123"). Required.
#' @param country_code Character. Two-letter ISO 3166-1 alpha-2 country code. Required.
#' @param description Character. Optional description for internal reference.
#' @param first_line Character. Optional. First line of address.
#' @param second_line Character. Optional. Second line of address.
#' @param city Character. Optional. City name.
#' @param postal_code Character. Optional. ZIP or postal code. Required for some countries.
#' @param region Character. Optional. Region, state, or county.
#' @param custom_data Named list. Optional custom metadata.
#'
#' @returns A list containing the created address entity and metadata.
#' @export
#' @examples
#' \dontrun{ # needs existing customer key
#' result <- paddle_create_customer_address(
#'   id = "ctm_123",
#'   country_code = "US",
#'   city = "New York",
#'   postal_code = "10001"
#' )
#' }

paddle_create_customer_address <- function(id,
                                           country_code,
                                           description = NULL,
                                           first_line = NULL,
                                           second_line = NULL,
                                           city = NULL,
                                           postal_code = NULL,
                                           region = NULL,
                                           custom_data = NULL) {
  if (missing(id) || !is.character(id) || nchar(id) == 0) {
    stop("`id` must be a non-empty string.", call. = FALSE)
  }

  if (missing(country_code) || !is.character(country_code) || !grepl("^[A-Z]{2}$", country_code)) {
    stop("`country_code` must be a valid 2-letter uppercase ISO 3166-1 alpha-2 code.", call. = FALSE)
  }

  body <- list(
    country_code = country_code
  )

  if (!is.null(description))  body$description  <- description
  if (!is.null(first_line))   body$first_line   <- first_line
  if (!is.null(second_line))  body$second_line  <- second_line
  if (!is.null(city))         body$city         <- city
  if (!is.null(postal_code))  body$postal_code  <- postal_code
  if (!is.null(region))       body$region       <- region
  if (!is.null(custom_data))  body$custom_data  <- custom_data

  url <- paste0(get_paddle_url(), "/customers/", id, "/addresses")

  post(url, body)
}

#' Update a Customer Address
#'
#' Updates an address for a specific customer using Paddle's API.
#'
#' @param id Character. Paddle customer ID (e.g., "ctm_123"). Required.
#' @param address_id Character. Paddle address ID (e.g., "add_456"). Required.
#' @param country_code Character. Optional. Two-letter ISO 3166-1 alpha-2 country code.
#' @param description Character. Optional. Internal reference.
#' @param first_line Character. Optional. First line of address.
#' @param second_line Character. Optional. Second line of address.
#' @param city Character. Optional. City name.
#' @param postal_code Character. Optional. ZIP or postal code.
#' @param region Character. Optional. Region, state, or county.
#' @param custom_data Named list. Optional structured metadata.
#' @param status Character. Status of the customer (`"active"` or `"archived"`). Optional.
#'
#' @returns A list containing the updated address entity and metadata.
#' @export
#' @examples
#' \dontrun{ # needs existing customer and address key
#' set_paddle_mode("sandbox")
#' result <- paddle_update_customer_address(
#'   id = "ctm_123",
#'   address_id = "add_123",
#'   city = "San Francisco",
#'   region = "CA"
#' )
#' }

paddle_update_customer_address <- function(id,
                                           address_id,
                                           country_code = NULL,
                                           description = NULL,
                                           first_line = NULL,
                                           second_line = NULL,
                                           city = NULL,
                                           postal_code = NULL,
                                           region = NULL,
                                           custom_data = NULL,
                                           status = NULL) {

  if (missing(id) || !is.character(id) || nchar(id) == 0) {
    stop("`id` must be a non-empty string.", call. = FALSE)
  }

  if (missing(address_id) || !is.character(address_id) || nchar(address_id) == 0) {
    stop("`address_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(country_code) && !grepl("^[A-Z]{2}$", country_code)) {
    stop("`country_code` must be a valid 2-letter ISO 3166-1 alpha-2 code.", call. = FALSE)
  }

  if (!is.null(status)) {
    valid_status <- c("active", "archived")
    if (any(!status %in% valid_status)) {
      stop(sprintf("`status` must be one of: %s", paste(valid_status, collapse = ", ")), call. = FALSE)
    }
  }

  body <- list()

  if (!is.null(country_code)) body$country_code <- country_code
  if (!is.null(description))  body$description  <- description
  if (!is.null(first_line))   body$first_line   <- first_line
  if (!is.null(second_line))  body$second_line  <- second_line
  if (!is.null(city))         body$city         <- city
  if (!is.null(postal_code))  body$postal_code  <- postal_code
  if (!is.null(region))       body$region       <- region
  if (!is.null(custom_data))  body$custom_data  <- custom_data
  if (!is.null(status))       body$status       <- status

  url <- paste0(get_paddle_url(), "/customers/", id, "/addresses/", address_id)

  update(url, body)
}


# --------------------------------------------------
# Customer's Businesses
# --------------------------------------------------

#' List Businesses for a Customer
#'
#' Retrieves a paginated list of businesses associated with a given customer from the Paddle API.
#' By default, only active businesses are returned.
#'
#' @param id Character. Paddle customer ID (e.g., "ctm_abc123"). Required.
#' @param business_id Character vector of business IDs (e.g., "biz_123"). Optional.
#' @param status Character. Status of the customer (`"active"` or `"archived"`). Optional.
#' @param after Character. Pagination cursor: return entities after this ID. Optional.
#' @param order_by Character. Must be one of `"id[ASC]"`, `"id[DESC]"`. Optional.
#' @param per_page Integer. Max number of results (default: 50, max: 200). Optional.
#' @param search Character. Optional search query across business fields and contacts (except status, created_at, updated_at).
#'
#' @returns A list containing business data and pagination metadata.
#' @export
#' @examples
#' \dontrun{ # needs existing customer key
#' set_paddle_mode("sandbox")
#' result <- paddle_list_customer_businesses(id = "ctm_123")
#' }

paddle_list_customer_businesses <- function(id,
                                            business_id = NULL,
                                            status = NULL,
                                            after = NULL,
                                            order_by = NULL,
                                            per_page = NULL,
                                            search = NULL) {

  if (missing(id) || !is.character(id) || nchar(id) == 0) {
    stop("`id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(order_by)) {
    valid_order_by <- c("id[ASC]", "id[DESC]")
    if (!order_by %in% valid_order_by) {
      stop(sprintf("`order_by` must be one of: %s", paste(valid_order_by, collapse = ", ")), call. = FALSE)
    }
  }

  if (!is.null(status)) {
    valid_status <- c("active", "archived")
    if (any(!status %in% valid_status)) {
      stop(sprintf("`status` must be one of: %s", paste(valid_status, collapse = ", ")), call. = FALSE)
    }
  }

  query <- list()

  if (!is.null(business_id))       query$business_id       <- paste(business_id, collapse = ",")
  if (!is.null(status))   query$status   <- paste(status, collapse = ",")
  if (!is.null(after))    query$after    <- after
  if (!is.null(order_by)) query$order_by <- order_by
  if (!is.null(per_page)) query$per_page <- per_page
  if (!is.null(search))   query$search   <- search  # Search applies to most fields and contacts

  url <- httr2::url_modify(
    paste0(get_paddle_url(), "/customers/", id, "/businesses"),
    query = query
  )

  get(url)
}

#' Create a Business for a Customer
#'
#' Creates a new business entity associated with a specific customer in the Paddle API.
#'
#' @param id Character. Paddle customer ID (e.g., "ctm_123"). Required.
#' @param name Character. Name of the business. Required.
#' @param company_number Character. Optional company number.
#' @param tax_identifier Character. Optional tax/VAT ID.
#' @param contacts List of contact objects (each a named list with `"name"` and required `"email"`). Optional.
#' @param custom_data Named list of additional custom fields. Optional.
#'
#' @returns A list containing the created business entity and metadata.
#' @export
#' @examples
#' \dontrun{ # needs existing customer key
#' result <- paddle_create_customer_business(
#'   id = "ctm_123",
#'   name = "Acme Inc.",
#'   tax_identifier = "123456789",
#'   contacts = list(list(email = "ceo@acme.com") )
#' )
#' }

paddle_create_customer_business <- function(id,
                                            name,
                                            company_number = NULL,
                                            tax_identifier = NULL,
                                            contacts = NULL,
                                            custom_data = NULL) {
  if (missing(id) || !is.character(id) || nchar(id) == 0) {
    stop("`id` must be a non-empty string.", call. = FALSE)
  }

  if (missing(name) || !is.character(name) || nchar(name) == 0) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(contacts)) {
    if (!is.list(contacts) || any(!vapply(contacts, function(x) is.list(x) && "email" %in% names(x), logical(1)))) {
      stop("Each contact must be a list containing at least an `email` field.", call. = FALSE)
    }
    if (any(!vapply(contacts, function(x) grepl(".+@.+\\..+", x$email), logical(1)))) {
      stop("Each contact must contain a valid email address.", call. = FALSE)
    }
  }

  if (!is.null(tax_identifier) && !grepl("^[0-9]+$", tax_identifier)) {
    stop("`tax_identifier` must contain only digits.", call. = FALSE)
  }

  body <- list(name = name)

  if (!is.null(company_number)) body$company_number <- company_number
  if (!is.null(tax_identifier)) body$tax_identifier <- tax_identifier
  if (!is.null(contacts))       body$contacts <- contacts
  if (!is.null(custom_data))    body$custom_data <- custom_data

  url <- paste0(get_paddle_url(), "/customers/", id, "/businesses")

  post(url, body)
}


#' Update a Business for a Customer
#'
#' Updates a business entity linked to a specific customer using the Paddle API.
#'
#' @param id Character. Paddle customer ID (e.g., "ctm_123"). Required.
#' @param business_id Character. Paddle business ID (e.g., "biz_456"). Required.
#' @param name Character. Updated name of the business. Optional.
#' @param company_number Character. Updated company number. Optional.
#' @param tax_identifier Character. Numeric VAT/tax ID. Optional.
#' @param status Character. Status of the customer (one of `"active"` or `"archived"`). Optional.
#' @param contacts List of contact objects (named list with at least `"email"`). Optional.
#' @param custom_data Named list of custom metadata. Optional.
#'
#' @returns A list containing the updated business entity and metadata.
#' @export
#' @examples
#' \dontrun{ # needs existing customer and business keys
#' set_paddle_mode("sandbox")
#' result <- paddle_update_customer_business(
#'   id = "ctm_123",
#'   business_id = "biz_123",
#'   name = "Acme International"
#' )
#' }
paddle_update_customer_business <- function(id,
                                            business_id,
                                            name = NULL,
                                            company_number = NULL,
                                            tax_identifier = NULL,
                                            status = NULL,
                                            contacts = NULL,
                                            custom_data = NULL) {
  if (missing(id) || !nzchar(id)) {
    stop("`id` must be a non-empty string.", call. = FALSE)
  }

  if (missing(business_id) || !nzchar(business_id)) {
    stop("`business_id` must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(tax_identifier) && !grepl("^[0-9]+$", tax_identifier)) {
    stop("`tax_identifier` must contain only digits.", call. = FALSE)
  }

  if (!is.null(status)) {
    valid_status <- c("active", "archived")
    if (any(!status %in% valid_status)) {
      stop(sprintf("`status` must be one of: %s", paste(valid_status, collapse = ", ")), call. = FALSE)
    }
  }

  if (!is.null(contacts)) {
    if (!is.list(contacts) || length(contacts) == 0) {
      stop("`contacts` must be a non-empty list of named lists.", call. = FALSE)
    }
    for (c in contacts) {
      if (!("email" %in% names(c)) || !grepl(".+@.+\\..+", c$email)) {
        stop("Each contact must include a valid `email` field.", call. = FALSE)
      }
    }
  }

  body <- list()

  if (!is.null(name))            body$name <- name
  if (!is.null(company_number)) body$company_number <- company_number
  if (!is.null(tax_identifier)) body$tax_identifier <- tax_identifier
  if (!is.null(status))         body$status <- status
  if (!is.null(contacts))       body$contacts <- contacts
  if (!is.null(custom_data))    body$custom_data <- custom_data

  url <- paste0(get_paddle_url(), "/customers/", id, "/businesses/", business_id)

  update(url, body)
}
