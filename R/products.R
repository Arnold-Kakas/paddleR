# --------------------------------------------------
# Products
# --------------------------------------------------

#' Retrieve Paddle Products
#'
#' Fetches a paginated list of products from the Paddle API. By default, only active products are returned.
#'
#' @param id Character vector of product IDs to match. Optional.
#' @param status Character vector of statuses to filter by (e.g., `"active"`, `"archived"`). Optional.
#' @param tax_category Character vector of tax categories to filter. One of `"digital-goods"`, `"ebooks`, `"implementation-services"`, `"professional-services"`, `"saas"`, `"software-programming-services"`, `"standard"`, `"training-services"`, and `"website-hosting"`. Optional.
#' @param type Character. Type of product (one of `"standard"` and `"custom"`). Optional.
#' @param include Character vector of related entities - default `NULL` or `"prices"`. Optional.
#' @param after Character. Return entities after the specified Paddle ID when working with paginated endpoints.. Optional.
#' @param order_by Character. Ordering string (e.g., `"id[ASC]"`). Valid fields for ordering: `"created_at"`, `"custom_data"`, `"id"`, `"description"`, `"image_url"`, `"name"`, `"status"`, `"tax_category"`, and `"updated_at"`. Valid directions `"[ASC]"` and `"[DESC]"` Optional. Optional.
#' @param per_page Number of products per page (max 200). Optional, defaults to 50.
#' @returns A list containing product data and pagination metadata.
#' @export
#' @examplesIf paddle_has_token()
#' set_paddle_mode("sandbox")
#' result <- paddle_list_products()
paddle_list_products <- function(id = NULL,
                                 status = NULL,
                                 tax_category = NULL,
                                 type = NULL,
                                 include = NULL,
                                 after = NULL,
                                 order_by = NULL,
                                 per_page = NULL) {

  if (!is.null(order_by)) {
    valid_order_entities <- c("created_at[ASC]",
                              "custom_data[ASC]",
                              "id[ASC]",
                              "description[ASC]",
                              "image_url[ASC]",
                              "name[ASC]",
                              "status[ASC]",
                              "tax_category[ASC]",
                              "updated_at[ASC]",
                              "created_at[DESC]",
                              "custom_data[DESC]",
                              "id[DESC]",
                              "description[DESC]",
                              "image_url[DESC]",
                              "name[DESC]",
                              "status[DESC]",
                              "tax_category[DESC]",
                              "updated_at[DESC]")
    if (!order_by %in% valid_order_entities) {
      stop(sprintf(
        "`order_by` must be one of: %s",
        paste(valid_order_entities, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (!is.null(tax_category)) {
    tax_categories <- c("digital-goods", "ebooks", "implementation-services",
                        "professional-services", "saas", "software-programming-services",
                        "standard", "training-services", "website-hosting")
    if (!tax_category %in% tax_categories) {
      stop(sprintf(
        "`tax_category` must be one of: %s",
        paste(tax_categories, collapse = ", ")
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

  if (!is.null(per_page)) {
    if (!is.numeric(per_page) || per_page < 1 || per_page > 200) {
      stop("`per_page` must be between 1 and 200.", call. = FALSE)
    }
  }

  query <- list()

  if (!is.null(id))           query$id           <- paste(id, collapse = ",")
  if (!is.null(status))       query$status       <- paste(status, collapse = ",")
  if (!is.null(tax_category)) query$tax_category <- paste(tax_category, collapse = ",")
  if (!is.null(type))         query$type         <- type
  if (!is.null(include))      query$include      <- paste(include, collapse = ",")
  if (!is.null(after))        query$after        <- after
  if (!is.null(order_by))     query$order_by     <- order_by
  if (!is.null(per_page))     query$per_page     <- per_page

  url <- httr2::url_modify(paste0(get_paddle_url(), "/products"), query = query)

  get(url)
}

#' Create a Paddle Product
#'
#' Creates a new product in Paddle. You must specify a name and tax category
#' (chosen from a predefined set supported by Paddle).
#'
#' @param name Name of the product. Required.
#' @param tax_category Character vector of tax categories to filter. One of `"digital-goods"`, `"ebooks`, `"implementation-services"`, `"professional-services"`, `"saas"`, `"software-programming-services"`, `"standard"`, `"training-services"`, and `"website-hosting"`. Required.
#' @param description Short description of the product. Optional.
#' @param type Character. Type of product (one of `"standard"` and `"custom"`). Optional, defaults to `"standard`.
#' @param image_url HTTPS URL for the product image (1:1 recommended). Optional.
#' @param custom_data Named list of your own structured key-value metadata. Optional.
#'
#' @returns A list representing the newly created product.
#' @export
#' @examples
#' \dontrun{ # do not run to not create products in users Paddle accounts
#' set_paddle_mode("sandbox")
#' result <- paddle_create_product(
#'  name = "My Product",
#'  tax_category = "digital-goods",
#'  description = "A great product"
#'  )
#' }
paddle_create_product <- function(name,
                                  tax_category,
                                  description = NULL,
                                  type = NULL,
                                  image_url = NULL,
                                  custom_data = NULL) {
  if (missing(name) || !nzchar(name)) {
    stop("`name` is required and must be a non-empty string.", call. = FALSE)
  }

  valid_tax_categories <- c(
    "digital-goods",
    "ebooks",
    "implementation-services",
    "professional-services",
    "saas",
    "software-programming-services",
    "standard",
    "training-services",
    "website-hosting"
  )

  if (missing(tax_category) || !nzchar(tax_category)) {
    stop("`tax_category` is required and must be a non-empty string.", call. = FALSE)
  }

  if (!tax_category %in% valid_tax_categories) {
    stop(
      sprintf("`tax_category` must be one of: %s", paste(valid_tax_categories, collapse = ", ")),
      call. = FALSE
    )
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

  body <- list(
    name = name,
    tax_category = tax_category
  )

  if (!is.null(description))  body$description  <- description
  if (!is.null(type))         body$type         <- type
  if (!is.null(image_url))    body$image_url    <- image_url
  if (!is.null(custom_data))  body$custom_data  <- custom_data

  url <- paste0(get_paddle_url(), "/products")

  post(url, body)
}

#' Update a Paddle Product
#'
#' Updates an existing product using its Paddle ID. You can update any combination
#' of fields, such as name, description, tax category, type, image URL, custom metadata, and status.
#'
#' @param id The Paddle product ID (e.g., `"pro_abc123"`). Required.
#' @param name Updated product name. Optional.
#' @param description Updated product description. Optional (use `NULL` to clear).
#' @param type Character. Type of product (one of `"standard"` and `"custom"`). Optional, defaults to `"standard`.
#' @param tax_category Character vector of tax categories to filter. One of `"digital-goods"`, `"ebooks`, `"implementation-services"`, `"professional-services"`, `"saas"`, `"software-programming-services"`, `"standard"`, `"training-services"`, and `"website-hosting"`. Optional.
#' @param image_url HTTPS image URL. Optional (use `NULL` to clear).
#' @param custom_data Named list of key-value metadata. Optional (use `NULL` to clear).
#' @param status Character vector of statuses (e.g., `"active"`, `"archived"`). Optional.
#'
#' @returns A list representing the updated product.
#' @export
#' @examples
#' \dontrun{ # needs valid product key
#' set_paddle_mode("sandbox")
#' result <- paddle_update_product(
#'   id = "pro_123",
#'   name = "Updated Product Name"
#' )
#' }
paddle_update_product <- function(id,
                                  name = NULL,
                                  description = NULL,
                                  type = NULL,
                                  tax_category = NULL,
                                  image_url = NULL,
                                  custom_data = NULL,
                                  status = NULL) {

  if (missing(id) || !nzchar(id)) {
    stop("`id` is required and must be a non-empty string.", call. = FALSE)
  }

  if (!is.null(tax_category)) {
    tax_categories <- c("digital-goods", "ebooks", "implementation-services",
                        "professional-services", "saas", "software-programming-services",
                        "standard", "training-services", "website-hosting")
    if (!tax_category %in% tax_categories) {
      stop(sprintf(
        "`tax_category` must be one of: %s",
        paste(tax_categories, collapse = ", ")
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

  body <- list()

  if (!missing(name))         body$name         <- name
  if (!missing(description))  body$description  <- description
  if (!missing(type))         body$type         <- type
  if (!missing(tax_category)) body$tax_category <- tax_category
  if (!missing(image_url))    body$image_url    <- image_url
  if (!missing(custom_data))  body$custom_data  <- custom_data
  if (!missing(status))       body$status       <- status

  url <- paste0(get_paddle_url(), "/products/", id)

  update(url, body)
}
