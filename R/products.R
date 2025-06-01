# --------------------------------------------------
# Products
# --------------------------------------------------

#' Retrieve Paddle Products
#'
#' Fetches a paginated list of products from the Paddle API. By default, only active products are returned.
#'
#' @param id Character vector of product IDs to match. Optional.
#' @param status Character vector of statuses to filter by (e.g., `"active"`, `"archived"`). Optional.
#' @param tax_category Character vector of tax categories to filter. Optional.
#' @param type Character string specifying product type. Optional.
#' @param include Character vector of related entities to include (e.g., `"prices"`). Optional.
#' @param after Character ID for pagination. Return products after this ID. Optional.
#' @param order_by Ordering string (e.g., `"id[ASC]"`, `"created_at[DESC]"`). Optional.
#' @param per_page Number of products per page (max 200). Optional, defaults to 50.
#'
#' @return A list containing product data and pagination metadata.
#' @export
#'
#' @examples
#' paddle_list_products(per_page = 10, include = "prices")
#' paddle_list_products(status = c("active", "archived"))
paddle_list_products <- function(id = NULL,
                                 status = NULL,
                                 tax_category = NULL,
                                 type = NULL,
                                 include = NULL,
                                 after = NULL,
                                 order_by = NULL,
                                 per_page = NULL) {
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
