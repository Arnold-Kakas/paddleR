# --------------------------------------------------
# subscriptions
# --------------------------------------------------

#' Get Paddle Events
#'
#' Retrieves a list of event entities from Paddle. You can optionally filter by event type.
#'
#' @param after Character. Return entities after the specified Paddle ID when working with paginated endpoints. Optional.
#' @param order_by Character. Optional. Must be in the form "id[ASC]" or "id[DESC]".
#' @param per_page Integer. Optional. Max 200, defaults to 50.
#' @returns A list of events returned by the Paddle API.
#' @export
#' @examplesIf paddle_has_token()
#' set_paddle_mode("sandbox")
#' result <- paddle_get_events(
#'   per_page = 2
#' )
paddle_get_events <- function(after = NULL,
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

  query <- drop_nulls(list(
    after = after,
    order_by = order_by,
    per_page = per_page))

  url <- httr2::url_modify(paste0(get_paddle_url(), "/events"), query = query)

  get(link = url)
}
