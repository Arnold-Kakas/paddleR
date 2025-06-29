% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/products.R
\name{paddle_list_products}
\alias{paddle_list_products}
\title{Retrieve Paddle Products}
\usage{
paddle_list_products(
  id = NULL,
  status = NULL,
  tax_category = NULL,
  type = NULL,
  include = NULL,
  after = NULL,
  order_by = NULL,
  per_page = NULL
)
}
\arguments{
\item{id}{Character vector of product IDs to match. Optional.}

\item{status}{Character vector of statuses to filter by (e.g., `"active"`, `"archived"`). Optional.}

\item{tax_category}{Character vector of tax categories to filter. One of `"digital-goods"`, `"ebooks`, `"implementation-services"`, `"professional-services"`, `"saas"`, `"software-programming-services"`, `"standard"`, `"training-services"`, and `"website-hosting"`. Optional.}

\item{type}{Character. Type of product (one of `"standard"` and `"custom"`). Optional.}

\item{include}{Character vector of related entities - default `NULL` or `"prices"`. Optional.}

\item{after}{Character. Return entities after the specified Paddle ID when working with paginated endpoints.. Optional.}

\item{order_by}{Character. Ordering string (e.g., `"id[ASC]"`). Valid fields for ordering: `"created_at"`, `"custom_data"`, `"id"`, `"description"`, `"image_url"`, `"name"`, `"status"`, `"tax_category"`, and `"updated_at"`. Valid directions `"[ASC]"` and `"[DESC]"` Optional. Optional.}

\item{per_page}{Number of products per page (max 200). Optional, defaults to 50.}
}
\value{
A list containing product data and pagination metadata.
}
\description{
Fetches a paginated list of products from the Paddle API. By default, only active products are returned.
}
\examples{
\dontshow{if (paddle_has_token()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
set_paddle_mode("sandbox")
result <- paddle_list_products()
\dontshow{\}) # examplesIf}
}
