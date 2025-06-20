% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/products.R
\name{paddle_update_product}
\alias{paddle_update_product}
\title{Update a Paddle Product}
\usage{
paddle_update_product(
  id,
  name = NULL,
  description = NULL,
  type = NULL,
  tax_category = NULL,
  image_url = NULL,
  custom_data = NULL,
  status = NULL
)
}
\arguments{
\item{id}{The Paddle product ID (e.g., `"pro_abc123"`). Required.}

\item{name}{Updated product name. Optional.}

\item{description}{Updated product description. Optional (use `NULL` to clear).}

\item{type}{Character. Type of product (one of `"standard"` and `"custom"`). Optional, defaults to `"standard`.}

\item{tax_category}{Character vector of tax categories to filter. One of `"digital-goods"`, `"ebooks`, `"implementation-services"`, `"professional-services"`, `"saas"`, `"software-programming-services"`, `"standard"`, `"training-services"`, and `"website-hosting"`. Optional.}

\item{image_url}{HTTPS image URL. Optional (use `NULL` to clear).}

\item{custom_data}{Named list of key-value metadata. Optional (use `NULL` to clear).}

\item{status}{Character vector of statuses (e.g., `"active"`, `"archived"`). Optional.}
}
\value{
A list representing the updated product.
}
\description{
Updates an existing product using its Paddle ID. You can update any combination
of fields, such as name, description, tax category, type, image URL, custom metadata, and status.
}
\examples{
\dontshow{if (paddle_has_token()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
set_paddle_mode("sandbox")
result <- paddle_update_product(
  id = "pro_123",
  name = "Updated Product Name"
)
\dontshow{\}) # examplesIf}
}
