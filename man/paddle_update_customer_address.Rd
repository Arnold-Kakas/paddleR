% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customers.R
\name{paddle_update_customer_address}
\alias{paddle_update_customer_address}
\title{Update a Customer Address}
\usage{
paddle_update_customer_address(
  id,
  address_id,
  country_code = NULL,
  description = NULL,
  first_line = NULL,
  second_line = NULL,
  city = NULL,
  postal_code = NULL,
  region = NULL,
  custom_data = NULL,
  status = NULL
)
}
\arguments{
\item{id}{Character. Paddle customer ID (e.g., "ctm_123"). Required.}

\item{address_id}{Character. Paddle address ID (e.g., "add_456"). Required.}

\item{country_code}{Character. Optional. Two-letter ISO 3166-1 alpha-2 country code.}

\item{description}{Character. Optional. Internal reference.}

\item{first_line}{Character. Optional. First line of address.}

\item{second_line}{Character. Optional. Second line of address.}

\item{city}{Character. Optional. City name.}

\item{postal_code}{Character. Optional. ZIP or postal code.}

\item{region}{Character. Optional. Region, state, or county.}

\item{custom_data}{Named list. Optional structured metadata.}

\item{status}{Character. Status of the customer (`"active"` or `"archived"`). Optional.}
}
\value{
A list containing the updated address entity and metadata.
}
\description{
Updates an address for a specific customer using Paddle's API.
}
\examples{
\dontshow{if (paddle_has_token()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
set_paddle_mode("sandbox")
result <- paddle_update_customer_address(
  id = "ctm_123",
  address_id = "add_123",
  city = "San Francisco",
  region = "CA"
)
\dontshow{\}) # examplesIf}
}
