% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customers.R
\name{paddle_update_customer}
\alias{paddle_update_customer}
\title{Update a Paddle Customer}
\usage{
paddle_update_customer(
  id,
  name = NULL,
  email = NULL,
  status = NULL,
  custom_data = NULL,
  locale = NULL
)
}
\arguments{
\item{id}{Character. Paddle customer ID (required).}

\item{name}{Character. Full name of the customer. Optional (can be `NULL` to remove).}

\item{email}{Character. Email address of the customer. Optional.}

\item{status}{Character. Status of the customer (`"active"` or `"archived"`). Optional.}

\item{custom_data}{Named list of custom metadata. Optional (can be `NULL` to remove).}

\item{locale}{Character. Locale string (IETF BCP 47). Optional.}
}
\value{
A list with the updated customer info.
}
\description{
Updates an existing customer using their Paddle ID.
}
\examples{
\dontshow{if (paddle_has_token()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
set_paddle_mode("sandbox")
# Requires a valid id
result <- paddle_update_customer(
  id = "ctm_123",
  name = "Updated Name",
  status = "active"
)
\dontshow{\}) # examplesIf}
}
