% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customers.R
\name{paddle_create_customer}
\alias{paddle_create_customer}
\title{Create a Customer.}
\usage{
paddle_create_customer(email, name = NULL, custom_data = NULL, locale = NULL)
}
\arguments{
\item{email}{Character. Email address for the customer. Required.}

\item{name}{Character. Full name of the customer. Optional.}

\item{custom_data}{Named list of custom metadata. Optional.}

\item{locale}{Character. Locale string (IETF BCP 47). Optional, defaults to "en".}
}
\value{
A data frame with the new customer info.
}
\description{
If successful, your response includes a copy of the new customer entity.
}
\examples{
\dontshow{if (paddle_has_token()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
set_paddle_mode("sandbox")
result <- paddle_create_customer(
  email = "test@example.com",
  name = "Test User",
  custom_data = list(plan = "demo"),
  locale = "en-US"
)
\dontshow{\}) # examplesIf}
}
