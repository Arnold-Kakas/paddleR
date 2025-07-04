% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paddle_mode.R
\name{get_paddle_url}
\alias{get_paddle_url}
\title{Get Paddle URL
This function retrieves the base URL for Paddle API requests based on the current mode.}
\usage{
get_paddle_url()
}
\value{
The base URL for Paddle API requests.
}
\description{
Get Paddle URL
This function retrieves the base URL for Paddle API requests based on the current mode.
}
\examples{
\dontshow{if (paddle_has_token()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
get_paddle_url()  # returns token for current mode
\dontshow{\}) # examplesIf}
}
