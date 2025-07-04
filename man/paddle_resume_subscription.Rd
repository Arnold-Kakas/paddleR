% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subscriptions.R
\name{paddle_resume_subscription}
\alias{paddle_resume_subscription}
\title{Resume a paused or scheduled-to-pause subscription}
\usage{
paddle_resume_subscription(id, effective_from, on_resume = NULL)
}
\arguments{
\item{id}{Character. Required. Paddle subscription ID (e.g. "sub_abc123").}

\item{effective_from}{Character. Required. RFC 3339 datetime string when the resume should occur.}

\item{on_resume}{Character or NULL. Optional. One of `"start_new_billing_period"` or `"continue_billing_period"`.}
}
\value{
A list representing the updated subscription object.
}
\description{
Resumes a paused subscription immediately or at a specified date.
Also updates a scheduled pause if subscription is active.
}
\examples{
\dontshow{if (paddle_has_token()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
set_paddle_mode("sandbox")
result <- paddle_resume_subscription(
 id = "sub_123",
 effective_from = "2025-07-01T00:00:00Z"
)
\dontshow{\}) # examplesIf}
}
