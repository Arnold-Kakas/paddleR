% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subscriptions.R
\name{paddle_activate_trial_subscription}
\alias{paddle_activate_trial_subscription}
\title{Activate a trialing subscription}
\usage{
paddle_activate_trial_subscription(id)
}
\arguments{
\item{id}{Character. Required. The Paddle subscription ID (e.g. "sub_abc123").}
}
\value{
A list with updated subscription entity and metadata.
}
\description{
Activates a trialing subscription using its ID. Only automatically-collected subscriptions
with status = "trialing" can be activated.
}
\details{
This triggers an immediate charge and recalculates billing dates from activation time.
}
\examples{
\dontrun{ # needs valid subscription key
set_paddle_mode("sandbox")
paddle_activate_trial_subscription(
 id = "sub_123" # subscription must be trialing
 )
 }
}
