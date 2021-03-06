% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregress.R
\name{agg_lm}
\alias{agg_lm}
\title{Fitting Linear Models with Aggregated Data}
\usage{
agg_lm(
  formula,
  data,
  subset,
  weights,
  na.action,
  method = "qr",
  model = TRUE,
  x = FALSE,
  y = FALSE,
  y_squared = NULL,
  qr = TRUE,
  singular.ok = TRUE,
  contrasts = NULL,
  offset,
  ...
)
}
\value{
\code{agg_lm} returns an object of class "\code{agg_lm}",
the analogue for an object of class "\code{lm}", except with
aggregated data.
}
\description{
\code{agg_lm} is used to fit a linear regression model
when the data are in aggregated form. That is, each unique
combination of the covariates is assigned an integer count
that is passed to the \code{weights} argument.
It is useful for data that has been aggregated with a
\code{GROUP BY <list of variables>} clause in an \code{SQL} query.
It is designed to imitate the \code{lm} function, with appropriate
adjustments for the sample size of the original un-aggregated data.
Aside from the \code{weights} argument being mandatory, the arguments
are identical to those in \code{lm}.
}
\seealso{
\code{lm} function in the \code{stats} library.
\code{summary_agg_lm} for summaries of objects in class "\code{agg_lm}".
}
