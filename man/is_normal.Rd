% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normality.R
\name{is_normal}
\alias{is_normal}
\title{Check for normality
is_normal() gives a TRUE/FALSE value based on normality test of the variable.}
\usage{
is_normal(variable, show.warnings = FALSE, decimals = 2, DEBUG = FALSE)
}
\arguments{
\item{variable}{vector of values (ideally numbers)}

\item{show.warnings}{Default FALSE. Whether or not you want to get possible warnings}
}
\value{
logical value (TRUE/FALSE)
}
\description{
Check for normality
is_normal() gives a TRUE/FALSE value based on normality test of the variable.
}
\examples{

is_normal(rnorm(100))

}
