\name{is.missing}
\alias{is.missing}
\alias{is.missing.or.infinite}
\title{Missing Values}
\description{Functions for detecting every variety of \emph{missing} and \emph{infinite} values (e.g., \code{NA}, \code{Inf}).}
\usage{
is.missing(x = NULL)
is.missing.or.infinite(x = NULL)
}
\arguments{
  \item{x}{Vector containing \emph{missing/infinite} values.}
}
\details{
\code{is.missing} serves as a catch-all solution for detecting missing values and solves the issue of having to check for each type of 'missingness' individually. The detectable missing values are \code{NA}, \code{NaN}, and \code{NULL}.

  \code{is.missing.or.infinite} expands on the previous function by also detecting infinite values, i.e., \code{Inf} and \code{-Inf}.

  The function is heavily reliant on functions from the built-in \code{base} package (R Core Team 2023).
}
\value{
Logical vector x, with \code{TRUE} indicating a missing value and \code{FALSE} indicating a non-missing value.
}
\references{R Core Team (2023): \emph{R: A Language & Environment for Statistical Computing}. R Foundation for Statistical Computing.}
\author{
Concept & Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 28/11/2023

  Updated: 28/11/2023
}
\seealso{
\code{\link[base:is.na]{is.na}}, \code{\link[base:is.null]{is.null}}, \code{\link[base:is.nan]{is.nan}}, or \code{\link[base:is.infinite]{is.infinite}} for the individual functions used in \code{is.missing}.
}
\examples{
  # Random data
  set.seed(1791)
  x <- c(rnorm(3), NA, NaN, NULL, Inf, -Inf)

  # is.missing
  is.missing(x)

  # is.missing.or.infinite
  is.missing.or.infinite(x)
}
\keyword{NA}
\keyword{logic}
\keyword{complex}
\concept{is.missing}
\concept{is.missing.or.infinite}
\concept{missingness}
