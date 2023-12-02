\name{pooledSE}
\alias{pooledSE}
\title{Pooled Standard Error}
\description{Function for computing the \emph{pooled standard error} (SE).}
\usage{pooledSE(se = NULL, q = NULL, m = NULL, na.rm = FALSE, inf.rm = FALSE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{se}{Non-negative numeric vector of standard errors.}
  \item{q}{Numeric vector of the quantity of interest.}
  \item{m}{Positive integer, number of imputed datasets.}
  \item{na.rm}{Logical, \code{TRUE} or \code{FALSE}, indicating whether \code{NA} values should be stripped before the computation proceeds.}
  \item{inf.rm}{Logical, \code{TRUE} or \code{FALSE}, indicating whether \code{Inf} values should be stripped before the computation proceeds.}
}
\details{
Created by Donald P. Rubin (1987) for use in relation to imputing missing values, the \emph{pooled standard error} estimates uncertainty across multiple imputed datasets (\code{m}) by accounting for uncertainty in each imputed dataset (within variance), as well as the uncertainty across the imputed datasets (between variance). It applies a penalty term scaled by \code{m} that converges to \code{0} as \code{m} increases to infinity (King 2020).

  The function as implemented here adapts the pooled SE equation from Heymans & Eekhout (2019). Includes improved exception handling.
}
\value{
Numeric, \emph{pooled standard error} accounting for within and between variance.
}
\references{
Heymans, M. W. & I. Eekhout (2019): "Rubin's Rules", in \emph{Applied Missing Data Analysis With SPSS & (R)Studio}, chapter 9. \href{https://bookdown.org/mwheymans/bookmi/rubins-rules.html}{Bookdown}.

  King, G. V. (2020): "18. Missing Data", \emph{Quantitative Social Science Methods, I (Gov2001 at Harvard University)}. \href{https://www.youtube.com/watch?v=qlPs8Ioa56Y&list=PL0n492lUg2sgSevEQ3bLilGbFph4l92gH&index=18}{Youtube}

  Rubin, D. B. (1987): \emph{Multiple Imputation for Nonresponse in Surveys}. Wiley.
}
\author{
Concept: Donald P. Rubin

  Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 3/10/2023

  Updated: 27/11/2023
}
\seealso{
\code{\link[stats:sd]{sd}} for the standard deviation of a vector, \code{\link[stats:var]{var}} for its variance, \code{\link[stats:mad]{mad}} for the median absolute deviation (MAD).
}
\examples{
# Random data
set.seed(1791)
m <- round(rnorm(1, 100, 25))
se <- rexp(m, 1)
q <- rnorm(m)

# Pooled SE
round(pooledSE(se, q, m), 3)
}
\keyword{algebra}
\keyword{NA}
\keyword{univar}
\keyword{htest}
\concept{pooledSE}
\concept{SE}
\concept{imputation}
