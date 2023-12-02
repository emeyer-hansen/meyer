\name{ci}
\alias{ci}
\title{Confidence Interval}
\description{Function for computing \emph{confidence intervalconfidence intervals}.
}
\usage{ci(x = NULL, se = NULL, alpha = 0.05, df = Inf)}
\arguments{
  \item{x}{Numeric vector of quantity of interest.}
  \item{se}{Positive numeric vector of standard errors.}
  \item{alpha}{Non-negative numeric false positive (Type I) error rate.}
  \item{df}{Positive numeric, degrees of freedom.}
}
\details{
Developed by Jerzy Neyman (1937), a \emph{confidence interval} (CI) is used to express certainty regarding the estimation of a given quantity of interest. Following a \emph{frequentist} understanding of probability, a CI with a typical certainty of 95\% can be interpreted as containing the 'true' value 95\% of the time in the long-run frequency. Note that this interpretation isn't equal to the CI containing the 'true' estimate with 95\% probability, as each specific CI either contains or doesn't contain the 'true' estimate.

The \code{ci} function computes the CI with a specified level of confidence (\code{alpha}) given an estimated quantity of interest (\code{x}), its standard errors (\code{se}) and a degrees of freedom (\code{df}).
}
\value{Numeric matrix of confidence intervals at the specified confidence level.}
\references{
Neyman, J. (1937): 'Outline of a Theory of Statistical Estimation Based on the Classical Theory of Probability', \emph{Philosophical Transactions of the Royal Society A}, 236(767): 333-380.
}
\author{
Concept: Jerzy Neyman

  Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 28/11/2023

  Updated: 28/11/2023
}
\examples{
# Random data
set.seed(1791)
n <- 5

# Quantity of interest
x <- rnorm(n)
round(x, 3)

# Standard error
se <- rexp(n)
round(se, 3)

# 95\% CI
ci(x, se)
}
\keyword{htest}
\concept{ci}
\concept{confidence_interval}
