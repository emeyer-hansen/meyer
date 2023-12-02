\name{criticalValue}
\alias{criticalValue}
\title{Critical Value}
\description{
Function for computing the the \emph{critical value} of a test-statistic necessary for achieving 'statistical significance'.
}
\usage{criticalValue(alpha, df = Inf, alternative = 'two.tailed')}
\arguments{
  \item{alpha}{Non-negative numeric vector, false positive error rate.}
  \item{df}{Positive integer, degrees of freedom.}
  \item{alternative}{Character, 'two-tailed'.}
}
\details{
The critical value indicates the cutoff point for achieving statistical significance. Using the conventional false positive (Type I) error rate of 5\% and a normal distribution, the critical value is 1.96. It is typically used in relation to a \emph{t-test} (Gosset 1908).

Sometimes it is necessary to compute the critical value, especially when using a different Type I error rate, or if working strictly with test statistics instead of \emph{p}-values.

\code{criticalValue} provides the means to compute the critical value given a specified Type I error rate and a df, allowing the researcher to better determine the exact cutoff point for achieving 'statistical significance'.
}
\value{
Non-negative numeric, critical value necessary for achieving 'statistical signicance'.
}
\references{
Gosset, W. S. (1908): 'The Probable Error of a Mean', \emph{Biometrika}, 6(1): 1-25.
}
\author{
Concept: William S. Gosset

  Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 12/10/2023

  Updated: 27/11/2023
}
\examples{
# Typical alphas, df
alpha <- c(0.05, 0.01)
df <- Inf

# Critical value
criticalValue(alpha, df)
}
\keyword{htest}
\concept{significance}
\concept{criticalValue}
