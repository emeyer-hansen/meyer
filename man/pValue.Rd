\name{pValue}
\alias{pValue}
\title{p-Value}
\description{Function for computing the \emph{p}-value.}
\usage{pValue(t_stat = NULL, df = Inf, alternative = 'two.tailed', correction = FALSE)}
\arguments{
  \item{t_stat}{Numeric vector of \emph{test statistics}, e.g., the \emph{t}-statistic.}
  \item{df}{Positive integer, degrees of freedom.}
  \item{alternative}{Character, 'two-tailed'.}
  \item{correction}{Logical, whether to account for underflow by upward adjusting the \emph{p}-value.}
}
\details{
 The \emph{p}-value, first formally introduced by Karl Pearson (1900), is the probability of data as or more extreme than the observed given the (null) hypothesis is true. \emph{p}-values are used as evidence against a (null) hypothesis (Fisher 1925), and values lower than 0.05 are typically used to demarcate 'statistical significance'.

  \code{pValue} computes a \emph{p}-value using the given test-statistics (e.g., \emph{t}-statistic, \emph{z}-statistic) and degrees of freedom (\code{df}).

  The function can account for underflow, i.e., when values relatively close to zero are rounded to zero (Demmel 1984). Underflow results in erroneous values that inappropriately assigns zero probability to an outcome. By assigning a value of \code{0} to a \emph{p}-value it makes adjustment methods for multiple comparisons defunct (e.g., Bonferroni; Miller 1966), since these methods typically apply a factor to a \emph{p}-value to increase its value in proportion to the number of \emph{hypothesis tests} being conducted. Applying the correction method solves this issue.
}
\value{Non-negative numeric vector of \emph{p}-values.}
\references{
Demmel, J. (1984): 'Underflow & the Reliability of Numerical Software', \emph{SIAM Journal on Scientific & Statistical Computing}, 5(4): 887-919.

  Fisher, R. A. (1925): \emph{Statistical Methods for Research Workers}. Oliver & Boyd.

  Miller, R. G. (1966): \emph{Simultaneous Statistical Inference}. Springer.

  Pearson, K. (1900): 'On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling', \emph{Philosophical Magazine}, 50(302): 157-175.
}
\author{
Concept: Karl Pearson

  Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 3/10/2023

  Updated: 27/11/2023
}

\seealso{
\code{\link{criticalValue}} for computing the critical value that determines 'statistical significance' and \code{\link[stats:p.adjust]{p.adjust}} for how to adjust \emph{p}-values for multiple comparisons.
}
\examples{
# t-statistics
t_stat <- c(1.95, 1.96, 2)

# p-values
round(pValue(t_stat, correction = TRUE), 3)
}
\keyword{htest}
\concept{pvalue}
\concept{p_value}
