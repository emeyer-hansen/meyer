\name{meyersDF}
\alias{meyersDF}
\title{Meyer's Degrees of Freedom}
\description{
Function for determining the \emph{degrees of freedom} (df) for a prior \emph{Student t}-distribution.
}
\usage{
meyersDF(n = NULL, s = NULL, bias = 40)
}
\arguments{
  \item{n}{Positive numeric value, average sample size.}
  \item{s}{Positive numeric value, number of studies.}
  \item{bias}{Numeric value, bias against fields with an average sample size of this size.}
}
\details{
This function serves to enable a reproducible procedure for qualitatively determining the \code{df} for a prior \emph{Student t}-distribution (see Gosset 1908) that reflects the strength of the existing evidence in a field. The resulting \code{df} is based on the number of previously conducted studies \code{s} in the given field and their average sample size \code{n}.

  As a qualitative assessment of the existing strength of evidence, the equation used to determine \code{df} nicely reflects decreasing skepticism as a function of the number of \code{s} and their \code{n}.

  When \code{s = 1}, the functions scales \code{df} linearly with \code{n}. As \code{s} increases, the relationship between \code{n} and \code{df} becomes exponential to reflect the growing evidence from multiple studies, assuming studies are independent of each other. To prevent \code{df} increasing exponentially with the addition of a study with a relatively smaller sample size, the average sample size \code{n} is used instead of the total sample size across studies to penalize additional studies with a relatively smaller sample.

  The \code{bias} is a penalty to \code{df} that serves to reflect the researcher's skepticism towards evidence from fields composed of studies with small sample sizes. It is equivalent to the intercept, and higher values of \code{bias} results in a lower intercept. Due to the ongoing issues in reproducing results from fields with an \code{n} of approximately \code{40} (Anderson et al. 2017), this is the default value of \code{bias}.

  With a small (\code{n}) and few (\code{s}), the value of \code{df} will likely be \code{1}, which when used in a \emph{Student t}-distribution makes it equivalent to the relatively skeptical \emph{Cauchy}-distribution, thus appropriately reflecting skepticism towards the evidence in the given field.

  With a large \code{n} and a multitude of studies (\code{s}), the value of \code{df} will approach infinity, which in a \emph{Student t}-distribution makes it equivalent to a (Gaussian) \emph{normal}-distribution, reflecting the relative strength of the evidence in the given field.

  Using this function to determine \code{df} thus allows a researcher draw on qualitative insights and take into account the average sample size \code{n}, number of studies \code{s}, and a healthy skepticism against studies with a small sample size \code{bias}, all while ensuring the resulting \code{df} is reproducible.

  The function is easily applied together with the \code{Student_t} function from the \code{rstanarm} package (Goodrich \emph{et al.} 2023; Brilleman \emph{et al.} 2018).
}
\value{
Positive numeric \code{df} for a \emph{Student t}-distribution, reflecting the strength of the existing evidence in the field.
}
\references{
Anderson, S. F., K. Kelley & S. E. Maxwell (2017): 'Sample Size Planning for More Accurate Statistical Power: A Method Adjusting Sample Effect Sizes for Publication Bias & Uncertainty', \emph{Psychological Science}, 28(11): 1547-1562.

  Brilleman, S. L, M. J. Crowther, M. Moreno-Betancur, J. B. Novik & R. Wolfe (2018): \emph{Joint longitudinal and time-to-event models via Stan}. \href{https://github.com/stan-dev/stancon_talks/}{StanCon}.

  Goodrich, B., J. Gabry, I. Ali & S. Brilleman (2023): \emph{rstanarm: Bayesian applied regression modeling via Stan}. \href{https://mc-stan.org/rstanarm}{Stan}.

  Gosset, W. S. (1908): 'The Probable Error of a Mean', \emph{Biometrika}, 6(1): 1-25.
}
\author{
Concept & Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 11/11/2023

  Updated: 27/11/2023
}
\seealso{
\code{\link[rstanarm:priors]{priors}} for specifying a prior \emph{Student t}-distribution.
}
\examples{
# Random average sample size
set.seed(1791)
n <- rnorm(1, 1000, 200)
round(n)

# Random number of studies
s <- round(runif(1, 1, 3))
s

# Resulting df
df <- meyersDF(n, s, bias = 40)
df
}
\keyword{algebra}
\concept{meyersDF}
\concept{df}
\concept{student_t}
