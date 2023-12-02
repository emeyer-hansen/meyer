\name{assessPower}
\alias{assessPower}
\title{Power Assessment}
\description{
Function for checking if an expected 'statistically (in)significant' coefficient actually tested 'statistically significant'.
}
\usage{assessPower(r = NULL, min_detectable_r = NULL, p_value = NULL, alpha = 0.05)}
\arguments{
  \item{r}{Numeric vector of coefficients.}
  \item{min_detectable_r}{Numeric vector of the smallest expected detectable coefficient given the study's \emph{power}.}
  \item{p_value}{Numeric vector of \emph{p}-values.}
  \item{alpha}{Numeric vector of \emph{type I error} rates.}
}
\details{Statistical power refers to the ability to correctly detect an effect and is the opposite of the \emph{false negative} (Type II) error rate (Cohen 1988). Conventionally, studies should aim for \emph{false positive} (Type I) error rate of 5\% and a power of 80\%. If the sample size is known, it is possible to calculate the smallest expectedly detectable coefficient. Sometimes, however, coefficients above this threshold won't achieve statistical significance, while values below it will test statisticially significant. Rather than take (in)significant results for granted, it can be in the interest of the researcher to assess whether such unexpected cases exist among the results and use these insights, e.g., in theory building and discussion.

  The input values are computable with the \code{pwr.r.test} function from the \code{pwr} package (Champely 2020), or alternative the \code{retrodesign} package (Timm 2023).
}
\value{Character vector of unexpectedly (in)significant results.}
\references{
Champely, S. (2020): \emph{pwr: Basic Functions for Power Analysis}. \href{https://CRAN.R-project.org/package=pwr}{CRAN}.

  Cohen, J. (1988): \emph{Statistical Power Analysis for the Behavioral Sciences}, 2nd edition. Routledge.

  Timm, A. (2023): \emph{retrodesign: Tools for Type S (Sign) and Type M (Magnitude) Errors}. \href{https://CRAN.R-project.org/package=retrodesign}{CRAN}.
}
\author{
Concept & Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 12/10/2023

  Updated: 27/11/2023
}
\seealso{
\code{\link[pwr:pwr.r.test]{pwr.r.test}} or \code{\link[retrodesign:retrodesign]{retrodesign}} for computing the statistical power of a study.
}
\examples{
# Random data
r <- c(0.04, 0.04, 0.051, 0.051)
min_detectable_r <- 0.05
p_value <- c(0.05, 0.06, 0.05, 0.06)
alpha <- 0.05

# Power Assessment
assessPower(r, min_detectable_r, p_value, alpha)
}
\keyword{htest}
\concept{assessPower}
\concept{power}
