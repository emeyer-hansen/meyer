\name{meyerize}
\alias{meyerize}
\title{'Meyerization' / R Score Transformation}
\description{
Function for rescaling a numeric vector to have an approximate mean of \code{0} and asymptotic range \code{[-1; 1]}.
}
\usage{
meyerize(x, na.rm = FALSE, inf.rm = FALSE)
}
\arguments{
  \item{x}{Numeric vector to \emph{meyerize}.}
  \item{na.rm}{Logical, \code{TRUE} or \code{FALSE}, indicating whether \code{NA} values should be stripped before the computation proceeds.}
  \item{inf.rm}{Logical, \code{TRUE} or \code{FALSE}, indicating whether \code{Inf} values should be stripped before the computation proceeds.}
}
\details{
\code{meyerize} serves to make a coefficient analogous to a correlation coefficient (e.g., Pearson's \emph{r}; 1895) though rescaling. For any numeric vector \code{x}, \emph{meyerization} ensures that \code{x} has an approximate mean of \code{0}. Under the assumption of \code{x} being normally distributed, the range of \code{x} will be asymptotically \code{[-1; 1]}.

   For the purposes of linear regression, \emph{meyerizing} both a continuous predictor \code{x} and continuous outcome \code{y}, and assuming they're jointly normally distributed, enables the coefficient of \code{x} on \code{y} to be interpreted as the following: An increase (decrease) in \code{x} from its mean to its highest (lowest) value is associated with an increase (decrease) in \code{y} from its mean to its highest (lowest) value by the size of the coefficient.

   This rescaling procedure allows an easier substantial interpretation of the coefficient since its comparable to an effect size. Similar to rescaling functions from other packages, e.g., \code{rescale} from the \code{scales} package (Wickham & Seidel 2022) and \code{standardize} from the \code{datawizard} package (Patil \emph{et al.} 2022), the coefficient can be directly compared to similarly rescaled coefficients from related studies.
}
\value{
\emph{meyerized} numeric vector \code{x}, rescaled with approximate mean of \code{0} and asymptotic range \code{[-1; 1]}, assuming normality.
}
\references{
Patil, I., D. Makowski, M. S. Ben-Shachar, B. M. Wiernik, E. Bacher & D. Ludecke (2022): 'datawizard: An R Package for Easy Data Preparation and Statistical Transformations', \emph{Journal of Open Source Software}, (78): 4684.

  Pearson, K. (1895) 'Notes on Regression & Inheritance in the Case of Two Parents', \emph{Proceedings in the Royal Society of London}, 58: 240-242.

  Wickham, H. & D. Seidel (2022): \emph{scales: Scale Functions for Visualization}. \href{https://CRAN.R-project.org/package=scales}{CRAN}.
}
\author{
Concept & Code: Emil Niclas Meyer-Hansen
}
\note{
Created: Oktober 12th 2023

  Updated: November 28th 2023
}
\seealso{
\code{\link[stats:cor]{cor}} for computing Pearson's \emph{r}, \code{\link[scales:rescale]{rescale}} for min-maxing / normalization, and \code{\link[datawizard:standardize]{standardize}} for Fisher's \emph{z score transformation}.
}
\examples{
# Random data
set.seed(1791)
n <- 1000
x = rnorm(n)
data <- data.frame(
  x,
  y = runif(1, 0.1, 10)*x+rnorm(n)
)

# Meyerizing
data$x <- meyerize(data$x)
data$y <- meyerize(data$y)

# Mean approx 0
format(mean(data$x), scientific = FALSE)
format(mean(data$y), scientific = FALSE)

# Range approx [-1; 1]
range(data$x)
range(data$y)

# Pearson's r
round(cor(data$y, data$x), digits = 3)

# Meyerized coefficient
round(summary(lm(y~x, data))$coefficients[2,1], digits = 3)
}
\keyword{manip}
\concept{meyerization}
\concept{r}
\concept{rescaling}
\concept{standardization}
\concept{standardisation}
\concept{demeaning}
