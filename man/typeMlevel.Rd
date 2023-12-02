\name{typeMlevel}
\alias{typeMlevel}
\title{Type M Error Level}
\description{Function for determining the 'level' of a \emph{Type M error}.}
\usage{typeMlevel(typeM = NULL, rules = 'meyer2023')}
\arguments{
  \item{typeM}{Numeric vector of \emph{Type M} errors.}
  \item{rules}{Character, rules for determining the 'level' of a \emph{Type M} error. Can be 'meyer2023' (default).}
}
\details{
A \emph{Type M} error refers to the \emph{magnitude} by which a quantity of interest has been under-/overestimated (Gelman & Carlin 2014). Values closer to 1  indicate a lower exaggeration magnitude. The \emph{type M} error can be computed with the \code{retrodesign} package (Timm 2023).

  The \code{typeMlevel} function serves as an heuristic to evaluate the 'level' of error by mimicking conventional rules-of-thumb used for related evaluation metrics (e.g., \emph{p}-value.). Thus note that these aren't hard cutoff points and that they are no replacement for drawing on qualitative insight to provide a better assessment of the 'level' of a \emph{Type M} error.

\itemize{
  \item{}{
    \code{'meyer2023'}, default
    \itemize{
      \item{0.95 <= typeM >= 1.05}{ - * (Error Magnitude less than 5\%)}
      \item{0.99 <= typeM >= 1.01}{ - ** (Error Magnitude less than 1\%)}
      \item{0.999 <= typeM >= 1.001}{ - *** (Error Magnitude less than 0.1\%)}
    }
  }
}
}
\value{Character vector of \emph{Type M} error 'levels'.}
\references{
Gelman, A. & J. Carlin (2014): 'Beyond Power Calculations: Assessing Type S (Sign) & Type M (Magnitude) Errors', \emph{Perspectives on Psychological Science}, 9(6): 641-651.

  Timm, A. (2023): \emph{retrodesign: Tools for Type S (Sign) and Type M (Magnitude) Errors}. \href{https://CRAN.R-project.org/package=retrodesign}{CRAN}.
}
\author{
Concept & Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 3/10/2023

  Updated: 27/11/2023
}

\seealso{
\code{\link[retrodesign:retrodesign]{retrodesign}} for computing \emph{Type M} and \emph{Type S} errors, \code{\link{pracSigLevel}} and \code{\link{statSigLevel}} for computing 'Practical' and 'Statistical Significance' levels, respectively.
}
\examples{
# Random Type M errors
typeM <- c(1, 1.001, 0.999, 1.01, 0.99, 1.05, 0.95, 0.949, 1.051)

# Type M levels
typeMlevel(typeM)
}
\keyword{htest}
\concept{typeM}
