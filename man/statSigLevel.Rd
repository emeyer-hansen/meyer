\name{statSigLevel}
\alias{statSigLevel}
\alias{stargaze}
\title{Statistical Significance Level}
\description{Function for using \emph{p}-values to determine the level of 'statistical significance'.}
\usage{
statSigLevel(p_values = NULL, rules = 'conventional')
stargaze(p_values = NULL, rules = 'conventional')
}
\arguments{
  \item{p_values}{Non-negative numeric vector of \emph{p}-values.}
  \item{rules}{Character, rules for determining the level of 'statistical significance'. Can be 'conventional' (default) or 'pseudo'.}
}
\details{
\code{statSigLevel} serves to determine the levels of 'statistical significance' of the provided \emph{p}-values using conventional rules developed by Fisher (1925), Jerzy Neyman and Egon Pearson (1933), and  various researchers from fields of research doing \emph{null hypothesis significance testing} (NHST).

\code{stargaze} is by all means and purposes functionally equivalent to \code{statSigLevel}.

\itemize{
  \item{Fisher (1925)}{
    (\code{'conventional'}), default
    \itemize{
      \item{0.05 >= \emph{p}-value > 0.01}{ - *}
      \item{0.01 >= \emph{p}-value > 0.001}{ - **}
      \item{0.001 >= \emph{p}-value}{ - ***}
    }
  }
}
\itemize{
  \item{Unknown / Various}{
    (\code{'pseudo'})
    \itemize{
      \item{0.1 >= \emph{p}-value}{ - #}
      \item{0.05 >= \emph{p}-value > 0.01}{ - *}
      \item{0.01 >= \emph{p}-value > 0.001}{ - **}
      \item{0.001 >= \emph{p}-value}{ - ***}
    }
  }
}
}
\value{
Character vector of levels of 'statistical significance'.
}
\references{
Fisher, R. A. (1925): \emph{Statistical Methods for Research Workers}. Oliver & Boyd.

  Neyman, J. & E. S. Pearson (1933): 'The Testing of Statistical Hypotheses in Relation to Probabilities a priori', \emph{Mathematical Proceedings of the Cambridge Philosophical Society}, 29(4): 492-510.
}
\author{
Concept: Ronald A. Fisher, Jerzy Neyman, and Egon Pearson

  Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 14/5/2023

  Updated: 27/11/2023
}
\seealso{
\code{\link[effectsize:interpret_p]{interpret_p}} for interpreting a \emph{p}-value, \code{\link[stats:p.adjust]{p.adjust}} for adjusting it, or \code{\link{pracSigLevel}} and \code{\link{typeMlevel}} for assessing level of 'practical significance' and 'Type M error level', respectively.
}
\examples{
# p-Values at typical cutoffs
p_values <- c(0.11, 0.1, 0.05, 0.01, 0.001)

# Using 'conventional' rules
statSigLevel(p_values)

# Using 'pseudo' rules
statSigLevel(p_values, 'pseudo')
}
\keyword{htest}
\concept{statSigLevel}
\concept{pvalue}
\concept{p_value}
\concept{significance}
