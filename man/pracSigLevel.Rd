\name{pracSigLevel}
\alias{pracSigLevel}
\title{Practical Significance Level}
\description{Function for using effect sizes to determine the level of 'practical significance'.}
\usage{
pracSigLevel(effect_sizes, type = "d", rules = "cohen1988")
}
\arguments{
  \item{effect_sizes}{Numeric vector of effect sizes.}
  \item{type}{Character, type of effect size. Can be \code{'d'}, \code{'g'}, \code{'r'}, \code{'eta'}, \code{'epsilon'}, and \code{'omega'}.}
  \item{rules}{Character, rules for determining the level of 'practical significance'. Depends on the \code{type} specified.}
}
\details{
\code{pracSigLevel} determines the 'practical significance' level of the provided effect sizes based on rules-of-thumb, with the original rules developed by Jacob Cohen (1988) being the default. Criticisms have been made regarding Cohen's fragile empirical foundation for his cutoff points, leading to various researchers reworking these by basing them on typical effect sizes of their specific fields (e.g., Lovakov & Agadullina 2021).

  The function currently only supports standardized differences and correlation coefficients, including Cohen's \emph{d} (1988), Hedges' \emph{g} (1981), Pearson's \emph{r} (1895), Eta, Epsilon, and Omega (Tabachnick & Fidell 2007). While Pearson's \emph{r} is computable with the built-in \code{cor} function from the \code{stats} package (R Core Team 2023), the remaining effect sizes can be computed with functions from the \code{effectsize} package (Ben-Shachar \emph{et al.} 2020).
}
\section{Standardized Differences}{
\itemize{
  \item{Lovakov & Agadullina (2021)}{
    (\code{'lovakov2021'})
    \itemize{
      \item{0 < effect size < 0.15}{ - I (Very Small)}
      \item{0.15 <= effect size < 0.36}{ - II (Small)}
      \item{0.36 <= effect size < 0.65}{ - III (Medium)}
      \item{0.65 <= effect size}{ - IV (Large)}
    }
  }
}
\itemize{
  \item{Gignac (2016)}{
    (\code{'gignac2016'})
    \itemize{
      \item{0 < effect size < 0.2}{ - I (Very Small)}
      \item{0.2 <= effect size < 0.41}{ - II (Small)}
      \item{0.41 <= effect size < 0.63}{ - III (Medium)}
      \item{0.63 <= effect size}{ - IV (Large)}
    }
  }
}
\itemize{
  \item{Sawilowsky (2009)}{
    (\code{'sawilowsky2009'})
    \itemize{
      \item{0 < effect size < 0.1}{ - I (Tiny)}
      \item{0.1 <= effect size < 0.2}{ - II (Very Small)}
      \item{0.2 <= effect size < 0.5}{ - III (Small)}
      \item{0.5 <= effect size < 0.8}{ - IV (Medium)}
      \item{0.8 <= effect size < 1.2}{ - V (Large)}
      \item{1.2 <= effect size < 2}{ - VI (Very Large)}
      \item{2 <= effect size}{ - VII (Huge)}
    }
  }
}
\itemize{
  \item{Cohen (1988)}{
    (\code{'cohen1988'}), default
    \itemize{
      \item{0 < effect size < 0.2}{ - I (Very Small)}
      \item{0.2 <= effect size < 0.5}{ - II (Small)}
      \item{0.5 <= effect size < 0.8}{ - III (Medium)}
      \item{0.8 <= effect size}{ - IV (Large)}
    }
  }
}
}
\section{Correlation Coefficients}{
\itemize{
  \item{Lovakov & Agadullina (2021)}{
    (\code{'lovakov2021'})
    \itemize{
      \item{0 < effect size < 0.12}{ - I (Very Small)}
      \item{0.12 <= effect size < 0.24}{ - II (Small)}
      \item{0.24 <= effect size < 0.41}{ - III (Moderate)}
      \item{0.41 <= effect size}{ - IV (Large)}
    }
  }
}
\itemize{
  \item{Funder & Ozer (2019)}{
    (\code{'funder2019'})
    \itemize{
      \item{0 < effect size < 0.05}{ - I (Tiny)}
      \item{0.05 <= effect size < 0.1}{ - II (Very Small)}
      \item{0.1 <= effect size < 0.2}{ - III (Small)}
      \item{0.2 <= effect size < 0.3}{ - IV (Medium)}
      \item{0.3 <= effect size < 0.4}{ - V (Large)}
      \item{0.4 <= effect size}{ - VI (Very Large)}
    }
  }
}
\itemize{
  \item{Gignac (2016)}{
    (\code{'gignac2016'})
    \itemize{
      \item{0 < effect size < 0.1}{ - I (Very Small)}
      \item{0.1 <= effect size < 0.2}{ - II (Small)}
      \item{0.2 <= effect size < 0.3}{ - III (Medium)}
      \item{0.3 <= effect size}{ - IV (Large)}
    }
  }
}
\itemize{
  \item{Evans (1996)}{
    (\code{'evans1996'})
    \itemize{
      \item{0 < effect size < 0.2}{ - I (Very Weak)}
      \item{0.2 <= effect size < 0.4}{ - II (Weak)}
      \item{0.4 <= effect size < 0.6}{ - III (Moderate)}
      \item{0.6 <= effect size < 0.8}{ - IV (Strong)}
      \item{0.8 <= effect size}{ - V (Very Strong)}
    }
  }
}
\itemize{
  \item{Cohen (1988)}{
    (\code{'cohen1988'}), default
    \itemize{
      \item{0 < effect size < 0.1}{ - I (Very Small)}
      \item{0.1 <= effect size < 0.3}{ - II (Small)}
      \item{0.3 <= effect size < 0.5}{ - III (Moderate)}
      \item{0.5 <= effect size}{ - IV (Large)}
    }
  }
}
}
\value{Character vector of the level of 'practical significance' given the chosen type of effect size and rules.}
\references{
Ben-Shachar, M., D. Ludecke & D. Makowski (2020): 'effectsize: Estimation of Effect Size Indices and Standardized Parameters', \emph{Journal of Open Source
  Software}, 5(56): 2815.

  Cohen, J. (1988): \emph{Statistical Power Analysis for the Behavioral Sciences}, 2nd edition. Routledge.

  Evans, J. D. (1996): \emph{Straightforward statistics for the behavioral sciences}, Thomson Brooks/Cole Publishing Co.

  Funder, D. C. & D. J. Ozer (2019): 'Evaluating Effect Size in Psychological Research: Sense & Nonsense', \emph{Advances in Methods & Practices in Psychological Science}, 2(2): 155-168.

  Gignac, G. E. & E. T. Szodorai (2016): 'Effect size guidelines for individual differences researchers', \emph{Personality & Individual Differences}, 102: 74-78.

  Hedges, L. (1981): 'Distribution Theory for Glass' Estimator of Effect Size & Related Estimators' \emph{Journal of Educational Statistics}, 6(2): 107-128.

  Lovakov, A. & E. R. Agadullina (2021): 'Empirically Derived Guidelines for Effect Size Interpretation in Social Psychology', \emph{European Journal of Social Psychology}, 51(3): 485-504.

  Pearson, K. (1895) 'Notes on Regression & Inheritance in the Case of Two Parents', \emph{Proceedings in the Royal Society of London}, 58: 240-242.

  R Core Team (2023): \emph{R: A Language and Environment for Statistical Computing}. R Foundation for Statistical Computing.

  Sawilowsky, S. S. (2009): 'New Effect Size Rules of Thumb', \emph{Journal of Modern Applied Statistical Methods}, 8(2): 26.

  Tabachnick, B. G. & L. S. Fidell (2007): 'Cleaning Up Your Act. Screening Data Prior to Analysis', in B. G. Tabachnick & L. S. Fidell (eds.): \emph{Using Multivariate Statistics}, 5th edition. Pearson Education, Inc. / Allyn & Bacon.
}
\author{
Concept: Jacob Cohen

  Code: Emil Niclas Meyer-Hansen
}
\note{
Created: 3/10/2023

  Updated: 27/11/2023
}

\seealso{
  \code{\link[effectsize:interpret_r]{interpret_r}} for interpreting correlation coefficients, \code{\link[effectsize:interpret_cohens_d]{interpret_cohens_d}} for interpreting standardized differences, or \code{\link{statSigLevel}} and \code{\link{typeMlevel}} for assessing level of 'statistical significance' and 'Type M error level', respectively.
}
\examples{
# Typical cutoffs
effect_sizes <- c(0.1, 0.2, 0.5, 0.8, 1.2, 2, 2.1)

# Practical Significance Level
pracSigLevel(effect_sizes)
}
\keyword{htest}
\concept{pracSigLevel}
\concept{significance}
\concept{effectsize}
