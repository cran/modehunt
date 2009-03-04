\name{cvModeAll}

\alias{cvModeAll}

\docType{data}

\title{Critical values for test statistic based on all intervals}

\description{This dataset contains critical values for some \eqn{n} and \eqn{\alpha} for the test statistic based on 
all intervals, with or without additive correction term \eqn{\Gamma}.}

\usage{data(cvModeAll)}

\format{
  A data frame providing 15 different combinations of \eqn{n} and \eqn{\alpha} and the following columns:
  
  \tabular{ll}{
    \code{alpha}   \tab The levels at which critical values were simulated. \cr
    \code{n}       \tab The number of observations for which critical values were simulated. \cr
    \code{withadd} \tab Critical values based on \eqn{T_n^+({\bf{U}})} and the set of all intervals \eqn{\mathcal{I}_{all}}. \cr
    \code{noadd}   \tab Critical values based on \eqn{T_n({\bf{U}})} and the set of all intervals \eqn{\mathcal{I}_{all}}.
  }
}

\details{For details on the above test statistics see \code{\link{modeHunting}}. Critical values are based on 
\eqn{M=100'000} simulations of i.i.d. random vectors

\deqn{{\bf{U}} = (U_1,\dots,U_n)}

where \eqn{U_i} is a uniformly on \eqn{[0,1]} distributed random variable, \eqn{i=1,\dots,M}.
}

\section{Remember}{\eqn{n} is the number of \emph{interior observations}, i.e. if you are analyzing a sample of size
\eqn{m}, then you need critical values corresponding to

  \tabular{ll}{
    \code{n = m-2} \tab If no additional information on \eqn{a} and \eqn{b} is available. \cr
    \code{n = m-1} \tab If either \eqn{a} or \eqn{b} is known to be a certain finite number. \cr
    \code{n = m  } \tab If both \eqn{a} and \eqn{b} are known to be certain finite numbers,
  }
  
where \eqn{[a,b] = \{x \ : \ f(x) > 0\}} is the support of \eqn{f}.
}

\source{These critical values were generated using the function \code{\link{criticalValuesAll}}. Critical values
        for other combinations for \eqn{\alpha} and \eqn{n} can be computed using this latter function.}

\references{
Rufibach, K. and Walther, G. (2007).
A general criterion for multiscale inference. 
Preprint, Department of Statistics, Stanford University.
}

\examples{
## extract critical values for alpha = 0.05, n = 200
data(cvModeAll)
cv <- cvModeAll[cvModeAll$alpha == 0.05 & cvModeAll$n == 200, 3:4]
cv
}

\keyword{datasets}
