\name{minimalIntervals}

\alias{minimalIntervals}

\title{Compute set of minimal intervals}

\encoding{latin1}

\description{In general, all intervals that have a test statistic bigger than the respective critical value
are output. For a given set of intervals \eqn{\mathcal{K}}, all intervals \eqn{J} such that \eqn{\mathcal{K}} 
does not contain a proper subset of \eqn{J} are called \emph{minimal}. Given \eqn{\mathcal{K}}, this function 
computes the set of minimal intervals.}

\usage{minimalIntervals(ints)}

\arguments{
  \item{ints}{Either one of the sets \eqn{\mathcal{D}^+} or \eqn{\mathcal{D}^-} as output by one of the functions
                \code{\link{modeHunting}}, \code{\link{modeHuntingApprox}}, or \code{\link{modeHuntingBlock}}.}}
                
\value{Returns the set of minimal elements \eqn{\bf{D}^\pm}, corresponding to the set of input intervals
       \eqn{\mathcal{D}^\pm}.}

\references{Minimal intervals were first introduced (although for a different multiscale procedure) on p. 517 in 

Lutz Dümbgen (2002).
Application of Local Rank Tests to Nonparametric Regression.
\emph{Journal of Nonparametric Statistics}, \bold{14}, 511--537.

Rufibach, K. and Walther, G. (2007).
A general criterion for multiscale inference. 
Preprint, Department of Statistics, Stanford University.
}

\author{
Kaspar Rufibach, \email{kaspar.rufibach@gmail.com} 

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{www-stat.stanford.edu/~gwalther}}

\note{Depending on the value of \eqn{min.int}, this function is called by \code{\link{modeHunting}}, \cr 
      \code{\link{modeHuntingApprox}}, and \code{\link{modeHuntingBlock}}.}

\keyword{htest}
\keyword{nonparametric}
