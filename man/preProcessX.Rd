\name{preProcessX}
\alias{preProcessX}
\title{Prepare data vector according to available information on support endpoints of f}
\description{Preprocesses the initial data vector X.raw according to whether the upper and/or lower endpoint of the support
of f is known.
}
\usage{preProcessX(X.raw, lower = -Inf, upper = Inf)}
\arguments{
  \item{X.raw}{Vector of observations.}
  \item{lower}{Lower support point of \eqn{f}, if known.}
  \item{upper}{Upper support point of \eqn{f}, if known.}
}
\details{Depending whether \eqn{lower} and \eqn{upper} are known, the vector of raw observations \eqn{X.raw}
is supplemented by \eqn{lower} and/or \eqn{upper} and finally sorted.}

\value{Sorted vector of (processed) observations.}

\author{
Kaspar Rufibach, \email{kaspar.rufibach@gmail.com} 

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{www-stat.stanford.edu/~gwalther}}

\note{This function is called by \code{\link{modeHunting}}, \code{\link{modeHuntingApprox}}, 
      and \code{\link{modeHuntingBlock}}.
      
      This function was derived from MatLab code provided on Lutz Duembgen's webpage, \cr \url{http://www.staff.unibe.ch/duembgen}.   
}

\keyword{htest}
\keyword{nonparametric}
