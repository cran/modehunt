\name{modeHuntingApprox}
\alias{modeHuntingApprox}
\title{Multiscale analysis of a density on the approximating set of intervals}
\description{Simultanous confidence statements for the existence and location of local increases and decreases 
             of a density f, computed on the approximating set of intervals.}
             
\usage{
modeHuntingApprox(X.raw, lower = -Inf, upper = Inf, 
    d0 = 2, m0 = 10, fm = 2, crit.vals, min.int = FALSE)
}

\arguments{
  \item{X.raw}{Vector of observations.}
  \item{lower}{Lower support point of \eqn{f}, if known.}
  \item{upper}{Upper support point of \eqn{f}, if known.}
  \item{d0}{Initial parameter for the grid resolution.}
  \item{m0}{Initial parameter for the number of observations in one block.}
  \item{fm}{Factor by which \eqn{m} is increased from block to block.}
  \item{crit.vals}{2-dimensional vector giving the critical values for the desired level.}
  \item{min.int}{If \code{min.int = TRUE}, the set of minimal intervals is output, otherwise all intervals with a test 
                 statistic above the critical value are given.}
}

\details{
See \code{\link{blocks}} for details how \eqn{\mathcal{I}_{app}} is generated and \code{\link{modeHunting}} for 
a proper introduction to the notation used here.
The function \code{\link{modeHuntingApprox}} computes \eqn{\mathcal{D}^\pm(\alpha)} based on the two 
test statistics \eqn{T_n^+({\bf{X}}, \mathcal{I}_{app})} and \eqn{T_n({\bf{X}}, \mathcal{I}_{app})}. 

If \code{min.int = TRUE}, the set \eqn{\mathcal{D}^\pm(\alpha)} is replaced by the set \eqn{{\bf{D}}^\pm(\alpha)}
of its \emph{minimal elements}. An interval \eqn{J \in \mathcal{D}^\pm(\alpha)} is called \emph{minimal} if 
\eqn{\mathcal{D}^\pm(\alpha)} contains no proper subset of \eqn{J}. This \emph{minimization} post-processing 
step typically massively reduces the number of intervals. If we are mainly interested in locating the ranges
of increases and decreases of \eqn{f} as precisely as possible, the intervals in 
\eqn{\mathcal{D}^\pm(\alpha) \setminus \bf{D}^\pm(\alpha)} do not contain relevant information.
}

\value{
  \item{Dp}{The set \eqn{\mathcal{D}^+(\alpha)} (or \eqn{\bf{D}^+(\alpha)}), based on the test statistic with additive correction \eqn{\Gamma}.}
  \item{Dm}{The set \eqn{\mathcal{D}^-(\alpha)} (or \eqn{\bf{D}^-(\alpha)}), based on the test statistic with \eqn{\Gamma}.}
  \item{Dp.noadd}{The set \eqn{\mathcal{D}^+(\alpha)} (or \eqn{\bf{D}^+(\alpha)}), based on the test statistic without \eqn{\Gamma}.}  
  \item{Dm.noadd}{The set \eqn{\mathcal{D}^+(\alpha)} (or \eqn{\bf{D}^-(\alpha)}), based on the test statistic without \eqn{\Gamma}.}
}

\references{
Duembgen, L. and Walther, G. (2008).
Multiscale Inference about a density.
\emph{Ann. Statist.}, \bold{36}, 1758--1785.

Rufibach, K. and Walther, G. (2010).
A general criterion for multiscale inference. 
\emph{J. Comput. Graph. Statist.}, \bold{19}, 175--190.
}

\author{
Kaspar Rufibach, \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch} 

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{www-stat.stanford.edu/~gwalther}
}

\note{Critical values for \code{\link{modeHuntingApprox}} and some combinations of \eqn{n} and \eqn{\alpha} are 
provided in the data set \code{\link{cvModeApprox}}. Critical values for other
values of \eqn{n} and \eqn{\alpha} can be generated using \code{\link{criticalValuesApprox}}.
}
%

\seealso{\code{\link{modeHunting}}, \code{\link{modeHuntingBlock}}, and \code{\link{cvModeApprox}}.}

\examples{
## for examples type
help("mode hunting")
## and check the examples there}

\keyword{htest}
\keyword{nonparametric}

