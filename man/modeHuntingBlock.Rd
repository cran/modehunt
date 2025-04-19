\name{modeHuntingBlock}
\alias{modeHuntingBlock}
\title{Multiscale analysis of a density via block procedure}
\description{Simultanous confidence statements for the existence and location of local increases and decreases 
             of a density f, computed via the block procedure.}
             
\usage{
modeHuntingBlock(X.raw, lower = -Inf, upper = Inf, d0 = 2, 
    m0 = 10, fm = 2, crit.vals, min.int = FALSE)
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
                 statistic above the critical value (in their respective block) are given.}
}

\details{
See \code{\link{blocks}} for details how \eqn{\mathcal{I}_{app}} is generated and \code{\link{modeHunting}} for 
a proper introduction to the notation used here.
The function \code{\link{modeHuntingBlock}} uses the test statistic \eqn{T^+_n({\bf X}, \mathcal{B}_r)},
where \eqn{\mathcal{B}_r} contains all intervals of Block \eqn{r}, \eqn{r=1,\ldots,\#blocks}. 
Critical values for each block individually are received via finding an \eqn{\tilde \alpha} such that

\deqn{P(B_n({\bf{X}}) > q_{r,\tilde \alpha / (r+tail)^\gamma} \ for \ at \ least \ one \ r) \le \alpha,}

where \eqn{q_{r,\alpha}} is the \eqn{(1-\alpha)}--quantile of the distribution of \eqn{T^+_n({\bf X}, \mathcal{B}_r).}
We then define the sets \eqn{\mathcal{D}^\pm(\alpha)} as

\deqn{\mathcal{D}^\pm(\alpha) := \Bigl\{\mathcal{I}_{jk} \ : \ \pm T_{jk}({\bf{X}}) > q_{r,\tilde \alpha / (r+tail)^\gamma} \, , \ r = 1,\ldots \#blocks\Bigr\}.}

Note that \eqn{\gamma} and \eqn{tail} are automatically determined by \eqn{crit.vals}.

If \code{min.int = TRUE}, the set \eqn{\mathcal{D}^\pm(\alpha)} is replaced by the set \eqn{{\bf{D}}^\pm(\alpha)}
of its \emph{minimal elements}. An interval \eqn{J \in \mathcal{D}^\pm(\alpha)} is called \emph{minimal} if 
\eqn{\mathcal{D}^\pm(\alpha)} contains no proper subset of \eqn{J}. This \emph{minimization} post-processing 
step typically massively reduces the number of intervals. If we are mainly interested in locating the ranges
of increases and decreases of \eqn{f} as precisely as possible, the intervals in 
\eqn{\mathcal{D}^\pm(\alpha) \setminus \bf{D}^\pm(\alpha)} do not contain relevant information.
}

\value{
  \item{Dp}{The set \eqn{\mathcal{D}^+(\alpha)} (or \eqn{\bf{D}^+(\alpha)}).}
  \item{Dm}{The set \eqn{\mathcal{D}^-(\alpha)} (or \eqn{\bf{D}^-(\alpha)}).}
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

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{https://gwalther.su.domains/}}

\note{Critical values for some combinations of \eqn{n} and \eqn{\alpha} are provided in the 
data sets \code{\link{cvModeBlock}}. Critical values for other
values of \eqn{n} and \eqn{\alpha} can be generated using \code{\link{criticalValuesApprox}}.
}
%
\seealso{\code{\link{modeHunting}}, \code{\link{modeHuntingApprox}}, and \code{\link{cvModeBlock}}.}

\examples{
## for examples type
help("mode hunting")
## and check the examples there}

\keyword{htest}
\keyword{nonparametric}

