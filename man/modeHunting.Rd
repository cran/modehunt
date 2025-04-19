\name{modeHunting}
\alias{modeHunting}
\title{Multiscale analysis of a density on all possible intervals}
\description{Simultanous confidence statements for the existence and location of local increases and decreases 
             of a density f, computed on all intervals spanned by two observations.}
\usage{
modeHunting(X.raw, lower = -Inf, upper = Inf, crit.vals, min.int = FALSE)
}
\arguments{
  \item{X.raw}{Vector of observations.}
  \item{lower}{Lower support point of \eqn{f}, if known.}
  \item{upper}{Upper support point of \eqn{f}, if known.}
  \item{crit.vals}{2-dimensional vector giving the critical values for the desired level.}
  \item{min.int}{If \code{min.int = TRUE}, the set of minimal intervals is output, otherwise all intervals with a test 
                 statistic above the critical value are given.}
}
\details{
In general, the methods \code{\link{modeHunting}}, \code{\link{modeHuntingApprox}}, and 
         \code{\link{modeHuntingBlock}} compute for a given level \eqn{\alpha \in (0, 1)} and the corresponding
         critical value \eqn{c_{jk}(\alpha)} two sets of intervals

\deqn{\mathcal{D}^\pm(\alpha) = \Bigl\{ \mathcal{I}_{jk} \ : \ \pm T_{jk}({\bf{X}} ) > c_{jk}(\alpha) \Bigr\}}

where \eqn{\mathcal{I}_{jk}:=(X_{(j)},X_{(k)})} for \eqn{0\le j < k \le n+1, k-j> 1} and \eqn{c_{jk}} are
appropriate critical values.

Specifically, the function \code{\link{modeHunting}} computes \eqn{\mathcal{D}^\pm(\alpha)} based on the two 
test statistics

\deqn{T_n^+({\bf{X}}, \mathcal{I}) = \max_{(j,k) \in \mathcal{I}} \Bigl( |T_{jk}({\bf{X}})| / \sigma_{jk} - \Gamma \Bigl(\frac{k-j}{n+2}\Bigr)\Bigr)}

and

\deqn{T_n({\bf{X}}, \mathcal{I}) = \max_{(j,k) \in \mathcal{I}} ( |T_{jk}({\bf{X}})| / \sigma_{jk} ),}


using the set \eqn{\mathcal{I} := \mathcal{I}_{all}} of all intervals spanned by two observations 
\eqn{(X_{(j)}, X_{(k)})}:

\deqn{\mathcal{I}_{all} = \Bigl\{(j, \ k ) \ : \ 0 \le j < k \le n+1, \ k - j > 1\Bigr\}.}

We introduced the local test statistics

\deqn{T_{jk}({\bf{X}}) := \sum_{i=j+1}^{k-1} ( 2 X_{(i; j, k)} - 1) 1\{X_{(i; j, k)} \in (0,1)\},}

for local order statistics

\deqn{X_{(i; j, k)} := \frac{X_{(i)}-X_{(j)}}{X_{(k)} - X_{(j)}},}

the standard deviation \eqn{\sigma_{jk} :=  \sqrt{(k-j-1)/3}} and the additive correction term 
\eqn{\Gamma(\delta) := \sqrt{2 \log(e / \delta)}} for \eqn{\delta > 0}.

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

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{https://gwalther.su.domains/}
}

\note{Critical values for \code{\link{modeHunting}} and some combinations of \eqn{n} and \eqn{\alpha} are provided in the 
data set \code{\link{cvModeAll}}. Critical values for other values of \eqn{n} and \eqn{\alpha} can be generated 
using \code{\link{criticalValuesAll}}.

Parts of this function were derived from MatLab code provided by Lutz Duembgen.}
%
\seealso{\code{\link{modeHuntingApprox}}, \code{\link{modeHuntingBlock}}, and \code{\link{cvModeAll}}.}
\examples{
## for examples type
help("mode hunting")
## and check the examples there}

\keyword{htest}
\keyword{nonparametric}

