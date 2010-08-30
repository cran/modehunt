\name{modehunt-package}
\alias{modehunt-package}
\alias{modehunt}
\alias{mode hunting}
\alias{multiscale}
\docType{package}
\title{Multiscale analysis about a density}

\description{This package provides five methods and corresponding critical values to perform mode hunting, i.e. to 
compute multiscale test statistics based on local order statistics and spacings that provide simultaneous confidence 
statements for the existence and location of local increases and decreases of a density.}

\details{
\tabular{ll}{
Package: \tab modehunt \cr
Type: \tab Package \cr
Version: \tab 1.0.5 \cr
Date: \tab 2010-08-30 \cr
License: \tab GPL (>=2) \cr
}

In Duembgen and Walther (2008) a multiscale test statistic based on spacings was introduced. This method provides 
simultaneous confidence statements for the existence and location of local increases and decreases of a density. 
The procedure guarantees finite--sample significance levels and possesses certain asymptotic optimality 
and adaptivity properties. However, since the local test statistics are computed on all \eqn{O(n^2)} intervals in 
the set 

\deqn{\mathcal{I}_{all} = \Bigl\{(j, \ k ) \ : \ 0 \le j < k \le n+1, \ k - j > 1\Bigr\},}
 
this latter procedure is computationally very expensive. Furthermore, the correction term \eqn{\Gamma} employed
by Duembgen and Walther (2008) to prevent the global test statistic to be dominated by the values of the local 
test statistics on small scales needs in principle to be re--derived for any new local test statistic, a non--trivial
task in general. In Rufibach and Walther (2010), two new procedures are proposed: One that within the original framework
of Duembgen and Walther (2008) approximates the set \eqn{\mathcal{I}_{all}} by a specific subset of intervals 
\eqn{\mathcal{I}_{app}} that only contains \eqn{O(n \log n )} intervals. It is shown that considering 
\eqn{\mathcal{I}_{app}} yields a procedure that is in terms of power asymptotically equivalent to that based 
on \eqn{\mathcal{I}_{all}}, however, computationally much more efficient.

Finally, Rufibach and Walther (2010) propose a \emph{block procedure}. Here, all intervals under consideration are
grouped into blocks, where each interval in a block contains approximately the same number of original observations.
Critical values are then computed per block. Again, this procedure is basically asymptotically equivalent to the
standard approach proposed in Duembgen and Walther (2008), but again computationally much faster. It further offers
a (finite--sample) tradeoff between employing or omitting the additive correction \eqn{\Gamma}.

The initial procedure by Duembgen and Walther (2008) is implemented as the function
\code{\link{modeHunting}}. The help file to the latter function also contains some more description of the mathematical details.
\code{\link{criticalValuesAll}} can be used to compute critical values for this approach and 
\code{\link{cvModeAll}} contains a table of critical values (with and without correction term) for some 
\eqn{n} and \eqn{alpha}.

The corresonding functions and \eqn{p}-values for the approximation are made available as 
\code{\link{modeHuntingApprox}}, \code{\link{criticalValuesApprox}}, and \code{\link{cvModeApprox}}
and for the block method as \code{\link{modeHuntingBlock}}, \code{\link{criticalValuesBlock}}, and
\code{\link{cvModeBlock}}.
}

\author{
Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{www-stat.stanford.edu/~gwalther}

Kaspar Rufibach acknowledges support by the Swiss National Science Foundation SNF, \url{http://www.snf.ch}.

Guenther Walther acknowledges support by NSF grants DMS-9875598, DMS-0505682, and NIH grant 5R33HL068522.
}

\references{
Duembgen, L. and Walther, G. (2008).
Multiscale Inference about a density.
\emph{Ann. Statist.}, \bold{36}, 1758--1785.

Rufibach, K. and Walther, G. (2010).
A general criterion for multiscale inference. 
\emph{J. Comput. Graph. Statist.}, \bold{19}, 175--190.
}

\keyword{htest}
\keyword{nonparametric}

\examples{
## generate random sample
set.seed(1977)
n <- 200; a <- 0; b <- 0.5; s <- 2 / (b - a) 
X.raw <- rlin(n, a, b, s)

## input critical values
alpha <- 0.05
data(cvModeAll); data(cvModeApprox); data(cvModeBlock)
cv.all <- cvModeAll[cvModeAll$alpha == alpha & cvModeAll$n == n, 3:4]
cv.approx <- cvModeApprox[cvModeApprox$alpha == alpha & cvModeApprox$n == n, 3:4]
cv.block <- cvModeBlock[cvModeBlock$alpha == alpha & cvModeBlock$n == n, 3:11]

## standard procedure from Duembgen and Walther (2008)
mod1 <- modeHunting(X.raw, lower = 0, upper = 1, cv.all, min.int = TRUE)

## procedure from Rufibach and Walther (2010) based on I_app
mod2 <- modeHuntingApprox(X.raw, lower = 0, upper = 1, 
                          crit.vals = cv.approx, min.int = TRUE)

## block procedure from Rufibach and Walther (2010)
mod3 <- modeHuntingBlock(X.raw, lower = 0, upper = 1, 
                         crit.vals = cv.block, min.int = TRUE)

## display
mod1; mod2; mod3
}
