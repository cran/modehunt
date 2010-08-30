\name{criticalValuesApprox}

\alias{criticalValuesApprox}

\alias{criticalValuesBlock}

\title{Compute critical values for (1) the original test statistic with or without additive correction, 
based on the aprroximating set of intervals and (2) for the block procedure}

\description{This function computes critical values that can be used to perform the multiscale analysis about a 
density with the functions \code{\link{modeHuntingApprox}} and \code{\link{modeHuntingBlock}}.}

\usage{criticalValuesApprox(n, d0 = 2, m0 = 10, fm = 2, alpha = 0.05, 
        gam = 2, tail = 10, M = 10 ^ 5, display = 0, path = NA)}

%\usage{criticalValuesApprox(n, \dots)}

\arguments{
  \item{n}{Number of observations.}
  \item{d0}{Initial parameter for the grid resolution.}
  \item{m0}{Initial parameter for the number of observations in one block.}
  \item{fm}{Factor by which \eqn{m} is increased from block to block.}
  \item{alpha}{Significance level, real number in \eqn{(0,1)}.}
  \item{gam}{Weighting exponent for level in each block.}
  \item{tail}{Offset, determines together with \code{gam} the decrease of the level from one block to another.}
  \item{M}{Number of runs to perform.}
  \item{display}{If \code{display == 1}, every \eqn{100}--th step is indicated in the output window, else not.}
  \item{path}{If \code{path != NA}, the current number of performed simulations is saved in this location.}
}

\details{For details see the function \code{\link{modeHuntingApprox}} and the data set \code{\link{cvModeApprox}}.}

\value{
  \item{approx}{A 2-dimensional vector containing the critical value for the test statistic with or without
                additive correction \eqn{\Gamma}.}
  \item{block}{A vector containing the critical value for each block.}             
}

\references{
Rufibach, K. and Walther, G. (2010).
A general criterion for multiscale inference. 
\emph{J. Comput. Graph. Statist.}, \bold{19}, 175--190.
}

\author{
Kaspar Rufibach, \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html} 

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{www-stat.stanford.edu/~gwalther}
}

\note{The asymptotic results in Rufibach and Walther (2010) are only derived for \eqn{f_m = 2}.}

\seealso{The resulting critical values are used by the functions \code{\link{modeHuntingApprox}} 
         and \cr \code{\link{modeHuntingBlock}}. Critical values
         for some combinations of \eqn{n} and \eqn{\alpha} are available in \code{\link{cvModeApprox}} and
         \code{\link{cvModeBlock}}.}
         
\examples{
## compute critical values and compare to those in cvModeAll and cvModeBlock
## (to see output in R, press CTRL + W)
cv <- criticalValuesApprox(n = 200, d0 = 2, m0 = 10, fm = 2, 
     alpha = 0.05, gam = 2, tail = 10, M = 10 ^ 2, display = 1, path = NA)
cv1 <- cv$approx; cv2 <- cv$block

data(cvModeApprox); data(cvModeBlock)
cv3 <- cvModeApprox[cvModeApprox$alpha == 0.05 & cvModeApprox$n == 200, 3:4]
cv4 <- cvModeBlock[cvModeBlock$alpha == 0.05 & cvModeBlock$n == 200, 3:6]
rbind(cv1, cv3)
rbind(cv2, cv4)
}

\keyword{htest}
\keyword{nonparametric}
