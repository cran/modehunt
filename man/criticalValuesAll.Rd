\name{criticalValuesAll}

\alias{criticalValuesAll}

\title{Compute critical values based on the set of all intervals}

\description{This function computes critical values that are needed to perform the multiscale analysis about a 
density using the function \code{\link{modeHunting}}.}

\usage{criticalValuesAll(n, alpha, M, display, path)}

\arguments{
  \item{n}{Number of observations.}
  \item{alpha}{Significance level, real number in \eqn{(0,1)}.}
  \item{M}{Number of runs to perform.}
  \item{display}{If \code{display == 1}, every \eqn{100}--th step is indicated in the output window, else not.}
  \item{path}{If \code{path != NA}, the current number of performed simulations is saved in this location.}
}

\details{For more details see the function \code{\link{modeHunting}} and the data set \code{\link{cvModeAll}}.}

\value{A 2-dimensional vector containing the critical value for the test statistic with or without additive 
       correction \eqn{\Gamma}.
       }

\references{
Rufibach, K. and Walther, G. (2007).
A general criterion for multiscale inference. 
Preprint, Department of Statistics, Stanford University.
}

\author{
Kaspar Rufibach, \email{kaspar.rufibach@gmail.com} 

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{www-stat.stanford.edu/~gwalther}
}

\seealso{The resulting critical values can be used by the function \code{\link{modeHunting}}. Critical values
         for some combinations of \eqn{n} and \eqn{\alpha} are available in \code{\link{cvModeAll}}.}
         
\examples{
## compute critical values and compare to those in cvModeAll 
## (to see output in R, press CTRL + W)
cv1 <- criticalValuesAll(n = 200, alpha = 0.05, M = 10 ^ 2, display = 1, path = NA)
data(cvModeAll)
cv2 <- cvModeAll[cvModeAll$alpha == 0.05 & cvModeAll$n == 200, 3:4]
rbind(cv1, cv2)
}

\keyword{htest}
\keyword{nonparametric}
