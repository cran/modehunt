\name{myRound}
\alias{myRound}
\title{Round 5 up to the next higher integer}
\description{The built-in \R function \code{\link{round}} rounds a 5 to the even digit. Instead, we preferred
the more intuitive rounding meaning that a 5 is always rounded to the next higher digit. 
}
\usage{myRound(d)}
\arguments{\item{d}{Real number.}}
\value{The biggest integer not bigger than \eqn{d} if \eqn{d - \lfloor d \rfloor < 0.5} and the smallest integer greater than
\eqn{d} if \eqn{d - \lfloor d \rfloor \ge 0.5}.}
\author{
Kaspar Rufibach, \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{https://gwalther.su.domains/}}

\seealso{The built-in \R function \code{\link{round}}.}
\examples{
x <- c(1.5, 2.5)

## built in R function
round(x)
## [1] 2 2

## this function
myRound(x)
## [1] 2 3
}

\keyword{arith}
