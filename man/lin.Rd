\name{lin}
\alias{lin}
\alias{dlin}
\alias{plin}
\alias{qlin}
\alias{rlin}
\title{Perturbed Uniform Distribution}
\description{Density function, distribution function, quantile function and
random generation for the perturbed uniform distribution having a linear increase of slope \eqn{s} 
on an interval \eqn{[a,b] \in [0,1]}.}
\usage{
dlin(x, a, b, s) 
plin(q, a, b, s) 
qlin(p, a, b, s)
rlin(n, a, b, s)}

\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations.}
  \item{a}{Left interval endpoint, real number in \eqn{[0,1)}.}
  \item{b}{Right interval endpoint, real number in \eqn{(0,1]}.}
  \item{s}{Slope parameter, real number such that \eqn{|s| \le 2/(b-a)}.}  
}
\details{
     The what we call perturbed uniform distribution (PUD) with perturbation on an 
     interval \eqn{[a,b] \in [0,1]} with slope parameter \eqn{s} such that
     \eqn{|s| \le 2 / (b-a)} has density function
     
     \deqn{f_{a, b, s}(x) = \Bigl(sx-s\frac{a+b}{2}\Bigr)1\{x \in [a,b)\} + 1\{[0,a) \cup [b,1]\},}

     distribution function
     
     \deqn{F_{a, b, s}(q) = \Bigl(q+\frac{s}{2}(q^2-a^2+(a-x)(a+b)) \Bigr)1\{q \in [a,b)\} + q\{[0,a) \cup [b,1]\},}
     
     and quantile function
     
     \deqn{F_{a, b, s}^{-1}(p) = \Bigl(-s^{-1}+\frac{a+b}{2}+\frac{s \sqrt{(a-b)^2+\frac{4}{s}(\frac{1}{s}-(a+b)+2p)}}{2|s|}  \Bigr) \ 1\{p \in [a,b)\} + p\{[0,a) \cup [b,1]\}.}
     
     This function was used to carry out the simulations to compute the power curves given in Rufibach and Walther (2010).
}
\value{
     \code{\link{dlin}} gives the values of the density function, \code{\link{plin}} those of the distribution
     function, and \code{\link{qlin}} those of the quantile function of the PUD at \eqn{x, q,} and \eqn{p}, 
     respectively. \code{\link{rlin}} generates \eqn{n} random numbers, returned as an ordered vector.
}
\references{
Rufibach, K. and Walther, G. (2010).
A general criterion for multiscale inference. 
\emph{J. Comput. Graph. Statist.}, \bold{19}, 175--190.
}
\author{
Kaspar Rufibach, \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch} 

Guenther Walther, \email{gwalther@stanford.edu}, \cr \url{https://gwalther.su.domains/}}

\keyword{htest}
\keyword{nonparametric}
