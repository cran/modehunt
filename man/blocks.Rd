\name{blocks}

\alias{blocks}

\title{Computes number of observations for each block}

\description{In Rufibach and Walther (2007) a new multiscale mode hunting procedure is presented
that compares the local test statistics with critical values given by blocks. Blocks are collection
of intervals on a given grid that contain roughly the same number of original observations.
}

\usage{blocks(n, m0 = 10, fm = 2)}

\arguments{
  \item{n}{Number of observations.}
  \item{m0}{Initial parameter that determines the number of observations in one block.}
  \item{fm}{Factor by which \eqn{m} is increased from block to block.}
}

\details{
In our block procedure, we only consider a subset \eqn{\mathcal{I}_{app}} of all possible intervals
\eqn{\mathcal{I}_{all}} where

\deqn{\mathcal{I}_{all} = \Bigl\{(j, \ k ) \ : \ 0 \le j < k \le n+1, \ k - j > 1\Bigr\}.}

This subset \eqn{\mathcal{I}_{app}} is computed as follows: 

Set \eqn{d_1, m_1, f_m > 1}. Then:

\eqn{for \ \ r = 1,\ldots,\#blocks}

\eqn{d_r := round(d_1 f_m^{(r-1)/2}), \ m_r := m_1 f_m^{r-1}.}

Include \eqn{(j,k)} in \eqn{\mathcal{I}_{app}} if 

(a) \eqn{j, k \in \{1+i d_r, \ i = 0, 1, \dots \}} \ \ (we only consider every \eqn{d}--th observation) and 

(b) \eqn{m_r \le k-j-1 \le 2m_r-1} \ \ (\eqn{\mathcal{I}_{jk}} contains between \eqn{m_r} and \eqn{2m_r - 1} observations)

\eqn{end \ \ for}

}

\value{
  \eqn{b \times 2}--matrix, where \eqn{b} is the number of blocks and the columns contain the lower
  and the upper number of observations that form each block.
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

\note{The asymptotic results in Rufibach and Walther (2007) are only derived for \eqn{f_m = 2}.}

\seealso{This function is called by \code{\link{modeHuntingBlock}}.}

\keyword{htest}
\keyword{nonparametric}

