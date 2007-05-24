"preProcessX" <-
function (X.raw, lower = -Inf, upper = Inf) 
{
    n.raw <- length(X.raw)
    if (lower == 0 & upper == Inf) {
        n <- n.raw + 1
        X <- 1:n * 0
        X[2:n] <- sort(X.raw)
    }
    if (lower == 0 & upper == 1) {
        n <- n.raw + 2
        X <- 1:n * 0
        X[2:(n - 1)] <- sort(X.raw)
        X[n] <- 1
    }
    if (lower == -Inf & upper == 0) {
        n <- n.raw + 1
        X <- 1:n * 0
        X[1:(n - 1)] <- sort(X.raw)
    }
    if (lower > -Inf & upper < Inf) {
        n <- n.raw + (lower > -Inf) + (upper < Inf)
        X <- 1:n * 0
        if (lower > -Inf) {
            X[1] <- lower
            X[2:(n.raw + 1)] <- sort(X.raw)
        }
        else {
            X[1:n.raw] <- sort(X.raw)
        }
        if (upper < Inf) {
            X[n] <- upper
        }
    }
    if (lower == -Inf & upper == Inf) {
        n <- n.raw
        X <- sort(X.raw)
    }
    d <- diff(X)
    if (min(d) == 0) {
        X <- X + 10^(-3) * mean(abs(d)) * rnorm(n)
        X <- sort(X)
        cat("Warning: Data contain ties and have been perturbed a litte!")
    }
    X <- matrix(sort(X), ncol = 1)
    return(X)
}
