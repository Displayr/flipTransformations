#' Weighted Singular Value Decomposition
#' \code{WeightedSVD} Computes a SVD with frequency weights.
#' @param x A numeric or complex matrix whose SVD decomposition is to be computed. Logical matrices are coerced to numeric.
#' @param weights Frequency Weights.
#' @param nu The number of left singular vectors to be computed. This must between 0 and n = nrow(x).
#' @param nv The number of right singular vectors to be computed. This must between 0 and n = ncol(x).
#' @export
WeightedSVD <- function(x, weights = rep(1, n), nu = min(n, p), nv = min(n, p))
{
  n <-nrow(x)
  p <- ncol(x)
  if(!is.matrix(x))
    x <- as.matrix(x)
  x.eigen <- eigen(t(x) %*% sweep(x, 1, weights, "*"))
  d <- sqrt(x.eigen$values)
  v <- x.eigen$vectors
  if (nu == 0L)
    return(list(d = d[1:nv], v = as.matrix(v[, 1:nv])))
  u <- t(solve(sweep(v, 2, d, "*"), t(x)))
  list(d = d[1:max(nu, nv)],
       u = as.matrix(u[, 1:nu]),
       v = as.matrix(v[, 1:nv]))
}

