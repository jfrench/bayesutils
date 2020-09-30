#' Simulate from a Multivariate Normal Distribution
#'
#' @param n the number of samples required
#' @param mu a vector giving the mean of the random variables
#' @param v the covariance matrix of the random variables
#' @inheritParams decomp_cov
#' @return An \eqn{m \times n} where \eqn{n} is the length of \code{mu}.
#' @export
#'
#' @examples
#' v = matrix(c(10,3,3,2), 2, 2)
#' y = rmvnorm(n = 4000, 1:2, v)
#' # should be close to 1:2
#' # rowMeans(y)
#' # should be close to v
#' # var(t(y))
rmvnorm = function(n = 1, mu, v, method = "eigen") {
  mu = as.vector(mu)
  nr = length(mu)

  # check argument validity
  if(n < 0 || !is.finite(n))
  { stop("n should be a non-negative integer")}
  if(!is.numeric(mu))
  { stop("mu must be a numeric vector")}
  if(!is.matrix(v) || nrow(v)!=ncol(v) || !is.numeric(v))
  { stop("v must be a square numeric matrix")}
  if(nr != nrow(v))
  { stop("length(mu)!=nrow(v)")}
  if(!is.element(method, c("chol", "eigen", "svd")))
  { stop("method must be 'chol', 'eigen', or 'svd'")}

  # return simulated values
  mu + decomp_cov(v, method) %*% matrix(rnorm(nr * n), nrow = nr, ncol = n)
}
