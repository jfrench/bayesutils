#' The Inverse Wishart Distribution
#'
#' Random generation for the inverse Wishart distribution. The parameterization
#' is consistent with Gelman et al. (2013) (see References)
#'
#' @name InvWishart
#' @return
#' The dimensionality of the result is determined by \code{n} for \code{rinvwish}.
#' The result will be an array of size \code{(nrow(v), nrow(v), n)}.
#' @export
#' @references Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). Bayesian data analysis, 3rd edition. CRC press.
#' @seealso \code{\link[stats]{rWishart}}
#' @examples
#' Sigma = crossprod(matrix(rnorm(16), nrow = 4))
#' y = rinvwish(3, 4, Sigma)
#' @rdname InvWishart
rinvwish <- function(n, df, v) {
  x = stats::rWishart(n, df, solve(v))
  xl = lapply(seq(dim(x)[3]), function(y) x[ , , y])
  g = lapply(xl, solve)
  g2 = array(unlist(g), dim = c(dim(g[[1]]), length(g)))
  return(g2)
}
