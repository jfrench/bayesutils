#' The Scaled and Shifted Student t Distribution
#'
#' Density, distribution function, quantile function, and random generation for
#' the scaled and shifted t distribution with \code{df} degrees of freedom and
#' optional parameter \code{scale}. The parameterization is consistent with Gelman et al. (2013).
#'
#' If \code{mean} is omitted, it assumes the default value of \code{0}. If \code{sigma} is omitted, it assumes the default value of \code{1}.
#'
#' The scaled and shifted t distribution with parameters \code{df = n}, \code{mean =} \eqn{m} and \code{sigma =} \eqn{s} has density
#'
#' \deqn{f(x)= Gamma((n+1)/2)/Gamma(n/2)/\sqrt{n\pi}/s (1 + 1/n ((x - m)/s)^2)^{-(n+1)/2}}
#'
#' for \eqn{x \ge 0}, \eqn{n > 0} and \eqn{s > 0}. (Here \eqn{Gamma()} is the function implemented by R's
#' \code{gamma()} and defined in its help.
#'
#' The mean and variance are \eqn{E(X) = m} and \eqn{Var(X) = n/(n-2) s^2} for \eqn{n>2}.

#' @inheritParams InvGammaDist
#' @return
#' \code{dst} gives the density, \code{pst} gives the distribution function,
#' \code{qst} gives the quantile function, and \code{rst}
#' generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN}, with a warning.
#'
#' The length of the result is determined by \code{n} for \code{rst},
#' and is the maximum of the lengths of the numerical arguments for the
#' other functions.
#'
#' The numerical arguments other than \code{n} are recycled to the length
#' of the result. Only the first elements of the logical arguments are used.
#' @export
#' @references Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). Bayesian data analysis, 3rd edition. CRC press.
#' @examples
#' 1 - pst(1:5, df = 1, mean = 1, sigma = 1.2)
#' qst(.975, df = c(1:5), mean = 1:5, sigma = 1:5)
#' @export
#' @name ScaledTDist
#' @rdname ScaledTDist
dst <- function(x, df, mean = 0, sigma = 1, log = FALSE) {
  if (any(sigma <= 0)) {
    stop("sigma must be positive")
  }
  if (!log) {
	  dt((x - mean)/sigma, df = df, log = log)/sigma
  } else {
    dt((x - mean)/sigma, df = df, log = log) - log(sigma)
  }
}

#' @export
#' @rdname ScaledTDist
rst <- function(n, df, mean = 0, sigma = 1) {
  if (any(sigma <= 0)) {
    stop("sigma must be positive")
  }
	mean + sigma * rnorm(n) * sqrt(df/rchisq(n, df = df))
}

#' @export
#' @rdname ScaledTDist
qst <- function(p, df, mean = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  if (any(sigma <= 0)) {
    stop("sigma must be positive")
  }
  mean + sigma * qt(p, df = df, lower.tail = TRUE, log.p = FALSE)
}

#' @export
#' @rdname ScaledTDist
pst <- function(q, df, mean = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  if (any(sigma <= 0)) {
    stop("sigma must be positive")
  }
  pt((q - mean)/sigma, df = df, lower.tail = lower.tail, log.p = log.p)
}
