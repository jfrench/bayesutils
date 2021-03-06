#' The Inverse Gamma Distribution
#'
#' Density, distribution function, quantile function, and random generation for
#' the inverse gamma distribution with parameters \code{shape} and \code{scale}. Note that
#' the parameterization is consistent with Gelman et al. (2013) (see References) and
#' that \code{scale} precedes \code{rate} in the argument list, which is the
#' reverse of the \code{\link[stats]{GammaDist}} functions.
#'
#' If \code{scale} is omitted, it assumes the default value of 1.
#'
#' The inverse gamma distribution with parameters \code{shape = a} and \code{scale = s} has density
#'
#' \deqn{f(x)= s^a/Gamma(a) x^(-a-1) e^-(s/x)}
#'
#' for \eqn{x \ge 0}, \eqn{a > 0} and \eqn{s > 0}. (Here \eqn{Gamma(a)} is the function implemented by R's
#' \code{gamma()} and defined in its help. Note that \eqn{a = 0} corresponds to the trivial distribution with all mass at point 0.)
#'
#' The mean and variance are \eqn{E(X) = s/(a - 1)} for \eqn{a>1} and \eqn{Var(X) = s^2/(a-1)^2/(a-2)} for \eqn{a>2}.
#'
#' The cumulative hazard \eqn{H(t) = - log(1 - F(t))} is
#' \code{-pinvgamma(t, ..., lower = FALSE, log = TRUE)}
#'
#' @inheritParams stats::GammaDist
#' @return
#' \code{dinvgamma} gives the density, \code{pinvgamma} gives the distribution function,
#' \code{qinvgamma} gives the quantile function, and \code{rinvgamma}
#' generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN}, with a warning.
#'
#' The length of the result is determined by \code{n} for \code{rinvgamma},
#' and is the maximum of the lengths of the numerical arguments for the
#' other functions.
#'
#' The numerical arguments other than \code{n} are recycled to the length
#' of the result. Only the first elements of the logical arguments are used.
#' @export
#' @references Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). Bayesian data analysis, 3rd edition. CRC press.
#' @name InvGammaDist
#' @rdname InvGammaDist
#' @seealso \code{\link[stats]{GammaDist}}
#' @examples
#' -log(dinvgamma(1:4, shape = 1))
#' p <- (1:9)/10
#' pinvgamma(qinvgamma(p, shape = 2), shape = 2)
#' 1 - 1/exp(qinvgamma(p, shape = 1))
rinvgamma <- function(n, shape, scale = 1, rate = 1/scale) {
  if (!missing(rate) && !missing(scale)) {
    if (abs(rate * scale - 1) < 1e-15) {
      warning("specify 'rate' or 'scale' but not both")
    } else {
      stop("specify 'rate' or 'scale' but not both")
    }
  }
  1/stats::rgamma(n, shape = shape, scale = rate)
}

#' @rdname InvGammaDist
#' @export
dinvgamma <- function(x, shape, scale = 1, rate = 1/scale, log = FALSE) {
  if (!missing(rate) && !missing(scale)) {
    if (abs(rate * scale - 1) < 1e-15) {
      warning("specify 'rate' or 'scale' but not both")
    } else {
      stop("specify 'rate' or 'scale' but not both")
    }
  }
  if (!log) {
    stats::dgamma(1/x, shape = shape, scale = rate, log = log)/x^2
  } else {
    stats::dgamma(1/x, shape = shape, scale = rate, log = log) - 2 * log(x)
  }
}

#' @rdname InvGammaDist
#' @export
pinvgamma <- function(q, shape, scale = 1, rate = 1/scale, lower.tail = TRUE, log.p = FALSE) {
  if (!missing(rate) && !missing(scale)) {
    if (abs(rate * scale - 1) < 1e-15) {
      warning("specify 'rate' or 'scale' but not both")
    } else {
      stop("specify 'rate' or 'scale' but not both")
    }
  }
  stats::pgamma(1/q, shape = shape, scale = rate, lower.tail = !lower.tail, log.p = log.p)
}

#' @rdname InvGammaDist
#' @export
qinvgamma <- function(p, shape, scale = 1, rate = 1/scale, lower.tail = TRUE, log.p = FALSE) {
  if (!missing(rate) && !missing(scale)) {
    if (abs(rate * scale - 1) < 1e-15) {
      warning("specify 'rate' or 'scale' but not both")
    } else {
      stop("specify 'rate' or 'scale' but not both")
    }
  }
  1/stats::qgamma(1 - p, shape = shape, scale = rate, lower.tail = lower.tail, log.p = log.p)
}
