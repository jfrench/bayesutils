#' The (scaled) Inverse Chi-squared Distribution
#'
#' Density, distribution function, quantile function, and random generation for
#' the (scaled) inverse chi-square distribution with \code{df} degrees of freedom and
#' optional parameter \code{scale}. The parameterization is consistent with Gelman et al. (2013). By default,
#' the \code{scale} is set to return results for the inverse chi-square distribution. If
#' \code{scale} is changed, then the results are returned for the scaled inverse chi-squared distribution.
#'
#' If \code{scale} is omitted, it assumes the default value of \code{1/df}.
#'
#' The scaled inverse chi-square distribution with parameters \code{df = n} and \code{scale = s^2} has density
#'
#' \deqn{f(x)= (2^{-n/2}/Gamma(n/2)s^n x^(-n/2 - 1) e^(-n s^2/(2x))}
#'
#' for \eqn{x \ge 0}, \eqn{n > 0} and \eqn{s > 0}. (Here \eqn{Gamma(n/2)} is the function implemented by R's
#' \code{gamma()} and defined in its help. Note that \eqn{n = 0} corresponds to the trivial distribution with all mass at point 0.)
#'
#' The mean and variance are \eqn{E(X) = n/(n - 2) s^2} for \eqn{n > 2} and \eqn{Var(X) = 2n^2/(n-2)^2/(n-4) s^4} for \eqn{n>4}.
#'
#' The cumulative hazard \eqn{H(t) = - log(1 - F(t))} is
#' \code{-pinvchisq(t, ..., lower = FALSE, log = TRUE)}
#'
#' @name InvChiSquare
#' @inheritParams InvGammaDist
#' @inheritParams stats::Chisquare
#' @return
#' \code{dinvchisq} gives the density, \code{pinvchisq} gives the distribution function,
#' \code{qinvchisq} gives the quantile function, and \code{rinvchisq}
#' generates random deviates.
#'
#' Invalid arguments will result in return value \code{NaN}, with a warning.
#'
#' The length of the result is determined by \code{n} for \code{rinvchisq},
#' and is the maximum of the lengths of the numerical arguments for the
#' other functions.
#'
#' The numerical arguments other than \code{n} are recycled to the length
#' of the result. Only the first elements of the logical arguments are used.
#' @export
#' @references Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). Bayesian data analysis, 3rd edition. CRC press.
#' @seealso \code{\link[stats]Chisquare}
#' @examples
#' x = 1:10
#' ## InvChisquare(df = n) is a special case of InvGamma(shape = n/2, scale = 1/2)
#' n = 3
#' all.equal(dinvchisq(x, df = n), dinvgamma(x, shape = n/2, scale = 1/2))
#' all.equal(pinvchisq(x, df = n), pinvgamma(x, shape = n/2, scale = 1/2))
#'
#' ## InvChisquare(df = n, scale = s^2) is a special case of InvGamma(shape = n/2, scale = n/2 * s^2)
#' s = 1.7
#' all.equal(dinvchisq(x, df = n, scale = s^2), dinvgamma(x, shape = n/2, scale = n/2 * s^2))
#' all.equal(pinvchisq(x, df = n, scale = s^2), pinvgamma(x, shape = n/2, scale = n/2 * s^2))
#' @rdname InvChisquare
rinvchisq <- function(n, df, scale = 1/df) {
	rinvgamma(n, shape = df/2, scale = df/2*scale)
}

#' @export
#' @rdname InvChisquare
dinvchisq <- function(x, df, scale = 1/df, log = FALSE) {
	dinvgamma(x, shape = df/2, scale = df/2*scale, log = log)
}

#' @export
#' @rdname InvChisquare
pinvchisq <- function(q, df, scale = 1/df, lower.tail = TRUE, log.p = FALSE) {
	pinvgamma(q, shape = df/2, scale = df/2*scale, lower.tail = lower.tail, log.p = log.p)
}

#' @export
#' @rdname InvChisquare
qinvchisq <- function(p, df, scale = 1/df, lower.tail = TRUE, log.p = FALSE) {
	qinvgamma(p, shape = df/2, scale = df/2*scale, lower.tail = lower.tail, log.p = log.p)
}
