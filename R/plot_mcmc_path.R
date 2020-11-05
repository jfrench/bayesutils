#' Plot path of MCMC chains
#'
#' This will produce a simple plot for visualizing the path of the variable steps (one variable
#' moving at a time) or cycles (the movement of the variables after completing
#' a cycle) for a list of MCMC chains stored as a list. The starting place of
#' the chain *the first row of each chain) is denoted by a point. See Details.
#'
#' The function currently only works for chains with two variables stored as
#' a 2-column matrix or data frame. Each column contains the MCMC samples of
#' each variable.
#'
#' The paths are distinguished by using different line colors. By default,
#' the \code{"RdYlBu"} palette from \code{\link[grDevices]{hcl.colors}} is used.
#' It is unlikely this color scheme will work well for more than 5 chains.
#'
#' @param x A matrix with 2 columns or a list of 2 column matrices
#' @param ncycles The number of cycles to plot
#' @inheritParams grDevices::palette
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param type Type of path to plot. \code{"step"} moves horizontally
#' and vertically for each variable step within the MCMC cycles. \code{"cycle"} plots
#' the variable movement for each completed MCMC cycle.
#' @param ... Additional parameters for the \code{[graphics]{plot.default}} function.
#' @return A plot.
#' @export
#' @examples
#' x1 = cbind(rnorm(3), rchisq(3, df = 1))
#' x2 = cbind(rnorm(3), rchisq(3, df = 1))
#' x = list(x1, x2)
#' plot_mcmc_path(x)
#' # change point type
#' plot_mcmc_path(x, type = "cycle", pch = 20)
plot_mcmc_path <- function(x, ncycles = 10, palette = "RdYlBu", xlim, ylim,
                           xlab = "var 1", ylab = "var 2",
                           type = "step", ...) {
  type = match.arg(type, c("step", "cycle"))
  if (!is.list(x)) {
    x = list(x)
  }

  for (i in seq_along(x)) {
    if (ncol(x[[i]]) != 2 | is.null(ncol(x[[i]]))) {
      stop("x must have two columns (or be a list of two column matrix-like objects")
    }
  }

  # get sensible x and y limits
  if (missing(xlim)) {
    xlim = c(Inf, -Inf)
    for (i in seq_along(x)) {
      xi1 = x[[i]][,1]
      xi1 = xi1[seq_len(pmin(ncycles, length(xi1)))]
      xlim = c(pmin(xlim[1], min(xi1)),
               pmax(xlim[2], max(xi1)))
    }
  }
  if (missing(ylim)) {
    ylim = c(Inf, -Inf)
    for (i in seq_along(x)) {
      xi2 = x[[i]][,2]
      xi2 = xi2[seq_len(pmin(ncycles, length(xi2)))]
      ylim = c(pmin(ylim[1], min(xi2)),
               pmax(ylim[2], max(xi2)))
    }
  }
  # need at least 2 colors
  mycol = grDevices::hcl.colors(pmax(2, length(x)), palette = palette)
  # plot empty canvas
  base::plot(xlim, ylim, xlim = xlim, ylim = ylim, xlab = xlab,
             ylab = ylab, type = "n", ...)
  # plot steps or cycles
  # make sure there are enough rows in x to plot
  for (i in seq_along(x)) {
    xi = x[[i]]
    graphics::points(xi[1, 1], xi[1, 2], col = mycol[i], ...)
    if (type == "step") {
      for (j in seq_len(min(ncycles, nrow(xi) - 1))) {
        graphics::lines(c(xi[j, 1], xi[j+1, 1]), c(xi[j, 2], xi[j, 2]),  col = mycol[i], ...)
        graphics::lines(c(xi[j + 1, 1], xi[j + 1, 1]), c(xi[j, 2], xi[j + 1, 2]), col = mycol[i], ...)
      }
    } else if (type == "cycle") {
      nx = seq_len(min(ncycles, nrow(xi)))
      graphics::lines(xi[nx, 1], xi[nx,2], col = mycol[i], )
    }
  }
}
