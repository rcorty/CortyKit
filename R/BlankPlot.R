#' @title BlankPlot Function
#' @name BlankPlot
#' @author Robert Corty
#'
#' @description This function draws a plot using base \R
#' graphics with no axes, no axis labels, no title, and no data.
#' It should usually be used in conjunction with other functions for
#' plotting in base \R, such as \code{points}, \code{lines},
#' \code{rect}, etc.
#'
#' @param xlim x axis limits for the plot, as in \code{plot}
#' @param ylim y axis limits for the plot, as in \code{plot}
#' @param ... additional graphical parameters
#'
#' @return No return value.  Just draws the plot.
#'
#' @examples
#' BlankPlot()
#' points(x = runif(n = 10), y = runif(n = 10))
#' BlankPlot(xlim = c(0, 100), ylim = c(0, 100))
#' lines(x = rnorm(n = 10, mean = 50, sd = 10),
#'       y = rnorm(n = 10, mean = 50, sd = 10))
#' BlankPlot(main = "My Great Plot")
#'
#' @export
#'
BlankPlot <- function(xlim = c(0, 1), ylim = c(0, 1), ...) {
  graphics::plot(x = NA,
                 axes = FALSE,
                 xlab = "",
                 ylab = "",
                 xlim = xlim,
                 ylim = ylim,
                 ...)
}
