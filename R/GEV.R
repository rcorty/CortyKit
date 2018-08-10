#' @title QGEV
#'
#' @param p the probability
#' @param gev the fitted gev
#' @param ...
#'
#' @return the quantile of p
#' @export
#'
QGEV <- function(p, gev, ...) {

  # validate input
  if ((!'estimate' %in% names(gev))) {
    stop("argument 'gev' must have an element named 'estimate' (all gev objects do)")
  }
  if (length(gev$estimate) != 3) {
    stop("gev$estimate must have three elements")
  }

  return(qgev(p = p,
              loc = gev$estimate[1],
              scale = gev$estimate[2],
              shape = gev$estimate[3], ...))

}


#' DGEV
#'
#' @param xs the xs
#' @param gev the fitted gev
#' @param log whether to return log likelihood
#'
#' @return the density at xs
#' @export
#'
DGEV <- function(xs, gev, log = FALSE) {

  # validate input
  if ((!'estimate' %in% names(gev))) {
    stop("argument 'gev' must have an element named 'estimate' (all gev objects do)")
  }
  if (length(gev$estimate) != 3) {
    stop("gev$estimate must have three elements")
  }

  return(evd::dgev(x = xs,
                   loc = gev$estimate[1],
                   scale = gev$estimate[2],
                   shape = gev$estimate[3],
                   log = log))

}


#' PGEV
#'
#' @param q the quantile
#' @param gev the fitted gev
#' @param ...
#'
#' @return the CDF evaluated at q
#' @export
#'
PGEV <- function(q, gev, ...) {

  # validate input
  if ((!'estimate' %in% names(gev))) {
    stop("argument 'gev' must have an element named 'estimate' (all gev objects do)")
  }
  if (length(gev$estimate) != 3) {
    stop("gev$estimate must have three elements")
  }

  return(evd::pgev(q = q,
                   loc = gev$estimate[1],
                   scale = gev$estimate[2],
                   shape = gev$estimate[3], ...))

}
