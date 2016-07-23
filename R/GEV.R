#' @title QGEV
#'
#' @param p
#' @param gev
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
#' @param xs
#' @param gev
#' @param log
#'
#' @return
#' @export
#'
#' @examples
DGEV <- function(xs, gev, log = FALSE) {

  # validate input
  if ((!'estimate' %in% names(gev))) {
    stop("argument 'gev' must have an element named 'estimate' (all gev objects do)")
  }
  if (length(gev$estimate) != 3) {
    stop("gev$estimate must have three elements")
  }

  return(dgev(x = xs,
              loc = gev$estimate[1],
              scale = gev$estimate[2],
              shape = gev$estimate[3],
              log = log))

}


#' PGEV
#'
#' @param q
#' @param gev
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PGEV <- function(q, gev, ...) {

  # validate input
  if ((!'estimate' %in% names(gev))) {
    stop("argument 'gev' must have an element named 'estimate' (all gev objects do)")
  }
  if (length(gev$estimate) != 3) {
    stop("gev$estimate must have three elements")
  }

  return(pgev(q = q,
              loc = gev$estimate[1],
              scale = gev$estimate[2],
              shape = gev$estimate[3], ...))

}
