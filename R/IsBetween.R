#' @title IsBetween Function
#' @name IsBetween
#' @author Robert Corty
#'
#' @description This function returns \code{TRUE} if \code{x}
#' is between \code{lb} (lower bound) and \code{ub}
#' (upper bound).  This function can act on any type for which
#' \code{<, <=, >}, and \code{>=} are defined.  This function
#' works nicely with the pipe operator from the \pkg{dplyr} and
#' \pkg{magrittr} packages.
#'
#' @param x the focal object
#' @param lb lower bound
#' @param ub upper bound
#' @param equality.ok Indicates whether it acceptable for the focal
#' object to be equal to one of the arguments.  Defaults to
#' \code{FALSE}.
#'
#' @return \code{TRUE} if \code{x} is between \code{lb} and
#' \code{ub} and \code{FALSE} otherwise.
#'
#' @examples
#' IsBetween(x = 0, lb = 1, ub = 2)
#' IsBetween(x = 1, lb = 0, ub = 2)
#' IsBetween(x = 0, lb = 0, ub = 2)
#' IsBetween(x = 0, lb = 0, ub = 2, equality.ok = TRUE)
#'
#'
#' @export
#'
IsBetween <- function(x, lb, ub, equality.ok = FALSE) {
  if (equality.ok) {
    return(x >= lb & x <= ub)
  } else {
    return(x > lb & x < ub)
  }
}
