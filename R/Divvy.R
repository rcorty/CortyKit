#' @title Divvy Function
#' @name Divvy
#' @author Robert Corty
#'
#' @description This function splits the integers from 1 to \code{n} into
#' \code{g} groups and returns the \code{i}th group.  All of this is
#' straightforward, but it's important to do it in a consistent way
#' for splitting up data/analyses.
#'
#' @param n number of items to divvy up
#' @param num.groups number of groups to divvy the items into, defaults to
#' \code{ceiling(n/group.size)}
#' @param group.size size of each group, defaults to
#' \code{ceiling(n/num.groups)}
#' @param i group of items to return
#'
#' @return the indices of the \code{i}th group of size \code{g} in \code{1:n}
#'
#' @details The \code{g}th group may be smaller than the other groups
#'
#' @examples
#'
#'
#' @export
#'
Divvy <- function(n,
                  num.groups = ceiling(n/group.size),
                  group.size = ceiling(n/num.groups),
                  i) {

  # avoid lunacy
  stopifnot(i > 0)
  stopifnot(num.groups > 0)
  stopifnot(group.size > 0)
  stopifnot(n > num.groups)
  stopifnot(n > group.size)
  stopifnot(num.groups >= i)

  if (i == num.groups) {
    return(((i - 1)*group.size + 1):(n))
  } else {
    return(((i - 1)*group.size + 1):(i*group.size))
  }
}
