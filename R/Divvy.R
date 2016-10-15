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
Divvy <- function(first.idx,
                  last.idx,
                  which.group,
                  num.groups = ceiling(n.idxs/group.size),
                  group.size = ceiling(n.idxs/num.groups)) {

  idxs <- first.idx:last.idx
  n.idxs <- length(idxs)

  # avoid mayhem
  stopifnot(num.groups > 0)
  stopifnot(group.size > 0)
  # commented these out because these cases actually make sense some times
  # stopifnot(n.idxs >= num.groups)
  # stopifnot(n.idxs >= group.size)

  if (missing(which.group)) {
    return(data_frame(group.num = 1:num.groups) %>%
             mutate(group.start.idx = (group.num - 1)*group.size + 1,
                    group.stop.idx = ifelse(test = group.num == num.groups,
                                            yes = last.idx,
                                            no = group.num*group.size)))
  } else {
  stopifnot(which.group > 0)
  stopifnot(num.groups >= which.group)
    if (which.group == num.groups) {
      return(idxs[((which.group - 1)*group.size + 1):(n.idxs)])
    } else {
      return(idxs[((which.group - 1)*group.size + 1):(which.group*group.size)])
    }
  }
}
