#' @title AllIdentical Function
#' @name AllIdentical
#' @author Robert Corty
#'
#' @description This function evaluates whether all elements in a
#' 		\code{\link{list}} are identical according to the
#' 		\code{\link{identical}} function.
#'
#' @details By default, this function checks whether the first
#' 		element is \code{\link{identical}} to all others on the assumption
#' 		of transivity ((n - 1) calls to \code{\link{identical}}).
#' 		If the user changes
#' 		the \code{check.all.pairs} option from \code{FALSE}, its default, to
#' 		\code{TRUE}, the function checks all pairs ((n * (n - 1))/2 calls to
#' 		\code{\link{identical}}).
#' 		I don't know of any cases where \code{\link{identical}} isn't transitive,
#' 		so I don't know of any cases where this option would be necessary.
#'
#' @param l the list of \R objects to test
#' @param check.all.pairs logical indicating whether all pairwise tests
#' 		should be done
#' @param ... additional arguments to \code{\link{identical}}
#'
#' @return \code{TRUE} if all elements of \code{l} are identical and
#' 		\code{FALSE} otherwise.
#'
#' @examples
#' AllIdentical(list('a', 'b', 'c'))
#' AllIdentical(list('a', 'a', 'a'))
#'
#' @export
#'
AllIdentical <- function(l, check.all.pairs = FALSE, ...) {

	if (!is.list(l)) {
		stop('AllIdentical argument l must be a list')
	}
	if (length(l) == 0) {
		message('AllIdentical argument l is a list of length zero.  Suspicious.')
		return(TRUE)
	}
	if (length(l) == 1) {
		message('AllIdentical() argument l is a list of length one.  Suspicious')
		return(TRUE)
	}

	if (!check.all.pairs) {

		e1 <- l[[1]]
		for (e2 in 2:length(l)) {
			if (!identical(e1, e2)) {
				return(FALSE)
			}
		}
		return(TRUE)
	}

	if (check.all.pairs) {

		# this two-step formulation is preferable over the more 'elegant':
		# combn(x = list('a', 'b', 'b'),
		# 			m = 2,
		# 			simplify = FALSE,
		# 			FUN = function(l) { identical(l[[1]], l[[2]]) } )
		# because it allows for termination as soon as the first FALSE is reached

		pairs <- utils::combn(x = l, m = 2, simplify = FALSE)

		for(p in pairs) {
			if (!identical(p[[1]], p[[2]])) {
				return(FALSE)
			}
		}
		return(TRUE)
	}
}
