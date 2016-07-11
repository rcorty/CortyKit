#'  @title AllIdentical Function
#'
#'  @description Returns TRUE iff all arguments are identical.
#'
#'  @param A list of objects for which 'identical' is defined.
#'
#'  @author Robert Corty
#'
#'  @usage l <- list(a, b, c)
#'         AllIdentical(l))
#'
#'  @export
#'
AllIdentical <- function(l) {

	pairs <- combn(x = l, m = 2, simplify = FALSE)

	for(p in pairs) {
		if (!identical(p[[1]], p[[2]])) {
			return(FALSE)
		}
	}

	return(TRUE)
}
