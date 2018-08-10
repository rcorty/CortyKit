#' permute a vector cyclically
#'
#' @param v the vector to permute
#' @param n the number of steps to cycle the vector
#'
#' @return the permuted vector
#' @export
#'
#' @examples
#' cycle_n(v = 1:5)
#' cycle_n(v = 1:5, n = 3)
#' cycle_n(v = 1:5, n = 8)
permute_cyclic <- function(v, n = 1) {
  l <- length(v)
  if (n == l) {
    return(v)
  }
  if (n > l) {
    return(c(tail(x = v, n = n %% l), head(x = v, n = l - n %% l)))
  }
  if (n < l) {
    return(c(tail(x = v, n = n), head(x = v, n = l - n)))
  }
}
