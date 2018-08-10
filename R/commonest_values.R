#' @title commonest_values
#'
#' @param v the vector to assess
#' @param n the number of common values are returned
#'
#' @return the commonest n values in v, sorted
#' @export
#'
commonest_values <- function(v, n = 5) {
  head(
    x = sort(
      x = table(
        eval(expr = v),
        useNA = 'ifany'),
      decreasing = TRUE),
    n = n)
}
