#' @title t
#' @description table of table of v
#'
#' @param v the vector whose table of table we want
#'
#' @return the table of table of v
#' @export
#'
tt <- function(v) {
  table(table(v))
}
