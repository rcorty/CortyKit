#' @title ArticlePrecedingNumber Function
#' @name ArticlePrecedingNumber
#' @author Robert Corty
#'
#' @description This function returns the appropriate english definite
#' or indefinite article to precede a number.  The possible return
#' values are 'a', 'an', and 'the'
#'
#' @param n the number
#' @param definite whether the definite (or indefinite) article is
#' required.  Defaults to \code{FALSE}.
#'
#' @return a character object from \code{c('a', 'an', 'the')}
#' appropriate to precede the number
#'
#' @examples
#' ArticlePrecedingNumber(-8)
#' ArticlePrecedingNumber(-2)
#' ArticlePrecedingNumber(2)
#' ArticlePrecedingNumber(8)
#' ArticlePrecedingNumber(8, definite = TRUE)
#'
#' @export
#'
ArticlePrecedingNumber <- function(n, definite = FALSE) {

  # When the definite article is sought, it's always 'the', as in
  # 'the one', 'the two', 'the eight'
  if (definite) {
    return('the')
  }

  # Negative numbers always get 'a' as in
  # 'a negative fifteen'
  if (n < 1) {
    return('a')
  }

  # Positive number that begin with 8 get 'an', all others get 'a'
  first.digit <- substr(x = as.character(n), start = 1, stop = 1)
  if (first.digit == '8') {
    return('an')
  } else {
    return('a')
  }
}
