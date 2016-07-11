#' @title Rank-based Inverse Normal Transform Function
#' @name RankInverseNormalTransform
#' @author Robert Corty
#'
#' @description This function transforms a vector of inputs using the rank-based
#' inverse normal transform, also known as the $z$ transform
#'
#' @param v the vector of values to be transformed
#' @param c a parameter of the transformation.
#' @param ... additional parameters passed to \code{qnorm}
#'
#' @return a vector of transformed values
#'
#' @details There is quite a bit of literature discussing what is the best
#' value of \code{c}, none of which the author of this function understands
#' in an detail.  The author of this function set the default to 3/8
#' because that seems to be the most popular amond statisticians.  Other
#' values that seem popular are 1/3, 1/2, and 0.  The author of this function
#' recommends "Rank-Based Inverse Normal Transformations are Increasingly Used,
#' But are They Merited?" by Beasley, Ericson, and Allison in Behavioral Genetics
#' in 2008 as a reference for learning more about this type of transform.
#'
#' @examples
#' RINT(runif(n = 100))
#' RINT(rnorm(n = 100))
#'
#' @export
#'
RINT <- function(v, c = 3/8, ...) {
  qnorm(p = (rank(v) - c)/(length(v) - 2*c + 1), ...)
}
