#' @title into
#'
#' @param value the value to store
#' @param name the expression to store it as
#'
#' @details from Konrad Rudolph in a comment on the win-vector blog
#'
#' @return nothing, just assigns
#' @export
#'

into <- function(value, name) {
  assign_expr <- substitute(expr = base::`<-`(name, value),
                            env = list(name = substitute(name),
                                       value = value))
  eval(expr = assign_expr,
       envir = parent.env(parent.frame()))
}
