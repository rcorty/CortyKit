#' @title forfor
#'
#' @param l named list of variables to loop over and their values to loop over
#' @param e expression to evaluate for every combination of values in l
#'
#' @details so  forfor(l = list(a = c(1,2), b = c(10, 20)), e = a + b)
#           evaluates to c(11, 21, 21, 22)
#           as if we had typed for(a in c(1,2)) { for(b in c(10,20)) { a + b }}
#' @return
#' @export
#'

forfor <- function(l, e) {

  g <- expand.grid(l)

  r <- list()
  for (i in 1:nrow(g)) {

    n <- paste0(colnames(g), '=', unlist(g[i,]), collapse = '_')
    r[[n]] <- eval(substitute(e), g[i,])
  }

  return(r)
}
