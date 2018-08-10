#' #' @title GGglimpse
#' #' @description produce a plot to get a glimpse at a data.frame
#' #'
#' #' @param df
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #'
#' GGglimpse <- function(df) {
#'
#'   for (col.name in names(df)) {
#'
#'     p <- ggplot(data = df, mapping = aes_string(x = col.name)) +
#'       geom_density()
#'
#'     print(p)
#'   }
#'
#'   return(3)
#' }
