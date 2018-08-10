#' DF2String
#'
#' @param DF data.frame to be turned into string
#' @param signif number of significant digits to keep of numeric columns
#'
#' @return a string
#' @export
#'
#' @examples
#' DF2String(iris)
DF2String <- function(DF, signif) {

  length_of_longest_row_namern <- max(sapply(X = row.names(DF), FUN = length))
  result <- dplyr::data_frame(rn = stringr::str_pad(string = row.names(DF),
                                                    width = length_of_longest_row_namern))

  loc <- function(x) {
    nchar(as.character(x))
  }

  for (col.name in colnames(DF)) {

    in.col <- DF[[col.name]]
    if (is.numeric(in.col)) {
      in.col <- signif(x = in.col, digits = signif)
    }

    longest.length <- max(sapply(X = in.col,
                                 FUN = loc))

    if (is.numeric(in.col)) {
      result[[col.name]] <- stringr::str_pad(string = in.col,
                                             width = longest.length,
                                             side = 'left',
                                             pad = '0')
    } else {
      result[[col.name]] <- stringr::str_pad(string = in.col,
                                             width = longest.length)
    }

  }

  return(result)
}
