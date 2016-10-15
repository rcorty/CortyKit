#' @title servr_patch
#' @description \code{servr_patch} goes through all the html files in \code{path}
#' and replaces all instances of 'src="figure' with 'src="/figure'.  This is
#' a very kludgey way to fix a small bug in \pkg{servr}.
#'
#' @param path the path in which to do the patching. Defaults to '_site'
#'
#' @return TRUE, if it completes successfully
#' @export
#'
servr_patch <- function(path = '_site/') {

  html_file_names <- list.files(path = path, pattern = '\\.html', full.names = TRUE, recursive = TRUE)

  for (html_file_name in html_file_names) {
    input_lines <- readLines(con = html_file_name)

    cat(gsub(pattern = 'src="figure',
             replacement = 'src="/figure',
             x = input_lines),
        file = html_file_name,
        sep = "\n")
  }

  return(TRUE)
}
