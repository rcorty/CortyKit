#' @title saveRWC
#'
#' @param overwrite should the function overwrite an existing file?
#' @param postfix_datetime should the function postfix the save file name with the ISO-formatted datetime?
#' @param ...
#'
#' @return TRUE is successful and FALSE otherwise
#' @export
#'
saveRWC <- function(object, file_name, overwrite = FALSE, postfix_datetime = FALSE, ...) {

  if (postfix_datetime) {
    has_suffix <- grepl(pattern = '.', x = file_name, fixed = TRUE)
    if (has_suffix) {
      components <- strsplit(x = file_name, split = '.', fixed = TRUE)[[1]]
      num_components <- length(components)
      file_name <- paste0(components[-num_components], collapse = '.')
      file_suffix <- paste0('.', components[num_components])
    } else {
      file_suffix <- ''
    }
    file_name <- paste0(file_name, '_', format(x = Sys.time(), format = "%Y-%m-%dT%H:%M:%S"), file_suffix)
  }


}

