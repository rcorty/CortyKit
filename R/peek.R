#' @title Take a Peek at Your Data
#'
#' This is a variant on [tibble::glimpse()], that gives better
#' summaries of factor and character-type columns.
#'
#' @section S3 methods:
#' `peek` is an S3 generic with a customised method for `tbl`s and
#' `data.frame`s, and a default method that calls [str()].
#'
#' @param x An object to peek at.
#' @param width Width of output: defaults to the setting of the option
#'   `tibble.width` (if finite) or the width of the console.
#' @param ... Other arguments passed on to individual methods.
#' @return x original x is (invisibly) returned, allowing `peek()` to be
#'   used within a data pipe line.
#' @export
#' @examples
#' peek(mtcars)
#'
#' if (!requireNamespace("nycflights13", quietly = TRUE))
#'   stop("Please install the nycflights13 package to run the rest of this example")
#'
#' peek(nycflights13::flights)
peek <- function(x, width = NULL, ...) {
  UseMethod('peek')
}

#' @export
#' @importFrom pillar new_pillar_title
#' @importFrom pillar new_pillar_type
peek.tbl <- function(x, width = NULL, ...) {
  width <- tibble_peek_width(width)
  if (!is.finite(width)) {
    abort(error_peek_infinite_width())
  }

  cat_line("Observations: ", big_mark(nrow(x)))

  # this is an overestimate, but shouldn't be too expensive.
  # every type needs at least three characters: "x, "
  rows <- as.integer(width / 3)
  df <- as.data.frame(head(x, rows))
  cat_line("Variables: ", big_mark(ncol(df)))
  if (ncol(df) == 0) return(invisible(x))

  var_types <- map_chr(map(df, new_pillar_type), format)
  ticked_names <- format(new_pillar_title(names(df)))
  var_names <- paste0("$ ", justify(ticked_names, right = FALSE), " ", var_types, " ")

  data_width <- width - crayon::col_nchar(var_names) - 2
  formatted <- map_chr(df, function(x) collapse(format_v(x)))
  truncated <- str_trunc(formatted, data_width)

  cat_line(var_names, truncated)
  invisible(x)
}

#' @export
peek.data.frame <- peek.tbl

#' @export
#' @importFrom utils str
peek.default <- function(x, width = NULL, max.level = 3, ...) {
  str(x, width = tibble_width(width), max.level = max.level, ...)
  invisible(x)
}

str_trunc <- function(x, max_width) {
  width <- nchar(x)

  for (i in seq_along(x)) {
    if (width[i] <= max_width[i]) next

    x[i] <- paste0(substr(x[i], 1, max_width[i] - 3), "...")
  }

  x
}

format_v <- function(x) UseMethod("format_v")

#' @export
format_v.default <- function(x) format(x, trim = TRUE, justify = "none")

#' @export
format_v.list <- function(x) {
  out <- map(x, format_v)
  atomic <- map_int(out, length) == 1L
  out <- map_chr(out, collapse)
  out[!atomic] <- paste0("<", out[!atomic], ">")
  paste0("[", collapse(out), "]")
}

#' @export
format_v.character <- function(x) encodeString(x, quote = '"')

#' @export
format_v.factor <- function(x) {
  if (any(grepl(",", x, fixed = TRUE))) {
    encodeString(as.character(x), quote = '"')
  } else {
    format(x, trim = TRUE, justify = "none")
  }
}


tibble_peek_width <- function(width) {
  if (!is_null(width)) {
    return(width)
  }

  width <- tibble_opt("width")
  if (!is_null(width) && is.finite(width)) {
    return(width)
  }

  getOption("width")
}

error_peek_infinite_width <- function() {
  "`peek()` requires a finite value for the `width` argument."
}

cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}
