#' #' @title explore_grid
#' #'
#' #' @param values a named list of variables and values that define the grid to explore, with the first value of each variable being it's default/base/fundamental value.
#' #' @param cross list of variables to combine all-by-all, as expand.grid does
#' #' @param in_parallel list of variables to combine in parallel
#' #' @param anti_parallel list of variables to combine in anti-parallel
#' #' @param foray list of variables to explore only at the default level of all other variables
#' #' @param constant
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #'
#' #'
#' #'
#' #'
#' #'
#' explore_grid <- function(values,
#'                          cross = NULL,
#'                          in_parallel = NULL,
#'                          anti_parallel = NULL,
#'                          foray = NULL,
#'                          constant = NULL) {
#'
#'   # check input
#'   stopifnot(is.list(values))
#'   if (!is.null(cross)) {
#'     stopifnot(all(cross %in% names(values)))
#'   }
#'   if (!is.null(in_parallel)) {
#'     stopifnot(all(in_parallel %in% names(in_parallel)))
#'   }
#'   if (!is.null(anti_parallel)) {
#'     stopifnot(all(anti_parallel %in% names(anti_parallel)))
#'   }
#'   if (!is.null(foray)) {
#'     stopifnot(all(foray %in% names(foray)))
#'   }
#'   if (!is.null(constant)) {
#'     stopifnot(all(constant %in% names(constant)))
#'   }
#'
#'   #... more todo here
#'
#'   if (!is.null(foray)) {
#'
#'   }
#'
#'
#'   if (!is.null(constant)) {
#'     for (constant_name in names(constant)) {
#'       result[[constant_name]] <- values[[constant_name]][1]
#'     }
#'   }
#'
#'   return(result)
#' }
