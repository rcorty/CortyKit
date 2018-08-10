#' @title glance
#'
#' @param df the data_frame
#'
#' @return a grob to plot
#' @export
#'
glance <- function(df, group_by = NULL) {

  grobs <- list()
  for (col_name in names(df)) {

    if (is.factor(df[[col_name]])) {
      grobs[[col_name]] <-
        ggplot2::ggplot(data = df, mapping = ggplot2::aes_string(x = col_name, fill = group_by)) +
        ggplot2::geom_bar()
    } else {
      grobs[[col_name]] <-
        ggplot2::ggplot(data = df, mapping = ggplot2::aes_string(x = col_name, fill = group_by)) +
        ggplot2::geom_density(alpha = ifelse(test = is.null(group_by), yes = 1, no = 0.4))
    }

  }

  return(gridExtra::arrangeGrob(grobs = grobs))
}
