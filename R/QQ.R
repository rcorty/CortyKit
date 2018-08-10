QQ_plot <- function(x, theoretical = 'unif', max_num_points = 1e3, sort = TRUE, add = FALSE, ...) {

  n <- length(x)

  if (sort) { x <- sort(x = x) }

  if (n > max_num_points) {

    idxs <- round(seq(from = 1, to = n, length.out = max_num_points))
    x <- x[idxs]
    n <- max_num_points

  }

  if (add) {
    points(x = (1:n - 0.5)/n,
           y = x,
           ...)
  } else {
    plot(x = (1:n - 0.5)/n,
         y = x,
         ...)

  }

}
