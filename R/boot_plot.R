#' Plot Bootstrap Distributions
#'
#' This function generates a plot comparing of the density estimates of a bootstrap
#' distribution.
#'
#' @param dens A list with components x (the coordinates
#' of the points where the density is estimated) and y (the estimated density
#' values), representing the density estimate of a bootstrap sample statistics.
#' @param title A plot title
#'
#' @return A ggplot object showing the density estimates of a bootstrap sample
#' statistic.
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' exact_bootstrap_result <- exact_bootstrap(data)
#' boot_plot(exact_bootstrap_result)
boot_plot <- function(dens, title = "Distribution") {
  if(methods::is(dens)[[1]] != "density")
    stop("dens must be an object of class density.")

  df <- data.frame(x = dens$x,
                   y = dens$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = "Sample Stat", y = "Density") +
    ggplot2::theme_minimal()

  return(p)
}
