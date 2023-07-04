#' Compare Bootstrap Distributions
#'
#' This function generates a plot comparing the density estimates of the exact
#' and regular bootstrap distributions.
#'
#' @param exact_bootstrap_distribution A list with components x (the coordinates
#' of the points where the density is estimated) and y (the estimated density
#' values), representing the density estimate of the exact bootstrap sample statistics.
#' @param regular_bootstrap_distribution A list with components x (the coordinates
#' of the points where the density is estimated) and y (the estimated density
#' values), representing the density estimate of the regular bootstrap sample statistics.
#'
#' @return A ggplot object showing the density estimates of the exact and regular
#' bootstrap sample statistics, with different colors used to distinguish between the two.
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' exact_bootstrap_result <- exact_bootstrap(data)
#' regular_bootstrap_result <- reg_bootstrap(data)
#' compare_bootstrap(exact_bootstrap_result, regular_bootstrap_result)
compare_bootstrap <- function(exact_bootstrap_distribution,
                              regular_bootstrap_distribution) {
  if(methods::is(exact_bootstrap_distribution)[[1]] != "density")
    stop("exact_bootstrap_distribution must be an object of class density.")

  if(methods::is(regular_bootstrap_distribution)[[1]] != "density")
    stop("regular_bootstrap_distribution must be an object of class density.")

  exact_df <- data.frame(x = exact_bootstrap_distribution$x,
                         y = exact_bootstrap_distribution$y,
                         Method = "Exact Bootstrap")

  reg_df <- data.frame(x = regular_bootstrap_distribution$x,
                       y = regular_bootstrap_distribution$y,
                       Method = "Regular Bootstrap")

  full_df <- rbind(exact_df, reg_df)

  p <- ggplot2::ggplot(full_df, ggplot2::aes(x = x, y = y, color = Method)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Comparison of Bootstrap Distributions", x = "Sample Stat", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("Exact Bootstrap" = "blue", "Regular Bootstrap" = "red"))

  return(p)
}
