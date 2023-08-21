#' Compare Bootstrap Distributions
#'
#' This function generates a plot comparing the density estimates of the exact
#' and regular bootstrap distributions.
#'
#' @param exact_bootstrap_distribution An named list returned from
#' exact_bootstrap() with components dens$x (the coordinates of the points where
#' the density is estimated) and y (the estimated density values), representing
#' the density estimate of the exact bootstrap sample statistic.
#' @param regular_bootstrap_distribution An named list returned from
#' reg_bootstrap() with components dens$x (the coordinates of the points where
#' the density is estimated) and y (the estimated density values), representing
#' the density estimate of the regular bootstrap sample statistic.
#' @param title Plot title
#'
#' @return A ggplot object showing the density estimates of the exact and regular
#' bootstrap sample statistics, with different colors used to distinguish between
#' the two.
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' exact_bootstrap_result <- exact_bootstrap(data)
#' regular_bootstrap_result <- reg_bootstrap(data)
#' compare_bootstrap(exact_bootstrap_result,
#'                   regular_bootstrap_result)
compare_bootstrap <- function(exact_bootstrap_distribution,
                              regular_bootstrap_distribution,
                              title = "Comparison of Bootstrap Distributions") {

  if(methods::is(exact_bootstrap_distribution$dens)[[1]] != "density")
    stop("exact_bootstrap_distribution must be an object of class density.")

  if(methods::is(exact_bootstrap_distribution) != "extboot")
    stop("exact_bootstrap_distribution must be an object of class extboot")

  if(methods::is(regular_bootstrap_distribution$dens)[[1]] != "density")
    stop("regular_bootstrap_distribution must be an object of class density.")

  if(methods::is(regular_bootstrap_distribution) != "regboot")
    stop("regular_bootstrap_distribution must be an object of class regboot")

  exact_df <- data.frame(x = exact_bootstrap_distribution$dens$x,
                         y = exact_bootstrap_distribution$dens$y,
                         Method = "Exact Bootstrap")

  reg_df <- data.frame(x = regular_bootstrap_distribution$dens$x,
                       y = regular_bootstrap_distribution$dens$y,
                       Method = "Regular Bootstrap")

  full_df <- rbind(exact_df, reg_df)
  full_df$Method <- factor(full_df$Method, levels = c("Exact Bootstrap",
                                                      "Regular Bootstrap"))

  p <- ggplot2::ggplot(full_df, ggplot2::aes(x = x, y = y, color = Method)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Sample Stat", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("Exact Bootstrap" = "blue",
                                           "Regular Bootstrap" = "red")) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(plot.title = ggplot2::element_text(color = "#333333",
                                                      size = 22.5, face = "plain",
                                                      hjust = -0.08))

  return(p)
}
