#' @title Plot Bootstrap Distributions
#' @description Generates a plot of the density estimates of a bootstrap
#' distribution.
#' @param dens An object of class `exboot` or `regboot`
#' @param title A plot title
#' @return A ggplot object showing the density estimates of a bootstrap sample
#' statistic.
#' @examples
#' \dontrun{
#' #set.seed(123)
#' #data <- rnorm(5)
#' #result <- exact_bootstrap(data)
#' #boot_plot(result)
#' }
#' @seealso \code{\link{exact_bootstrap}}
boot_plot <- function(dens, title = "Distribution") {
  if(methods::is(dens$dens)[[1]] != "density")
    stop("dens$dens must be an object of class density.")

  if(!(methods::is(dens) == "extboot" || methods::is(dens) == "regboot"))
    stop("dens must be an object of class density.")

  df <- data.frame(x = dens$dens$x,
                   y = dens$dens$y)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, x = "Sample Stat", y = "Density") +
    ggplot2::theme_minimal()

  return(p)
}
