#' Regular Bootstrap with Summary Statistics
#'
#' @description Performs a regular bootstrap to generate a distribution of sample
#' statistics and computes a set of summary statistics: mode, median, mean,
#' standard deviation, and lower and upper confidence intervals.
#' @param data A numeric vector of data points.
#' @param n_bootstraps An integer indicating the number of bootstrap samples to
#' generate. Default is 10000.
#' @param anon A function to compute the sample statistic for each bootstrap
#' sample. Default is the mean function.
#' @param lb Lower bound for the confidence interval. Default is 0.025.
#' @param ub Upper bound for the confidence interval. Default is 0.975.
#' @param density_args Additional arguments to be passed to \code{\link[stats]{density}} function.
#' @return A list with two elements: \itemize{
#'  \item dens: a density estimate of the bootstrap sample statistics,
#'  \item stats: a list of summary statistics including mode, median, mean,
#'  standard deviation, lower confidence interval (lCI), and upper confidence
#'  interval (uCI).}
#' @seealso \code{\link{plot.regboot}}, \code{\link{summary.regboot}}
#' @export
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- reg_bootstrap(data)
#' result$stats
reg_bootstrap <- function(data, n_bootstraps = 10000, anon = function(x)(mean(x)),
                          lb = 0.025, ub = 0.975, density_args) {
  n <- length(data)

  if(!(is.vector(data) & is.numeric(data))) stop("data should be a numeric vector.")

  # Generate the bootstrap samples and compute the statistics.
  bootstrap_stats <- replicate(n_bootstraps, anon(sample(data, n, replace = TRUE)))

  # Process bootstrap stats
  result <- process_bootstrap_stats(bootstrap_stats = bootstrap_stats,
                                    density_args = density_args,
                                    lb = lb, ub = ub)

  # Assign "regboot" class
  class(result) <- "regboot"

  return(result)
}

#' Plot Method for 'regboot' Class
#'
#' @description Creates a plot of the density estimates of the bootstrap sample statistics
#' returned from the \code{\link{reg_bootstrap}} function.
#' @param x An object of class 'regboot', usually the output of the \code{\link{reg_bootstrap}} function.
#' @param title A plot title. Default is "Regular Bootstrap Distribution".
#' @param ... Additional parameters (currently ignored).
#' @return A ggplot object showing the density estimates of the bootstrap sample statistic.
#' @seealso \code{\link{reg_bootstrap}}, \code{\link{summary.regboot}}
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- reg_bootstrap(data)
#' plot(result)
#' @export
plot.regboot <- function(x, title = "Regular Bootstrap Distribution", ...) {
  if(methods::is(x) != "regboot")
    stop("object must be an object of class regboot")
  boot_plot(x, title = title)
}

#' Summary Method for 'regboot' Class
#'
#' @description Creates a summary table of the summary statistics computed in the
#' \code{\link{reg_bootstrap}} function.
#' @param object An object of class 'regboot', usually the output of the \code{\link{reg_bootstrap}} function.
#' @param ... Additional parameters (currently ignored).
#' @return A data.frame containing the summary statistics.
#' @seealso \code{\link{reg_bootstrap}}, \code{\link{plot.regboot}}
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- reg_bootstrap(data)
#' summary(result)
#' @export
summary.regboot <- function(object, ...) {
  if(methods::is(object) != "regboot")
    stop("object must be an object of class regboot")
  summary_table <- xboot_summary(object)
  return(summary_table)
}
