#' Regular Bootstrap
#'
#' This function performs a regular bootstrap to generate a distribution of sample
#' statistics.
#'
#' @param data A numeric vector of data points.
#' @param n_bootstraps An integer indicating the number of bootstrap samples to
#' generate. Default is 10000.
#' @param anon A function to compute the sample statistic for each bootstrap
#' sample. Default is the mean function.
#'
#' @return A list with components x (the n coordinates of the points where the
#' density is estimated) and y (the estimated density values) which gives the
#' density estimate of the bootstrap sample statistics.
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' reg_bootstrap(data)
reg_bootstrap <- function(data, n_bootstraps = 10000, anon = function(x)(mean(x))) {
  n <- length(data)

  # Generate the bootstrap samples and compute the statistics.
  bootstrap_stats <- replicate(n_bootstraps, anon(sample(data, n, replace = TRUE)))

  # Estimate the density of the bootstrap statistics.
  density_estimate <- stats::density(bootstrap_stats, n = n_bootstraps)

  return(density_estimate)
}
