#' Exact Bootstrap
#'
#' This function performs an exact bootstrap for small datasets of less than 10
#' observations.
#'
#' @param data A numeric vector of data points.
#' @param n_bootstraps An integer indicating the number of bootstrap samples to
#' generate. Default is 10000.
#' @param check_size A logical value indicating whether to check if the size of
#' the dataset is less than 10. Default is TRUE.
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
#' exact_bootstrap(data)
exact_bootstrap <- function(data, n_bootstraps = 10000, check_size = TRUE,
                            anon = function(x)(mean(x))) {
  n <- length(data)
  if (check_size == TRUE & n > 9) stop("This function only works for datasets
                                        with less than 10 observations.")

  # Generate all possible bootstrap samples.
  bootstrap_samples <- expand.grid(rep(list(data), n))

  # Compute the sample stat for each bootstrap sample.
  bootstrap_stats <- apply(bootstrap_samples, 1, anon)

  # Estimate the density of the bootstrap means.
  density_estimate <- stats::density(bootstrap_stats, n = n_bootstraps)

  return(density_estimate)
}
