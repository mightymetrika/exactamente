#' Exact Bootstrap with Summary Statistics
#'
#' This function performs an exact bootstrap for small datasets of less than 10
#' observations. It then computes and returns a set of summary statistics: mode, mean,
#' standard deviation, and lower and upper confidence intervals.
#'
#' @param data A numeric vector of data points.
#' @param n_bootstraps An integer indicating the number of bootstrap samples to
#' generate. Default is 10000.
#' @param check_size A logical value indicating whether to check if the size of the
#' dataset is less than 10. Default is TRUE.
#' @param anon A function to compute the sample statistic for each bootstrap
#' sample. Default is the mean function.
#' @param lb Lower bound for the confidence interval. Default is 0.025.
#' @param ub Upper bound for the confidence interval. Default is 0.975.
#'
#' @return A list with two elements: dens (a density estimate of the bootstrap sample statistics)
#' and stats (a list of summary statistics including mode, mean, standard deviation,
#' lower confidence interval (lCI), and upper confidence interval (uCI)).
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- exact_bootstrap(data)
#' result$stats
exact_bootstrap <- function(data, n_bootstraps = 10000, check_size = TRUE,
                            anon = function(x)(mean(x)), lb = 0.025, ub = 0.975) {
  n <- length(data)
  if (check_size == TRUE & n > 9) stop("This function only works for datasets
                                        with less than 10 observations.")

  if(!(is.vector(data) & is.numeric(data))) stop("data should be a numeric vector.")

  # Generate all possible bootstrap samples.
  bootstrap_samples <- expand.grid(rep(list(data), n))

  # Compute the sample stat for each bootstrap sample.
  bootstrap_stats <- apply(bootstrap_samples, 1, anon)

  # Estimate the density of the bootstrap statistic
  density_estimate <- stats::density(bootstrap_stats, n = n_bootstraps)

  # Mode
  mode <- bootstrap_stats[which.max(bootstrap_stats)]

  # Mean
  mean <- mean(bootstrap_stats)

  # Standard deviation
  sd <- sd(bootstrap_stats)

  # Confidence Interval
  lCI <- stats::quantile(bootstrap_stats, lb)
  uCI <- stats::quantile(bootstrap_stats, ub)

  # Store results in list
  stats <- list(mode = mode,
                mean = mean,
                sd = sd,
                lCI = lCI,
                uCI = uCI)

  result <- list(dens = density_estimate, stats = stats)

  return(result)
}
