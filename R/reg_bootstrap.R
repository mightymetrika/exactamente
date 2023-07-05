#' Regular Bootstrap with Summary Statistics
#'
#' This function performs a regular bootstrap to generate a distribution of sample
#' statistics. It then computes and returns a set of summary statistics: mode, mean,
#' standard deviation, and lower and upper confidence intervals.
#'
#' @param data A numeric vector of data points.
#' @param n_bootstraps An integer indicating the number of bootstrap samples to
#' generate. Default is 10000.
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
#' result <- reg_bootstrap(data)
#' result$stats
reg_bootstrap <- function(data, n_bootstraps = 10000, anon = function(x)(mean(x)), lb = 0.025, ub = 0.975) {
  n <- length(data)

  if(!(is.vector(data) & is.numeric(data))) stop("data should be a numeric vector.")

  # Generate the bootstrap samples and compute the statistics.
  bootstrap_stats <- replicate(n_bootstraps, anon(sample(data, n, replace = TRUE)))

  # Estimate the density of the bootstrap statistics.
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

