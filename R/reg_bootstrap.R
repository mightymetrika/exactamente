reg_bootstrap <- function(data, n_bootstraps = 10000, anon = function(x)(mean(x))) {
  n <- length(data)

  # Generate the bootstrap samples and compute the statistics.
  bootstrap_stats <- replicate(n_bootstraps, anon(sample(data, n, replace = TRUE)))

  # Estimate the density of the bootstrap statistics.
  density_estimate <- stats::density(bootstrap_stats, n = n_bootstraps)

  return(density_estimate)
}
