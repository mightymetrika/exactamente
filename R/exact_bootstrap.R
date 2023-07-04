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
