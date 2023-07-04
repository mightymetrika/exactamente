bootsummer <- function(dens, lb = 0.025, ub = 0.975){
  # Mode
  mode <- dens$x[which.max(dens$y)]

  # Mean
  mean <- sum(dens$x * dens$y) / sum(dens$y)

  # Standard deviation
  sd <- sqrt(sum(dens$y * (dens$x - mean)^2) / sum(dens$y))

  # Sort density values and their corresponding x values
  sorted_indices <- order(dens$y)
  sorted_y <- dens$y[sorted_indices]
  sorted_x <- dens$x[sorted_indices]

  # Calculate cumulative sum of sorted density values
  cum_y <- cumsum(sorted_y)

  # Find the indices that make up the lower and upper bounds of the 95% highest density interval
  HDI_indices <- which(cum_y >= lb & cum_y <= ub)

  # Calculate the 95% highest density interval
  HDI <- c(min(sorted_x[HDI_indices]), max(sorted_x[HDI_indices]))

  # Store results in list
  summer <- list(mode = mode,
                 mean = mean,
                 sd = sd,
                 HDI = HDI)

  # Return summary
  return(summer)
}

