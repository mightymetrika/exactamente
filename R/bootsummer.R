#' Bootstrap Summary Statistics
#'
#' This function computes the mode, mean, standard deviation, and highest density
#' interval of the density estimate of a bootstrap sample.
#'
#' @param dens A list with components x (the coordinates of the points where the
#' density is estimated)
#' and y (the estimated density values), which gives the density estimate of the
#' bootstrap sample statistics.
#' @param lb Lower bound for the highest density interval. Default is 0.025.
#' @param ub Upper bound for the highest density interval. Default is 0.975.
#'
#' @return A list with components mode (the mode of the density estimate),
#' mean (the mean of the density estimate), sd (the standard deviation of the
#' density estimate), and HDI (the highest density interval of the density estimate).
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' boot_result <- reg_bootstrap(data)
#' bootsummer(boot_result)
bootsummer <- function(dens, lb = 0.025, ub = 0.975){

  if(methods::is(dens)[[1]] != "density") stop("dens must be an object of class density.")

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
  lHDI <- min(sorted_x[HDI_indices])
  uHDI <- max(sorted_x[HDI_indices])

  # Store results in list
  summer <- list(mode = mode,
                 mean = mean,
                 sd = sd,
                 lHDI = lHDI,
                 uHDI = uHDI)

  # Return summary
  return(summer)
}
# bootsummer <- function(dens, lb = 0.025, ub = 0.975){
#
#   if(methods::is(dens)[[1]] != "density")stop("dens must be an object of class density.")
#
#   # Mode
#   mode <- dens$x[which.max(dens$y)]
#
#   # Mean
#   mean <- sum(dens$x * dens$y) / sum(dens$y)
#
#   # Standard deviation
#   sd <- sqrt(sum(dens$y * (dens$x - mean)^2) / sum(dens$y))
#
#   # Sort density values and their corresponding x values
#   sorted_indices <- order(dens$y)
#   sorted_y <- dens$y[sorted_indices]
#   sorted_x <- dens$x[sorted_indices]
#
#   # Calculate cumulative sum of sorted density values
#   cum_y <- cumsum(sorted_y)
#
#   # Find the indices that make up the lower and upper bounds of the 95% highest density interval
#   HDI_indices <- which(cum_y >= lb & cum_y <= ub)
#
#   # Calculate the 95% highest density interval
#   HDI <- c(min(sorted_x[HDI_indices]), max(sorted_x[HDI_indices]))
#
#   # Store results in list
#   summer <- list(mode = mode,
#                  mean = mean,
#                  sd = sd,
#                  HDI = HDI)
#
#   # Return summary
#   return(summer)
# }

