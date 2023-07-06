#' @title Performs an Exact Bootstrap with Summary Statistics
#' @description Performs an exact bootstrap method on small datasets and returns
#' a set of summary statistics. This function implements a popular variant of exact
#' bootstrapping where the total number of combinations is expressed as N^N
#' (i.e., the vector c(1 ,2, 7) has 3^3 = 27 combinations rather than the 10
#' combinations obtained from exact case bootstrapping), giving more weight to a
#' resample with larger variation, thus potentially increasing the bootstrap
#' estimate's accuracy. This technique is described in Jekel and Romero (2020).
#' @param data A numeric vector of data points.
#' @param check_size A logical value indicating whether to check if the size of the
#' dataset is less than 10. Default is TRUE.
#' @param anon A function to compute the sample statistic for each bootstrap
#' sample. Default is the mean function.
#' @param lb Lower bound for the confidence interval. Default is 0.025.
#' @param ub Upper bound for the confidence interval. Default is 0.975.
#' @param density_args Pass additional arguments to stats::density
#' @return A list with two elements: dens (a density estimate of the bootstrap
#' sample statistics) and stats (a list of summary statistics including mode,
#' median, mean, standard deviation, lower confidence interval (lCI), and upper
#' confidence interval (uCI)).
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- exact_bootstrap(data)
#' summary(result)
#' plot(result)
#' @seealso \code{\link{plot.extboot}}, \code{\link{summary.extboot}}
#' @references
#' Jekel, C. F., & Romero, V. J. (2020). Conservative Estimation of Tail Probabilities from Limited Sample Data. \doi{https://doi.org/10.2172/1605343}
#' @export
exact_bootstrap <- function(data, check_size = TRUE, anon = function(x)(mean(x)),
                            lb = 0.025, ub = 0.975, density_args) {
  n <- length(data)
  if (check_size == TRUE & n > 9) stop("This function only works for datasets
                                        with less than 10 observations.")

  if(!(is.vector(data) & is.numeric(data))) stop("data should be a numeric vector.")

  # Generate all possible bootstrap samples.

bootstrap_samples <- expand.grid(rep(list(data), n))
  # Compute the sample stat for each bootstrap sample.
  bootstrap_stats <- apply(bootstrap_samples, 1, anon)

  # Estimate the density of the bootstrap statistic
  if (missing(density_args)){
    density_estimate <- stats::density(bootstrap_stats)
  } else {
    density_estimate <- do.call(stats::density, c(list(x = bootstrap_stats),
                                                  density_args))
  }

  # Mode
  mode <- density_estimate$x[which.max(density_estimate$y)]

  # Median
  median <- stats::median(bootstrap_stats)

  # Mean
  mean <- mean(bootstrap_stats)

  # Standard deviation
  sd <- sd(bootstrap_stats)

  # Confidence Interval
  lCI <- stats::quantile(bootstrap_stats, lb)
  uCI <- stats::quantile(bootstrap_stats, ub)

  # Store results in list
  stats <- list(mode = mode,
                median = median,
                mean = mean,
                sd = sd,
                lCI = lCI,
                uCI = uCI)

  result <- list(dens = density_estimate, stats = stats)

  # Assign "extboot" class
  class(result) <- "extboot"

  return(result)
}


#' Plot Method for 'extboot' Class
#'
#' @description Creates a plot of the density estimates of the bootstrap sample statistics
#' returned from the \code{\link{exact_bootstrap}} function.
#' @param x An object of class 'extboot', usually the output of the \code{\link{exact_bootstrap}} function.
#' @param title A plot title. Default is "Exact Bootstrap Distribution".
#' @param ... Additional parameters (currently ignored).
#' @return A ggplot object showing the density estimates of the bootstrap sample statistic.
#' @seealso \code{\link{exact_bootstrap}}, \code{\link{summary.extboot}}
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- exact_bootstrap(data)
#' plot(result)
#' @export
plot.extboot <- function(x, title = "Exact Bootstrap Distribution", ...) {
  if(methods::is(x) != "extboot")
    stop("x must be an object of class extboot")
  boot_plot(x, title = title)
}

#' Summary Method for 'extboot' Class
#'
#' @description Creates a summary table of the summary statistics computed in the
#' \code{\link{exact_bootstrap}} function.
#' @param object An object of class 'extboot', usually the output of the \code{\link{exact_bootstrap}} function.
#' @param ... Additional parameters (currently ignored).
#' @return A data.frame containing the summary statistics.
#' @seealso \code{\link{exact_bootstrap}}, \code{\link{plot.extboot}}
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- exact_bootstrap(data)
#' summary(result)
#' @export
summary.extboot <- function(object, ...) {
  if(methods::is(object) != "extboot")
    stop("object must be an object of class extboot")
  summary_table <- as.data.frame(object$stats)
  summary_table$Method <- "exact_bootstrap"
  summary_table <- summary_table[, c(7, 1:6)]
  return(summary_table)
}
