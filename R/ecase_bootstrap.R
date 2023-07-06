#' @title Performs an Exact Case Bootstrap with Summary Statistics
#' @description Performs an exact case bootstrap method on small datasets and returns
#' a set of summary statistics. For a given sample size N, it generates all
#' (2N - 1) choose N combinations with replacement. This is sometimes referred to
#' as exact case bootstrap resampling.
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
#' result <- ecase_bootstrap(data)
#' summary(result)
#' plot(result)
#' @seealso \code{\link{plot.ecboot}}, \code{\link{summary.ecboot}}
#' @references
#' Jekel, C. F., & Romero, V. J. (2020). Conservative Estimation of Tail Probabilities from Limited Sample Data. \doi{https://doi.org/10.2172/1605343}
#' @export
ecase_bootstrap <- function(data, check_size = TRUE, anon = function(x)(mean(x)),
                            lb = 0.025, ub = 0.975, density_args) {
  n <- length(data)
  if (check_size == TRUE & n > 9) stop("This function only works for datasets
                                        with less than 10 observations.")

  if(!(is.vector(data) & is.numeric(data))) stop("data should be a numeric vector.")

  # Generate all possible bootstrap samples.
  combinations <- expand.grid(replicate(n, data, simplify = FALSE))

  # Sort each row in ascending order
  combinations <- t(apply(combinations, 1, sort))

  # Remove duplicate rows
  bootstrap_samples <- unique(combinations)

  # Compute the sample stat for each bootstrap sample.
  bootstrap_stats <- apply(bootstrap_samples, 1, anon)

  # Process bootstrap stats
  result <- process_bootstrap_stats(bootstrap_stats = bootstrap_stats,
                                    density_args = density_args,
                                    lb = lb, ub = ub)

  # Assign "ecboot" class
  class(result) <- "ecboot"

  return(result)
}

#' Plot Method for 'ecboot' Class
#'
#' @description Creates a plot of the density estimates of the bootstrap sample statistics
#' returned from the \code{\link{ecase_bootstrap}} function.
#' @param x An object of class 'ecboot', usually the output of the \code{\link{ecase_bootstrap}} function.
#' @param title A plot title. Default is "Exact Case Bootstrap Distribution".
#' @param ... Additional parameters (currently ignored).
#' @return A ggplot object showing the density estimates of the bootstrap sample statistic.
#' @seealso \code{\link{ecase_bootstrap}}, \code{\link{summary.ecboot}}
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- ecase_bootstrap(data)
#' plot(result)
#' @export
plot.ecboot <- function(x, title = "Exact Case Bootstrap Distribution", ...) {
  if(methods::is(x) != "ecboot")
    stop("x must be an object of class ecboot")
  boot_plot(x, title = title)
}

#' Summary Method for 'ecboot' Class
#'
#' @description Creates a summary table of the summary statistics computed in the
#' \code{\link{ecase_bootstrap}} function.
#' @param object An object of class 'ecboot', usually the output of the \code{\link{ecase_bootstrap}} function.
#' @param ... Additional parameters (currently ignored).
#' @return A data.frame containing the summary statistics.
#' @seealso \code{\link{ecase_bootstrap}}, \code{\link{plot.ecboot}}
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' result <- ecase_bootstrap(data)
#' summary(result)
#' @export
summary.ecboot <- function(object, ...) {
  if(methods::is(object) != "ecboot")
    stop("object must be an object of class ecboot")
  summary_table <- xboot_summary(object)
  return(summary_table)
}

