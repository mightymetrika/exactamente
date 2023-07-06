#' Process Bootstrap Statistics
#'
#' This function takes in bootstrap statistics and additional arguments to compute
#' the density estimate of the bootstrap statistic, the number of resamples,
#' the mode, median, mean, standard deviation, and confidence interval. It returns
#' a list containing the density estimate and summary statistics.
#'
#' @param bootstrap_stats A numeric vector of bootstrap statistics.
#' @param density_args A list of additional arguments to be passed to the density function. Default is NULL.
#' @param lb A numeric value specifying the lower bound for the confidence interval.
#' @param ub A numeric value specifying the upper bound for the confidence interval.
#'
#' @return A list with two elements: dens (a density estimate of the bootstrap
#' sample statistics) and stats (a list of summary statistics including number of
#' resamples, mode, median, mean, standard deviation, lower confidence interval
#' (lCI), and upper confidence interval (uCI)).
#'
#' @keywords internal
process_bootstrap_stats <- function(bootstrap_stats = bootstrap_stats,
                                    density_args = density_args,
                                    lb = lb, ub = ub){
  # Estimate the density of the bootstrap statistic
  if (missing(density_args)){
    density_estimate <- stats::density(bootstrap_stats)
  } else {
    density_estimate <- do.call(stats::density, c(list(x = bootstrap_stats),
                                                  density_args))
  }

  # Number of resample
  nres <- length(bootstrap_stats)

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
  stats <- list(nres = nres,
                mode = mode,
                median = median,
                mean = mean,
                sd = sd,
                lCI = lCI,
                uCI = uCI)

  result <- list(dens = density_estimate, stats = stats)
}

#' Generate a Summary Table for Bootstrap Objects
#'
#' This function takes a bootstrap object and generates a summary table
#' containing the method used and the computed summary statistics.
#'
#' @param object A bootstrap object of class 'extboot', 'ecboot' or 'regboot'.
#'
#' @return A data frame containing the summary statistics with the method as the
#' first column.
#'
#' @keywords internal
xboot_summary <- function(object){
  # Coerce to data.frame
  summary_table <- as.data.frame(object$stats)

  # Set Method variable
  if (methods::is(object) == "extboot"){
    summary_table$Method <- "exact_bootstrap"
  } else if (methods::is(object) == "ecboot"){
    summary_table$Method <- "ecase_bootstrap"
  } else {
    summary_table$Method <- "reg_bootstrap"
  }

  # Reorder columns
  summary_table <- summary_table[, c(8, 1:7)]

  # Return summary_table
  return(summary_table)
}


