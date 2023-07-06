#' Compare Exact Case Bootstrap vs Exact Bootstrap vs Regular Bootstrap
#'
#' This function runs the exact case, exact, and regular bootstrap functions on
#' a dataset, summarizes the results, and provides a comparative plot. It provides
#' a convenient way to compare these two methods of bootstrapping.
#'
#' @param data A numeric vector of data values to be bootstrapped.
#' @param n_bootstraps The number of bootstrap samples to generate. Defaults to 10000.
#' @param check_size Logical indicating if a check should be performed to ensure
#' the dataset has less than 10 observations for the exact bootstrap. Defaults to TRUE.
#' @param anon An anonymous function to compute the statistic of interest on
#' each bootstrap sample. Defaults to mean.
#' @param lb Lower bound for the confidence interval. Defaults to 0.025.
#' @param ub Upper bound for the confidence interval. Defaults to 0.975.
#' @param density_args Pass additional arguments to stats::density
#'
#' @return A list containing two items:
#' - summary_table: A summary table containing the mode, median, mean,
#' standard deviation, and confidence interval for each bootstrap method.
#' - comp_plot: A ggplot object comparing the bootstrap distributions.
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(5)
#' results <- e_vs_r(data)
#' print(results$summary_table)
#' print(results$comp_plot)
e_vs_r <- function(data, n_bootstraps = 10000, check_size = TRUE,
                   anon = function(x)(mean(x)), lb = 0.025, ub = 0.975, density_args) {

  # Run bootstrap functions
  ecase_result <- ecase_bootstrap(data, check_size, anon, lb = lb,
                                  ub = ub, density_args)
  exact_result <- exact_bootstrap(data, check_size, anon, lb = lb,
                                  ub = ub, density_args)
  reg_result <- reg_bootstrap(data, n_bootstraps, anon, lb = lb, ub = ub, density_args)

  # Create summary table
  summary_table <- rbind(as.data.frame(ecase_result$stats),
                         as.data.frame(exact_result$stats),
                         as.data.frame(reg_result$stats))
  summary_table$Method <- c("ecase_bootstrap", "exact_bootstrap", "reg_bootstrap")
  summary_table$Method <- factor(summary_table$Method,
                                 levels = c("ecase_bootstrap",
                                            "exact_bootstrap",
                                            "reg_bootstrap"))
  summary_table <- summary_table[, c(7, 1:6)]

  # Plot comparison
  comp_plot <- compare_bootstrap(ecase_result, exact_result, reg_result)

  # Store output in a list
  out <- list(summary_table = summary_table,
              comp_plot = comp_plot)

  # Return output
  return(out)
}
