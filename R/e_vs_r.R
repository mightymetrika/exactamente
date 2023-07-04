#' Compare Exact Bootstrap vs Regular Bootstrap
#'
#' This function runs both the exact and regular bootstrap functions on a dataset,
#' summarizes the results, and provides a comparative plot. It provides a
#' convenient way to compare these two methods of bootstrapping.
#'
#' @param data A numeric vector of data values to be bootstrapped.
#' @param n_bootstraps The number of bootstrap samples to generate. Defaults to 10000.
#' @param check_size Logical indicating if a check should be performed to ensure
#' the dataset has less than 10 observations for the exact bootstrap. Defaults to TRUE.
#' @param anon An anonymous function to compute the statistic of interest on
#' each bootstrap sample. Defaults to mean.
#' @param lb Lower bound for the highest density interval. Defaults to 0.025.
#' @param ub Upper bound for the highest density interval. Defaults to 0.975.
#'
#' @return A list containing two items:
#' - summary_table: A summary table containing the mode, mean, standard deviation,
#' and highest density interval for each bootstrap method.
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
anon = function(x)(mean(x)), lb = 0.025, ub = 0.975) {

  # Run both bootstrap functions
  exact_result <- exact_bootstrap(data, n_bootstraps, check_size, anon)
  reg_result <- reg_bootstrap(data, n_bootstraps, anon)

  # Summarize results
  exact_summary <- bootsummer(exact_result, lb = lb, ub = ub)
  reg_summary <- bootsummer(reg_result, lb = lb, ub = ub)

  # Create summary table
  summary_table <- as.data.frame(rbind(exact_summary, reg_summary))
  summary_table$Method <- c("Exact_bootstrap", "Regular_bootstrap")
  summary_table <- summary_table[, c(6, 1:2, 3, 4, 5)]

  # Plot comparison
  comp_plot <- compare_bootstrap(exact_result, reg_result)

  # Store output in a list
  out <- list(summary_table = summary_table,
              comp_plot = comp_plot)

  # Return output
  return(out)
}
# e_vs_r <- function(data, n_bootstraps = 10000, check_size = TRUE,
#                    anon = function(x)(mean(x)), lb = 0.025, ub = 0.975) {
#
#   # Run both bootstrap functions
#   exact_result <- exact_bootstrap(data, n_bootstraps, check_size, anon)
#   reg_result <- reg_bootstrap(data, n_bootstraps, anon)
#
#   # Summarize results
#   exact_summary <- bootsummer(exact_result, lb = lb, ub = ub)
#   reg_summary <- bootsummer(reg_result, lb = lb, ub = ub)
#
#   # Create summary table
#   summary_table <- as.data.frame(rbind(exact_summary, reg_summary))
#   summary_table$Method <- c("Exact_bootstrap", "Regular_bootstrap")
#   summary_table <- summary_table[, c(5, 1:4)]
#
#   # Plot comparison
#   comp_plot <- compare_bootstrap(exact_result, reg_result)
#
#   # Store output in a list
#   out <- list(summary_table = summary_table,
#               comp_plot = comp_plot)
#
#   # Return output
#   return(out)
# }
