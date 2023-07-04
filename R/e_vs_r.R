e_vs_r <- function(data, n_bootstraps = 10000, check_size = TRUE,
                   anon = function(x)(mean(x)), lb = 0.025, ub = 0.975) {

  # Run both bootstrap functions
  exact_result <- exact_bootstrap(data, n_bootstraps, check_size, anon)
  reg_result <- reg_bootstrap(data, n_bootstraps, anon)

  # Summarize results
  exact_summary <- bootsummer(exact_result, lb, ub)
  reg_summary <- bootsummer(reg_result, lb, ub)

  # Create summary table
  summary_table <- rbind(exact_summary, reg_summary)
  rownames(summary_table) <- c("Exact_bootstrap", "Regular_bootstrap")

  # Plot comparison
  comp_plot <- compare_bootstrap(exact_result, reg_result)

  # Store output in a list
  out <- list(summary_table = summary_table,
              comp_plot = comp_plot)

  # Return output
  return(out)
}
