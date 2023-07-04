compare_bootstrap <- function(exact_bootstrap_distribution,
                              regular_bootstrap_distribution) {
  exact_df <- data.frame(x = exact_bootstrap_distribution$x,
                         y = exact_bootstrap_distribution$y,
                         Method = "Exact Bootstrap")

  reg_df <- data.frame(x = regular_bootstrap_distribution$x,
                       y = regular_bootstrap_distribution$y,
                       Method = "Regular Bootstrap")

  full_df <- rbind(exact_df, reg_df)

  p <- ggplot2::ggplot(full_df, ggplot2::aes(x = x, y = y, color = Method)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Comparison of Bootstrap Distributions", x = "Sample Stat", y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("Exact Bootstrap" = "blue", "Regular Bootstrap" = "red"))

  return(p)
}
