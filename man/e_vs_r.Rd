% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/e_vs_r.R
\name{e_vs_r}
\alias{e_vs_r}
\title{Compare Exact Bootstrap vs Regular Bootstrap}
\usage{
e_vs_r(
  data,
  n_bootstraps = 10000,
  check_size = TRUE,
  anon = function(x) (mean(x)),
  lb = 0.025,
  ub = 0.975,
  density_args,
  title = "Comparison of Bootstrap Distributions"
)
}
\arguments{
\item{data}{A numeric vector of data values to be bootstrapped.}

\item{n_bootstraps}{The number of bootstrap samples to generate. Defaults to 10000.}

\item{check_size}{Logical indicating if a check should be performed to ensure
the dataset has less than 10 observations for the exact bootstrap. Defaults to TRUE.}

\item{anon}{An anonymous function to compute the statistic of interest on
each bootstrap sample. Defaults to mean.}

\item{lb}{Lower bound for the confidence interval. Defaults to 0.025.}

\item{ub}{Upper bound for the confidence interval. Defaults to 0.975.}

\item{density_args}{Pass additional arguments to stats::density}

\item{title}{Plot title}
}
\value{
A list containing two items:
\itemize{
\item summary_table: A summary table containing the mode, median, mean,
standard deviation, and confidence interval for each bootstrap method.
\item comp_plot: A ggplot object comparing the bootstrap distributions.
}
}
\description{
This function runs the exact and regular bootstrap functions on
a dataset, summarizes the results, and provides a comparative plot. It provides
a convenient way to compare these two methods of bootstrapping.
}
\examples{
set.seed(123)
data <- rnorm(5)
results <- e_vs_r(data)
print(results$summary_table)
print(results$comp_plot)
}
