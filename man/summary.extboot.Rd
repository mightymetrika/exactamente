% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exact_bootstrap.R
\name{summary.extboot}
\alias{summary.extboot}
\title{Summary Method for 'extboot' Class}
\usage{
\method{summary}{extboot}(object, ...)
}
\arguments{
\item{object}{An object of class 'extboot', usually the output of the \code{\link{exact_bootstrap}} function.}

\item{...}{Additional parameters (currently ignored).}
}
\value{
A data.frame containing the summary statistics.
}
\description{
Creates a summary table of the summary statistics computed in the
\code{\link{exact_bootstrap}} function.
}
\examples{
set.seed(123)
data <- rnorm(5)
result <- exact_bootstrap(data)
summary(result)
}
\seealso{
\code{\link{exact_bootstrap}}, \code{\link{plot.extboot}}
}
