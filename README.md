
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exactamente

<!-- badges: start -->
<!-- badges: end -->

`exactamente` is a versatile R package that offers a collection of tools
to assist researchers and data analysts in exploring different bootstrap
methods on small sample size data. Bootstrap methods are widely used for
estimating the sampling distribution of an estimator by resampling with
replacement from the original sample.

This package is focused particularly on the exact case bootstrap and the
exact bootstrap as described in [Jekel & Romero
(2020)](https://doi.org/10.2172/1605343). Both methods are advantageous
for bootstrapping small data sets where standard methods might be
inadequate.

For a given sample size N, the exact case bootstrap generates all
(2N - 1) choose N resamples. For instance, for the sample \[1,2,7\] as
used in the Jekel & Romero paper, we would see 10 unique resamples:

\[1, 1, 1\] \[1, 1, 2\] \[1, 1, 7\] \[1, 2, 2\] \[1, 2, 7\] \[1, 7, 7\]
\[2, 2, 2\] \[2, 2, 7\] \[2, 7, 7\] \[7, 7, 7\].

In contrast, the exact bootstrap generates N^N resamples, thereby
including permutations such as \[1, 1, 2\], \[1, 2, 1\], and \[2, 1, 1\]
as distinct resamples. This method can provide a richer set of
resamples, potentially enhancing the accuracy of the bootstrap estimate.

Furthermore, `exactamente` provides a standard bootstrap function where
the user can specify the desired number of resamples, allowing for
direct comparison between the exact methods and conventional bootstrap
techniques.

## Installation

You can install the development version of `exactamente` from GitHub
like so:

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/exactamente")
```

## Example Usage

After installation, you can load the `exactamente` package using the
library function.

``` r
library(exactamente)
```

To utilize the fundamental tools provided by `exactamente`, begin by
creating a numeric vector of data. You can then use the \_bootstrap
functions to obtain objects holding summary statistics and density
estimates of the respective bootstrap distributions. In the following
example, we use the \[1, 2, 7\] sample vector and take the mean as our
test statistic.

``` r
data <- c(1, 2, 7)

# Run exact case bootstrap
ec_res <- ecase_bootstrap(data)

# Run exact bootstrap
e_res <- exact_bootstrap(data)

# Run regular bootstrap
set.seed(183)
r_res <- reg_bootstrap(data)
```

Each \_bootstrap function comes with plot and summary methods for
further investigation of the bootstrap results.

``` r
res <- list(ec_res, e_res, r_res)

lapply(res, plot)
#> [[1]]
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

    #> 
    #> [[2]]

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

    #> 
    #> [[3]]

<img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" />

``` r
lapply(res, summary)
#> [[1]]
#>            Method nres     mode   median     mean       sd   lCI   uCI
#> 1 ecase_bootstrap   10 2.252608 3.166667 3.333333 1.956313 1.075 6.625
#> 
#> [[2]]
#>            Method nres     mode   median     mean      sd      lCI      uCI
#> 1 exact_bootstrap   27 3.303687 3.333333 3.333333 1.54422 1.216667 5.916667
#> 
#> [[3]]
#>          Method  nres     mode   median   mean       sd lCI uCI
#> 1 reg_bootstrap 10000 3.335788 3.333333 3.3368 1.518024   1   7
```

`exactamente` also includes the `e_vs_r()` function, which enables
direct comparison between the three methods using the same data set.

``` r
set.seed(183)
comp_res <- e_vs_r(data)
comp_res$comp_plot
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
comp_res$summary_table
#>            Method  nres     mode   median     mean       sd      lCI      uCI
#> 1 ecase_bootstrap    10 2.252608 3.166667 3.333333 1.956313 1.075000 6.625000
#> 2 exact_bootstrap    27 3.303687 3.333333 3.333333 1.544220 1.216667 5.916667
#> 3   reg_bootstrap 10000 3.335788 3.333333 3.336800 1.518024 1.000000 7.000000
```

## Interactive Shiny App

One of the exciting features of the `exactamente` package is the
inclusion of an interactive Shiny app. The app allows you to visually
explore and compare different bootstrap methods. It is designed with a
user-friendly interface and offers real-time results visualization.

You can access this app with the following command:

``` r
exactamente_app()
```

## References

Jekel, C. F., & Romero, V. J. (2020). Conservative Estimation of Tail
Probabilities from Limited Sample Data.
