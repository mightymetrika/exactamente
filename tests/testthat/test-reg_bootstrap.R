# Test 1: Functionality
test_that("reg_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  result <- reg_bootstrap(data)
  expect_type(result, "list")
  expect_named(result$dens, c("x", "y", "bw", "n", "call", "data.name","has.na" ))
  expect_named(result$stats, c("mode", "mean", "sd", "lCI", "uCI"))
})

# Test 2: `n_bootstraps` and `anon` parameters
test_that("`n_bootstraps` and `anon` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- reg_bootstrap(data, n_bootstraps = 5000, anon = median)
  result2 <- reg_bootstrap(data, n_bootstraps = 10000, anon = mean)
  expect_true(any(result1$dens$x != result2$dens$x) || any(result1$dens$y != result2$dens$y))
})

# Test 3: Error handling
test_that("reg_bootstrap throws an error if `data` is not a numeric vector", {
  data <- letters[1:5]
  expect_error(reg_bootstrap(data))
})

# Test 4: `lb` and `ub` parameters
test_that("`lb` and `ub` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- reg_bootstrap(data, lb = 0.01, ub = 0.99)
  result2 <- reg_bootstrap(data, lb = 0.025, ub = 0.975)
  expect_true(any(result1$stats$lCI != result2$stats$lCI))
  expect_true(any(result1$stats$uCI != result2$stats$uCI))
})
