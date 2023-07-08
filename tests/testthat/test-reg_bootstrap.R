# Test 1: Functionality
test_that("reg_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  result <- reg_bootstrap(data)
  expect_s3_class(result, "regboot")
  expect_type(result, "list")
  expect_equal(names(result$dens)[[1]], "x")
  expect_equal(names(result$dens)[[2]], "y")
  expect_named(result$stats, c("nres", "mode", "median", "mean", "sd", "lCI", "uCI"))
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

# Test 5: `density_args` parameter
test_that("`density_args` works correctly", {
  set.seed(123)
  data <- rnorm(5)
  result <- reg_bootstrap(data, density_args = list(kernel = "cosine"))
  expect_type(result, "list")
  expect_equal(names(result$dens)[[1]], "x")
  expect_equal(names(result$dens)[[2]], "y")
  expect_named(result$stats, c("nres", "mode", "median", "mean", "sd", "lCI", "uCI"))
})

# Test 6: Plot method
test_that("plot method works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  reg_bootstrap_result <- reg_bootstrap(data)
  result_exact <- plot(reg_bootstrap_result)
  expect_s3_class(result_exact, c("gg", "ggplot"))
})

# Test 7: Summary method
test_that("plot method works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  reg_bootstrap_result <- reg_bootstrap(data)
  result_exact <- summary(reg_bootstrap_result)
  expect_s3_class(result_exact, c("data.frame"))
  expect_equal(length(result_exact), 8)
})


# Test 8: Error handling
test_that("plot method throws an error if dens is not an object of class density", {
  set.seed(123)
  data <- rnorm(5)
  reg_bootstrap_result <- reg_bootstrap(data)
  class(reg_bootstrap_result$dens) <- "hello world"
  expect_error(plot(reg_bootstrap_result))
})

# Test 9: Error handling
test_that("plot method throws an error if object is not an object of class extboot", {
  set.seed(123)
  data <- rnorm(5)
  reg_bootstrap_result <- reg_bootstrap(data)
  class(reg_bootstrap_result) <- "hello world"
  expect_error(plot(reg_bootstrap_result))
})

