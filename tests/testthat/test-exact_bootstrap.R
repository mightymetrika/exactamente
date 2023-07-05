# Test 1: Functionality
test_that("exact_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  result <- exact_bootstrap(data)
  expect_type(result, "list")
  expect_s3_class(result, "extboot")
  expect_named(result$dens, c("x", "y", "bw", "n", "call", "data.name","has.na"))
  expect_named(result$stats, c("mode", "median", "mean", "sd", "lCI", "uCI"))
})

# Test 2: Number of observations
test_that("exact_bootstrap throws an error if the number of observations is more than 9", {
  set.seed(111)
  data <- rnorm(10)
  expect_error(exact_bootstrap(data))
})

# Test 3: `n_bootstraps` and `anon` parameters
test_that("`n_bootstraps` and `anon` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- exact_bootstrap(data, n_bootstraps = 5000, anon = median)
  result2 <- exact_bootstrap(data, n_bootstraps = 10000, anon = mean)
  expect_true(any(result1$dens$x != result2$dens$x) || any(result1$dens$y != result2$dens$y))
})

# Test 4: Error handling
test_that("exact_bootstrap throws an error if `data` is not a numeric vector", {
  data <- letters[1:5]
  expect_error(exact_bootstrap(data))
})

# Test 5: `lb` and `ub` parameters
test_that("`lb` and `ub` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- exact_bootstrap(data, lb = 0.01, ub = 0.99)
  result2 <- exact_bootstrap(data, lb = 0.025, ub = 0.975)
  expect_true(any(result1$stats$lCI != result2$stats$lCI))
  expect_true(any(result1$stats$uCI != result2$stats$uCI))
})

# Test 6: `density_args` parameter
test_that("`density_args` works correctly", {
  set.seed(123)
  data <- rnorm(5)
  result <- exact_bootstrap(data, density_args = list(kernel = "cosine"))
  expect_type(result, "list")
  expect_named(result$dens, c("x", "y", "bw", "n", "call", "data.name","has.na"))
  expect_named(result$stats, c("mode", "median", "mean", "sd", "lCI", "uCI"))
})

# Test 7: Plot method
test_that("plot method works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  exact_bootstrap_result <- exact_bootstrap(data)
  result_exact <- plot(exact_bootstrap_result)
  expect_s3_class(result_exact, c("gg", "ggplot"))
})

# Test 8: Summary method
test_that("plot method works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  exact_bootstrap_result <- exact_bootstrap(data)
  result_exact <- summary(exact_bootstrap_result)
  expect_s3_class(result_exact, c("data.frame"))
  expect_equal(length(result_exact), 7)
})


# Test 9: Error handling
test_that("plot method throws an error if dens is not an object of class density", {
  set.seed(123)
  data <- rnorm(5)
  exact_bootstrap_result <- exact_bootstrap(data)
  class(exact_bootstrap_result$dens) <- "hello world"
  expect_error(plot(exact_bootstrap_result))
})

# Test 10: Error handling
test_that("plot method throws an error if object is not an object of class extboot", {
  set.seed(123)
  data <- rnorm(5)
  exact_bootstrap_result <- exact_bootstrap(data)
  class(exact_bootstrap_result) <- "hello world"
  expect_error(plot(exact_bootstrap_result))
})

