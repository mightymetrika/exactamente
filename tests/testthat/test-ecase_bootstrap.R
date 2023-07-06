# Test 1: Functionality
test_that("ecase_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  result <- ecase_bootstrap(data)
  expect_type(result, "list")
  expect_s3_class(result, "ecboot")
  expect_named(result$dens, c("x", "y", "bw", "n", "call", "data.name","has.na"))
  expect_named(result$stats, c("mode", "median", "mean", "sd", "lCI", "uCI"))
})

# Test 2: Number of observations
test_that("ecase_bootstrap throws an error if the number of observations is more than 9", {
  set.seed(111)
  data <- rnorm(10)
  expect_error(ecase_bootstrap(data))
})

# Test 3: `anon` parameter
test_that("`anon` parameter modififies the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- ecase_bootstrap(data, anon = median)
  result2 <- ecase_bootstrap(data, anon = mean)
  expect_true(any(result1$dens$x != result2$dens$x) || any(result1$dens$y != result2$dens$y))
})

# Test 4: Error handling
test_that("ecase_bootstrap throws an error if `data` is not a numeric vector", {
  data <- letters[1:5]
  expect_error(ecase_bootstrap(data))
})

# Test 5: `lb` and `ub` parameters
test_that("`lb` and `ub` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- ecase_bootstrap(data, lb = 0.01, ub = 0.99)
  result2 <- ecase_bootstrap(data, lb = 0.025, ub = 0.975)
  expect_true(any(result1$stats$lCI != result2$stats$lCI))
  expect_true(any(result1$stats$uCI != result2$stats$uCI))
})

# Test 6: `density_args` parameter
test_that("`density_args` works correctly", {
  set.seed(123)
  data <- rnorm(5)
  result <- ecase_bootstrap(data, density_args = list(kernel = "cosine"))
  expect_type(result, "list")
  expect_named(result$dens, c("x", "y", "bw", "n", "call", "data.name","has.na"))
  expect_named(result$stats, c("mode", "median", "mean", "sd", "lCI", "uCI"))
})

# Test 7: Plot method
test_that("plot method works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  ecase_bootstrap_result <- ecase_bootstrap(data)
  result_exact <- plot(ecase_bootstrap_result)
  expect_s3_class(result_exact, c("gg", "ggplot"))
})

# Test 8: Summary method
test_that("plot method works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  ecase_bootstrap_result <- ecase_bootstrap(data)
  result_ecase <- summary(ecase_bootstrap_result)
  expect_s3_class(result_ecase, c("data.frame"))
  expect_equal(length(result_ecase), 7)
})


# Test 9: Error handling
test_that("plot method throws an error if dens is not an object of class density", {
  set.seed(123)
  data <- rnorm(5)
  ecase_bootstrap_result <- ecase_bootstrap(data)
  class(ecase_bootstrap_result$dens) <- "hello world"
  expect_error(plot(ecase_bootstrap_result))
})

# Test 10: Error handling
test_that("plot method throws an error if object is not an object of class ecboot", {
  set.seed(123)
  data <- rnorm(5)
  ecase_bootstrap_result <- ecase_bootstrap(data)
  class(ecase_bootstrap_result) <- "hello world"
  expect_error(plot(ecase_bootstrap_result))
})
