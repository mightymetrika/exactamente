# Test 1: Functionality
test_that("exact_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  result <- exact_bootstrap(data)
  expect_type(result, "list")
  expect_named(result, c("x", "y", "bw", "n", "call", "data.name","has.na" ))
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
  expect_true(any(result1$x != result2$x) || any(result1$y != result2$y))
})

# Test 4: Error handling
test_that("exact_bootstrap throws an error if `data` is not a numeric vector", {
  data <- letters[1:5]
  expect_error(exact_bootstrap(data))
})
