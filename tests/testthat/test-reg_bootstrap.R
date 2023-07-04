# Test 1: Functionality
test_that("reg_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  result <- reg_bootstrap(data)
  expect_type(result, "list")
  expect_named(result, c("x", "y", "bw", "n", "call", "data.name","has.na" ))
})

# Test 2: `n_bootstraps` and `anon` parameters
test_that("`n_bootstraps` and `anon` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- reg_bootstrap(data, n_bootstraps = 5000, anon = median)
  result2 <- reg_bootstrap(data, n_bootstraps = 10000, anon = mean)
  expect_true(any(result1$x != result2$x) || any(result1$y != result2$y))
})

# Test 3: Error handling
test_that("reg_bootstrap throws an error if `data` is not a numeric vector", {
  data <- letters[1:5]
  expect_error(reg_bootstrap(data))
})
