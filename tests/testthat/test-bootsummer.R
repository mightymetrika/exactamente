# Test 1: Functionality
test_that("bootsummer works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  dens <- reg_bootstrap(data)
  result <- bootsummer(dens)
  expect_type(result, "list")
  expect_named(result, c('mode', 'mean', 'sd', 'lHDI', 'uHDI'))
})

# Test 2: `lb` and `ub` parameters
test_that("`lb` and `ub` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  dens <- reg_bootstrap(data)
  result1 <- bootsummer(dens, lb = 0.01, ub = 0.99)
  result2 <- bootsummer(dens, lb = 0.025, ub = 0.975)
  expect_true(any(result1$lHDI != result2$lHDI))
  expect_true(any(result1$uHDI != result2$uHDI))
})

# Test 3: Error handling
test_that("bootsummer throws an error if `dens` is not a list containing numeric vectors `x` and `y`", {
  dens <- list(x = letters[1:5], y = letters[1:5])
  expect_error(bootsummer(dens))
})
