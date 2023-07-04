# Test 1: Functionality
test_that("boot_plot works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  exact_bootstrap_result <- exact_bootstrap(data)
  regular_bootstrap_result <- reg_bootstrap(data)
  result_exact <- boot_plot(exact_bootstrap_result)
  result_reg <- boot_plot(regular_bootstrap_result)
  expect_s3_class(result_exact, c("gg", "ggplot"))
  expect_s3_class(result_reg, c("gg", "ggplot"))
})

# Test 2: Error handling
test_that("boot_plot throws an error if dens is not an object of class density", {
  set.seed(123)
  data <- rnorm(5)
  exact_bootstrap_result <- exact_bootstrap(data)
  class(exact_bootstrap_result) <- "hello world"
  expect_error(boot_plot(exact_bootstrap_result))
           })
