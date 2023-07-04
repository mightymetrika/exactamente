# Test 1: Functionality
test_that("compare_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  exact_bootstrap_result <- exact_bootstrap(data)
  regular_bootstrap_result <- reg_bootstrap(data)
  result <- compare_bootstrap(exact_bootstrap_result, regular_bootstrap_result)
  expect_s3_class(result, c("gg", "ggplot"))
  expect_length(unique(result[["data"]]$Method), 2)  # We expect to have 2 values (one for each bootstrap method)
})

# Test 2: Error handling
test_that("compare_bootstrap throws an error if `exact_bootstrap_distribution`
           or `regular_bootstrap_distribution` is not an object of class density", {
             wrong_distribution <- list(x = letters[1:5], y = letters[1:5])
             correct_distribution <- list(x = rnorm(5), y = rnorm(5))
             expect_error(compare_bootstrap(wrong_distribution, correct_distribution))
             expect_error(compare_bootstrap(correct_distribution, wrong_distribution))
           })
