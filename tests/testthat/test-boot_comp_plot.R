# Test 1: Functionality
test_that("compare_bootstrap works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  ecase_bootstrap_result <- ecase_bootstrap(data)
  exact_bootstrap_result <- exact_bootstrap(data)
  regular_bootstrap_result <- reg_bootstrap(data)
  result <- compare_bootstrap(ecase_bootstrap_result, exact_bootstrap_result, regular_bootstrap_result)
  expect_s3_class(result, c("gg", "ggplot"))
  expect_length(unique(result[["data"]]$Method), 3)  # We expect to have 2 values (one for each bootstrap method)
})

# Test 2: Error handling
test_that("compare_bootstrap throws an error if `ecase_bootstrap_distribution`
          `exact_bootstrap_distribution` or `regular_bootstrap_distribution`
          is not an object of class density", {
             wcase_distribution <- list(x = letters[1:5], y = letters[1:5])
             wrong_distribution <- list(x = letters[1:5], y = letters[1:5])
             correct_distribution <- list(x = rnorm(5), y = rnorm(5))
             expect_error(compare_bootstrap(wcase_distribution,
                                            wrong_distribution,
                                            correct_distribution))
             expect_error(compare_bootstrap(wcase_distribution,
                                            correct_distribution,
                                            wrong_distribution))
           })
