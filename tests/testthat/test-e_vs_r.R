# Test 1: Functionality
test_that("e_vs_r works correctly with a valid input", {
  set.seed(123)
  data <- rnorm(5)
  result <- e_vs_r(data)
  expect_type(result, "list")
  expect_named(result, c("summary_table", "comp_plot"))
  expect_s3_class(result$summary_table, "data.frame")
  expect_s3_class(result$comp_plot, c("gg", "ggplot"))
  expect_equal(nrow(result$summary_table), 2)
  expect_equal(result$summary_table$Method, c("Exact_bootstrap", "Regular_bootstrap"))
})

# Test 2: `n_bootstraps`, `check_size`, `anon`, `lb` and `ub` parameters
test_that("`n_bootstraps`, `check_size`, `anon`, `lb` and `ub` parameters modify the result", {
  set.seed(123)
  data <- rnorm(5)
  result1 <- e_vs_r(data)
  result2 <- e_vs_r(data, n_bootstraps = 5000, check_size = FALSE, anon = function(x)(median(x)), lb = 0.01, ub = 0.99)
  # The summary tables should be different
  expect_false(result1$summary_table$mode[[1]] == result2$summary_table$mode[[1]])
  expect_false(result1$summary_table$mode[[2]] == result2$summary_table$mode[[2]])
  expect_false(result1$summary_table$mean[[1]] == result2$summary_table$mean[[1]])
  expect_false(result1$summary_table$mean[[2]] == result2$summary_table$mean[[2]])
})

# Test 3: Error handling
test_that("e_vs_r throws an error if `data` is not a numeric vector", {
  data <- letters[1:5]
  expect_error(e_vs_r(data))
})
