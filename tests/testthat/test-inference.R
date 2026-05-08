# =========================================================
# tests/testthat/test-regression-functions.R
# =========================================================

library(testthat)

set.seed(123)

# ---------------------------------------------------------
# Test datasets
# ---------------------------------------------------------

slr_data <- data.frame(
  x = 1:10,
  y = 2 * (1:10) + rnorm(10)
)

log_data <- data.frame(
  x = rnorm(100),
  y = sample(c(0, 1), 100, replace = TRUE)
)

ttest_data <- data.frame(
  group = rep(c("A", "B"), each = 20),
  score = c(
    rnorm(20, mean = 10),
    rnorm(20, mean = 12)
  )
)

anova_data <- data.frame(
  group = rep(c("A", "B", "C"), each = 20),
  score = c(
    rnorm(20, mean = 10),
    rnorm(20, mean = 12),
    rnorm(20, mean = 15)
  )
)

# ---------------------------------------------------------
# simple_linear_regression tests
# ---------------------------------------------------------

test_that("simple_linear_regression returns expected structure", {

  result <- simple_linear_regression(slr_data, x, y)

  expect_type(result, "list")

  expect_named(
    result,
    c("slope", "p_value", "adj_r_squared")
  )
})

test_that("simple_linear_regression errors with non-data.frame", {

  expect_error(
    simple_linear_regression(matrix(1:10), x, y),
    "Data must be a data frame"
  )
})

test_that("simple_linear_regression errors with nonnumeric variables", {

  bad_data <- data.frame(
    x = letters[1:10],
    y = 1:10
  )

  expect_error(
    simple_linear_regression(bad_data, x, y),
    "must be numeric"
  )
})

# ---------------------------------------------------------
# logistic_regression tests
# ---------------------------------------------------------

test_that("logistic_regression returns expected structure", {

  result <- logistic_regression(log_data, x, y)

  expect_type(result, "list")

  expect_named(
    result,
    c("slope", "p_value", "adj_r_squared")
  )
})

test_that("logistic_regression errors when x is not numeric", {

  bad_data <- data.frame(
    x = letters[1:10],
    y = sample(c(0,1), 10, replace = TRUE)
  )

  expect_error(
    logistic_regression(bad_data, x, y),
    "x variable must be numeric"
  )
})

test_that("logistic_regression errors when y is not binary", {

  bad_data <- data.frame(
    x = rnorm(20),
    y = sample(1:3, 20, replace = TRUE)
  )

  expect_error(
    logistic_regression(bad_data, x, y),
    "y variable must be binary"
  )
})

# ---------------------------------------------------------
# t_test tests
# ---------------------------------------------------------

test_that("t_test returns expected structure", {

  result <- t_test(ttest_data, group, score)

  expect_type(result, "list")

  expect_named(
    result,
    c("t_stat", "p_value", "estimate")
  )
})

test_that("t_test errors when y is not numeric", {

  bad_data <- data.frame(
    group = rep(c("A","B"), each = 5),
    score = letters[1:10]
  )

  expect_error(
    t_test(bad_data, group, score),
    "y variable must be quantitative"
  )
})

test_that("t_test errors when x is not binary", {

  bad_data <- data.frame(
    group = rep(c("A","B","C"), each = 5),
    score = rnorm(15)
  )

  expect_error(
    t_test(bad_data, group, score),
    "x variable must be binary"
  )
})

# ---------------------------------------------------------
# anova_test tests
# ---------------------------------------------------------

test_that("anova_test returns expected structure", {

  result <- anova_test(anova_data, group, score)

  expect_type(result, "list")

  expect_named(
    result,
    c("p_value", "f_stat")
  )
})

test_that("anova_test errors when x has fewer than 3 groups", {

  bad_data <- data.frame(
    group = rep(c("A","B"), each = 10),
    score = rnorm(20)
  )

  expect_error(
    anova_test(bad_data, group, score),
    "must be categorical"
  )
})

test_that("anova_test errors when y is not numeric", {

  bad_data <- data.frame(
    group = rep(c("A","B","C"), each = 5),
    score = letters[1:15]
  )

  expect_error(
    anova_test(bad_data, group, score),
    "must be quantitative"
  )
})
