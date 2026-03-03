test_that("validate_inputs requires data or design", {
  expect_error(
    validate_inputs(NULL, list(d1 = "a", d2 = "b", d3 = "c"), NULL,
                    "sex", 2, "hhid", 0.8, NULL, NULL),
    "Either 'data' or 'design'"
  )
})

test_that("validate_inputs rejects non-data.frame", {
  expect_error(
    validate_inputs("not_a_df", list(d1 = "a", d2 = "b", d3 = "c"), NULL,
                    "sex", 2, "hhid", 0.8, NULL, NULL),
    "must be a data.frame"
  )
})

test_that("validate_inputs requires at least 3 domains", {
  df <- data.frame(a = 1, b = 1, sex = 1, hhid = 1)
  expect_error(
    validate_inputs(df, list(d1 = "a", d2 = "b"), NULL,
                    "sex", 1, "hhid", 0.8, NULL, NULL),
    "at least 3"
  )
})

test_that("validate_inputs requires named domains", {
  df <- data.frame(a = 1, b = 1, c = 1, sex = 1, hhid = 1)
  expect_error(
    validate_inputs(df, list("a", "b", "c"), NULL,
                    "sex", 1, "hhid", 0.8, NULL, NULL),
    "must be named"
  )
})

test_that("validate_inputs checks indicator columns exist", {
  df <- data.frame(a = 1, b = 1, sex = c(1, 2), hhid = 1)
  expect_error(
    validate_inputs(df, list(d1 = "a", d2 = "b", d3 = "missing_col"), NULL,
                    "sex", 1, "hhid", 0.8, NULL, NULL),
    "not found in data"
  )
})

test_that("validate_inputs checks indicators are binary", {
  df <- data.frame(a = c(0, 1), b = c(0, 1), c = c(0, 2),
                   sex = c(1, 2), hhid = c(1, 1))
  expect_error(
    validate_inputs(df, list(d1 = "a", d2 = "b", d3 = "c"), NULL,
                    "sex", 1, "hhid", 0.8, NULL, NULL),
    "must contain only 0, 1, or NA"
  )
})

test_that("validate_inputs checks weight sum", {
  df <- data.frame(a = c(0, 1), b = c(0, 1), c = c(0, 1),
                   sex = c(1, 2), hhid = c(1, 1))
  expect_error(
    validate_inputs(df, list(d1 = "a", d2 = "b", d3 = "c"),
                    list(d1 = 0.5, d2 = 0.5, d3 = 0.5),
                    "sex", 1, "hhid", 0.8, NULL, NULL),
    "must sum to 1"
  )
})

test_that("validate_inputs checks sex column exists and has 2 values", {
  df <- data.frame(a = c(0, 1), b = c(0, 1), c = c(0, 1),
                   sex = c(1, 1), hhid = c(1, 1))
  expect_error(
    validate_inputs(df, list(d1 = "a", d2 = "b", d3 = "c"), NULL,
                    "sex", 1, "hhid", 0.8, NULL, NULL),
    "exactly 2 unique"
  )
})

test_that("validate_inputs checks cutoff range", {
  df <- data.frame(a = c(0, 1), b = c(0, 1), c = c(0, 1),
                   sex = c(1, 2), hhid = c(1, 1))
  expect_error(
    validate_inputs(df, list(d1 = "a", d2 = "b", d3 = "c"), NULL,
                    "sex", 1, "hhid", 1.5, NULL, NULL),
    "between 0.*and 1"
  )
})

test_that("validate_inputs accepts valid input", {
  df <- data.frame(a = c(0, 1), b = c(0, 1), c = c(0, 1),
                   sex = c(1, 2), hhid = c(1, 1))
  result <- validate_inputs(df, list(d1 = "a", d2 = "b", d3 = "c"), NULL,
                            "sex", 1, "hhid", 0.8, NULL, NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})
