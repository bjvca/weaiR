test_that("compute_scores computes correct deprivation scores", {
  df <- data.frame(a = c(1, 0, 1), b = c(1, 1, 0), c = c(0, 0, 1))
  w <- c(a = 1/3, b = 1/3, c = 1/3)
  result <- compute_scores(df, c("a", "b", "c"), w, 0.8)

  # Row 1: inadequate in c only. dscore = 1/3
  expect_equal(result$dscore[1], 1/3, tolerance = 1e-4)
  expect_equal(result$emp_score[1], 2/3, tolerance = 1e-4)
  expect_equal(result$empowered[1], 0)  # 0.667 < 0.8

  # Row 2: inadequate in a and c. dscore = 2/3
  expect_equal(result$dscore[2], 2/3, tolerance = 1e-4)
  expect_equal(result$empowered[2], 0)

  # Row 3: inadequate in b only. dscore = 1/3
  expect_equal(result$dscore[3], 1/3, tolerance = 1e-4)
})

test_that("compute_scores handles NA indicators", {
  df <- data.frame(a = c(1, NA), b = c(1, 1), c = c(0, 1))
  w <- c(a = 1/3, b = 1/3, c = 1/3)
  result <- compute_scores(df, c("a", "b", "c"), w, 0.8)
  expect_true(is.na(result$dscore[2]))
  expect_true(is.na(result$emp_score[2]))
  expect_true(is.na(result$empowered[2]))
})

test_that("compute_scores empowerment threshold works", {
  # All adequate => dscore = 0, emp_score = 1, empowered = 1
  df <- data.frame(a = 1, b = 1, c = 1)
  w <- c(a = 1/3, b = 1/3, c = 1/3)
  result <- compute_scores(df, c("a", "b", "c"), w, 0.8)
  expect_equal(result$empowered[1], 1)
  expect_equal(result$emp_score[1], 1)
})

test_that("compute_scores with custom weights", {
  df <- data.frame(a = c(0), b = c(1), c = c(1))
  w <- c(a = 0.5, b = 0.3, c = 0.2)
  result <- compute_scores(df, c("a", "b", "c"), w, 0.8)
  # Inadequate only in a. dscore = 0.5
  expect_equal(result$dscore[1], 0.5, tolerance = 1e-4)
  expect_equal(result$emp_score[1], 0.5, tolerance = 1e-4)
})
