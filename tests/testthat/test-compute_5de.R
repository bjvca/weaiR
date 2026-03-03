test_that("compute_5de computes correct H, A, M0, DE", {
  dscore <- c(0.4, 0.3, 0, 0.5)
  empowered <- c(0, 0, 1, 0)

  result <- compute_5de(dscore, empowered)

  # H = 3/4 = 0.75 (3 out of 4 disempowered)
  expect_equal(result$H, 0.75)

  # M0 = mean of censored dscore = (0.4 + 0.3 + 0 + 0.5) / 4
  expect_equal(result$M0, (0.4 + 0.3 + 0 + 0.5) / 4, tolerance = 1e-6)

  # A = M0 / H
  expect_equal(result$A, result$M0 / result$H, tolerance = 1e-6)

  # DE = 1 - M0
  expect_equal(result$DE, 1 - result$M0, tolerance = 1e-6)

  expect_equal(result$N, 4L)
})

test_that("compute_5de handles all empowered", {
  dscore <- c(0.1, 0.05)
  empowered <- c(1, 1)
  result <- compute_5de(dscore, empowered)
  expect_equal(result$H, 0)
  expect_equal(result$M0, 0)
  expect_equal(result$DE, 1)
  expect_true(is.na(result$A))
})

test_that("compute_5de handles subset", {
  dscore <- c(0.5, 0.3, 0, 0.4)
  empowered <- c(0, 0, 1, 0)
  subset_expr <- c(TRUE, TRUE, FALSE, FALSE)

  result <- compute_5de(dscore, empowered, subset_expr = subset_expr)
  expect_equal(result$N, 2L)
  expect_equal(result$H, 1)  # both are disempowered
  expect_equal(result$M0, (0.5 + 0.3) / 2, tolerance = 1e-6)
})

test_that("compute_5de handles empty subset", {
  dscore <- c(0.5)
  empowered <- c(0)
  result <- compute_5de(dscore, empowered, subset_expr = c(FALSE))
  expect_equal(result$N, 0L)
  expect_true(is.na(result$H))
})
