test_that("compute_gpi computes correct GPI for simple case", {
  # 2 households, each with 1 male + 1 female
  df <- data.frame(
    hhid = c(1, 1, 2, 2),
    sex = c(2, 1, 2, 1),
    a = c(1, 1, 0, 1),
    b = c(0, 1, 0, 1),
    c = c(1, 1, 0, 1),
    empowered = c(0, 1, 0, 1),
    dscore = c(1/3, 0, 1, 0),
    emp_score = c(2/3, 1, 0, 1)
  )
  w <- c(a = 1/3, b = 1/3, c = 1/3)

  result <- compute_gpi(df, "sex", 2, "hhid", 0.8, c("a", "b", "c"), w)

  expect_equal(result$N_dual, 2L)
  expect_true(!is.na(result$GPI))
  expect_true(result$GPI >= 0 && result$GPI <= 1)
})

test_that("compute_gpi returns NA when no dual households", {
  df <- data.frame(
    hhid = c(1, 2),
    sex = c(2, 1),
    a = c(1, 1),
    empowered = c(1, 1),
    dscore = c(0, 0),
    emp_score = c(1, 1)
  )
  w <- c(a = 1)

  result <- compute_gpi(df, "sex", 2, "hhid", 0.8, c("a"), w)
  expect_equal(result$N_dual, 0L)
  expect_true(is.na(result$GPI))
})

test_that("compute_gpi sets parity when woman is empowered", {
  # Woman empowered => gender parity regardless of scores
  df <- data.frame(
    hhid = c(1, 1),
    sex = c(2, 1),
    a = c(0, 1),
    b = c(1, 1),
    c = c(1, 1),
    empowered = c(1, 0),
    dscore = c(1/3, 0),
    emp_score = c(2/3, 1)
  )
  w <- c(a = 1/3, b = 1/3, c = 1/3)

  result <- compute_gpi(df, "sex", 2, "hhid", 0.8, c("a", "b", "c"), w)
  expect_equal(result$gender_parity[1], 1L)  # Woman has parity
  expect_equal(result$gender_parity[2], 1L)  # Man also has parity (same HH)
})

test_that("compute_gpi returns correct hh_ineq", {
  df <- data.frame(
    hhid = c(1, 1),
    sex = c(2, 1),
    a = c(0, 1),
    b = c(0, 1),
    c = c(1, 1),
    empowered = c(0, 1),
    dscore = c(2/3, 0),
    emp_score = c(1/3, 1)
  )
  w <- c(a = 1/3, b = 1/3, c = 1/3)

  result <- compute_gpi(df, "sex", 2, "hhid", 0.8, c("a", "b", "c"), w)
  # hh_ineq for woman = W_ci - M_ci
  # W_ci = 2/3 (rounded to 0.6667), M_ci = 0
  expect_true(result$hh_ineq[1] > 0)   # woman more deprived
  expect_true(result$hh_ineq[2] < 0)   # man's perspective: negative
})
