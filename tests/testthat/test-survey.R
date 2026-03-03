test_that("svy_mean returns correct estimate without design", {
  x <- c(1, 0, 1, 1, 0)
  result <- svy_mean(x)
  expect_equal(result$estimate, 0.6)
  expect_true(is.na(result$se))
})

test_that("svy_mean handles subsetting", {
  x <- c(1, 0, 1, 1, 0)
  sub <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  result <- svy_mean(x, subset_expr = sub)
  expect_equal(result$estimate, 2/3, tolerance = 1e-6)
})

test_that("svy_ratio returns correct estimate without design", {
  num <- c(2, 4, 6)
  den <- c(1, 1, 1)
  result <- svy_ratio(num, den)
  expect_equal(result$estimate, 12/3, tolerance = 1e-6)
})

test_that("weai works with survey design", {
  skip_if_not_installed("survey")
  weai_sample <- load_weai_sample()

  # Remove NAs for clean design
  complete_rows <- complete.cases(weai_sample[, c("autonomy_inc", "selfeff",
    "never_violence", "feelinputdecagr", "assetownership", "credit_accdec",
    "incomecontrol", "work_balance", "mobility", "groupmember")])
  ws_clean <- weai_sample[complete_rows, ]

  des <- survey::svydesign(ids = ~1, data = ws_clean)

  res <- weai(
    design = des,
    domains = list(
      intrinsic = c("autonomy_inc", "selfeff", "never_violence"),
      instrumental = c("feelinputdecagr", "assetownership", "credit_accdec",
                        "incomecontrol", "work_balance", "mobility"),
      collective = c("groupmember")
    ),
    sex = "sex", female = 2, hhid = "hhid"
  )

  expect_s3_class(res, "weai_result")
  expect_true(res$config$has_design)
})
