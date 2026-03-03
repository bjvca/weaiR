test_that("print.weai_result produces output", {
  weai_sample <- load_weai_sample()

  res <- weai(
    weai_sample,
    domains = list(
      intrinsic = c("autonomy_inc", "selfeff", "never_violence"),
      instrumental = c("feelinputdecagr", "assetownership", "credit_accdec",
                        "incomecontrol", "work_balance", "mobility"),
      collective = c("groupmember")
    ),
    sex = "sex", female = 2, hhid = "hhid"
  )

  output <- capture.output(print(res))
  expect_true(any(grepl("pro-WEAI", output)))
  expect_true(any(grepl("3DE Index", output)))
  expect_true(any(grepl("GPI", output)))
  expect_true(any(grepl("Women", output)))
})

test_that("summary.weai_result includes decomposition", {
  weai_sample <- load_weai_sample()

  res <- weai(
    weai_sample,
    domains = list(
      intrinsic = c("autonomy_inc", "selfeff", "never_violence"),
      instrumental = c("feelinputdecagr", "assetownership", "credit_accdec",
                        "incomecontrol", "work_balance", "mobility"),
      collective = c("groupmember")
    ),
    sex = "sex", female = 2, hhid = "hhid"
  )

  output <- capture.output(print(summary(res)))
  expect_true(any(grepl("Uncensored", output)))
  expect_true(any(grepl("Censored", output)))
  expect_true(any(grepl("Relative contribution", output)))
})
