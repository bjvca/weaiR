test_that("weai returns a weai_result object", {
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

  expect_s3_class(res, "weai_result")
  expect_true("individual" %in% names(res))
  expect_true("aggregate" %in% names(res))
  expect_true("decomposition" %in% names(res))
  expect_null(res$subgroup)
})

test_that("weai DE values satisfy 1 - M0 identity", {
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

  de_f <- res$aggregate$de_female
  expect_equal(de_f$DE, 1 - de_f$M0, tolerance = 1e-6)
  expect_equal(de_f$M0, de_f$H * de_f$A, tolerance = 1e-6)

  de_m <- res$aggregate$de_male
  expect_equal(de_m$DE, 1 - de_m$M0, tolerance = 1e-6)
  expect_equal(de_m$M0, de_m$H * de_m$A, tolerance = 1e-6)
})

test_that("weai GPI satisfies 1 - HGPI*IGPI identity", {
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

  gpi <- res$aggregate$gpi
  expect_equal(gpi$GPI, 1 - gpi$HGPI * gpi$IGPI, tolerance = 1e-6)
})

test_that("weai WEAI = 0.9*DE + 0.1*GPI", {
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

  expected <- 0.9 * res$aggregate$de_female$DE + 0.1 * res$aggregate$gpi$GPI
  expect_equal(res$aggregate$overall$WEAI, expected, tolerance = 1e-6)
})

test_that("weai with subgroups returns subgroup results", {
  weai_sample <- load_weai_sample()

  res <- weai(
    weai_sample,
    domains = list(
      intrinsic = c("autonomy_inc", "selfeff", "never_violence"),
      instrumental = c("feelinputdecagr", "assetownership", "credit_accdec",
                        "incomecontrol", "work_balance", "mobility"),
      collective = c("groupmember")
    ),
    sex = "sex", female = 2, hhid = "hhid", by = "group"
  )

  expect_true(!is.null(res$subgroup))
  expect_equal(length(res$subgroup), 3)
  for (sg in res$subgroup) {
    expect_true(!is.null(sg$de_female))
    expect_true(!is.null(sg$gpi))
    expect_true(!is.na(sg$weai))
  }
})

test_that("weai with custom cutoff works", {
  weai_sample <- load_weai_sample()

  res75 <- weai(
    weai_sample,
    domains = list(
      intrinsic = c("autonomy_inc", "never_violence"),
      instrumental = c("feelinputdecagr", "assetownership", "credit_accdec",
                        "incomecontrol", "work_balance"),
      collective = c("groupmember")
    ),
    cutoff = 0.75,
    sex = "sex", female = 2, hhid = "hhid"
  )

  expect_s3_class(res75, "weai_result")
  expect_equal(res75$config$cutoff, 0.75)
})

test_that("weai with 5-domain A-WEAI config works", {
  weai_sample <- load_weai_sample()

  res <- weai(
    weai_sample,
    domains = list(
      d1 = c("feelinputdecagr"),
      d2 = c("assetownership", "credit_accdec"),
      d3 = c("incomecontrol"),
      d4 = c("work_balance"),
      d5 = c("groupmember")
    ),
    weights = list(
      d1 = 0.2,
      d2 = c(0.13333, 0.06667),
      d3 = 0.2,
      d4 = 0.2,
      d5 = 0.2
    ),
    sex = "sex", female = 2, hhid = "hhid"
  )

  expect_s3_class(res, "weai_result")
  expect_equal(res$config$n_domains, 5)
  expect_equal(res$config$n_indicators, 6)
})

test_that("weai decomposition relative contributions sum to ~100", {
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

  rel_f <- sum(res$decomposition$female$rel_contribution, na.rm = TRUE)
  rel_m <- sum(res$decomposition$male$rel_contribution, na.rm = TRUE)
  expect_equal(rel_f, 100, tolerance = 0.5)
  expect_equal(rel_m, 100, tolerance = 0.5)
})
