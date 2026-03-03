test_that("compute_decomposition returns correct structure", {
  df <- data.frame(
    a = c(1, 0, 1, 0),
    b = c(1, 1, 0, 0),
    c = c(0, 0, 1, 1)
  )
  indicators <- c("a", "b", "c")
  w <- c(a = 1/3, b = 1/3, c = 1/3)
  domains <- list(d1 = c("a", "b"), d2 = c("c"))
  empowered <- c(1, 0, 0, 0)
  M0 <- 0.3  # arbitrary for testing

  result <- compute_decomposition(df, indicators, w, domains, empowered, M0)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("indicator", "domain", "weight", "uncensored_hc",
                     "censored_hc", "abs_contribution", "rel_contribution")
                   %in% names(result)))
})

test_that("compute_decomposition uncensored headcounts are correct", {
  df <- data.frame(a = c(1, 0, 0, 1), b = c(1, 1, 1, 0), c = c(0, 1, 0, 0))
  indicators <- c("a", "b", "c")
  w <- c(a = 1/3, b = 1/3, c = 1/3)
  domains <- list(d1 = c("a"), d2 = c("b"), d3 = c("c"))
  empowered <- c(0, 0, 0, 0)
  M0 <- 0.5

  result <- compute_decomposition(df, indicators, w, domains, empowered, M0)

  # a: inadequate in rows 2,3 => 2/4 = 0.5
  expect_equal(result$uncensored_hc[result$indicator == "a"], 0.5)
  # b: inadequate in row 4 => 1/4 = 0.25
  expect_equal(result$uncensored_hc[result$indicator == "b"], 0.25)
  # c: inadequate in rows 1,3,4 => 3/4 = 0.75
  expect_equal(result$uncensored_hc[result$indicator == "c"], 0.75)
})

test_that("compute_decomposition relative contributions sum to ~100", {
  df <- data.frame(a = c(1, 0, 0), b = c(0, 1, 0), c = c(1, 1, 0))
  indicators <- c("a", "b", "c")
  w <- c(a = 1/3, b = 1/3, c = 1/3)
  domains <- list(d1 = c("a"), d2 = c("b"), d3 = c("c"))
  empowered <- c(0, 0, 0)

  # Compute M0 properly
  dscore <- c(1/3, 1/3, 1)
  M0 <- mean(dscore)

  result <- compute_decomposition(df, indicators, w, domains, empowered, M0)

  total_rel <- sum(result$rel_contribution, na.rm = TRUE)
  expect_equal(total_rel, 100, tolerance = 0.1)
})

test_that("compute_decomposition handles subsetting", {
  df <- data.frame(a = c(1, 0, 0, 1), b = c(0, 1, 0, 1), c = c(1, 0, 1, 0))
  indicators <- c("a", "b", "c")
  w <- c(a = 1/3, b = 1/3, c = 1/3)
  domains <- list(d1 = c("a"), d2 = c("b"), d3 = c("c"))
  empowered <- c(0, 0, 0, 0)
  M0 <- 0.4

  sub <- c(TRUE, TRUE, FALSE, FALSE)
  result <- compute_decomposition(df, indicators, w, domains, empowered, M0,
                                  subset_expr = sub)

  # Only first 2 rows
  # a: inadequate in row 2 => 1/2 = 0.5
  expect_equal(result$uncensored_hc[result$indicator == "a"], 0.5)
})
