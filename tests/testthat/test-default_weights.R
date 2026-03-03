test_that("resolve_weights returns equal weights for 3 domains (pro-WEAI)", {
  domains <- list(d1 = c("a", "b", "c"), d2 = c("d", "e"), d3 = c("f"))
  w <- resolve_weights(domains)
  expect_equal(length(w), 6)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  # All indicators have equal weight = 1/6

  expect_true(all(abs(w - 1/6) < 1e-10))
})

test_that("resolve_weights returns domain-based weights for 5 domains", {
  domains <- list(d1 = c("a", "b"), d2 = c("c", "d", "e"),
                  d3 = c("f"), d4 = c("g"), d5 = c("h", "i"))
  w <- resolve_weights(domains)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  # d1 has 2 indicators, each gets 1/10
  expect_equal(w["a"], c(a = 0.1), tolerance = 1e-10)
  expect_equal(w["b"], c(b = 0.1), tolerance = 1e-10)
  # d2 has 3 indicators, each gets 1/15
  expect_equal(as.numeric(w["c"]), 1/15, tolerance = 1e-10)
  # d3 has 1 indicator, gets 1/5
  expect_equal(as.numeric(w["f"]), 0.2, tolerance = 1e-10)
})

test_that("resolve_weights uses provided weights", {
  domains <- list(d1 = c("a", "b"), d2 = c("c"), d3 = c("d"))
  weights <- list(d1 = c(0.3, 0.2), d2 = 0.3, d3 = 0.2)
  w <- resolve_weights(domains, weights)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  expect_equal(as.numeric(w["a"]), 0.3)
  expect_equal(as.numeric(w["c"]), 0.3)
})

test_that("resolve_weights rescales near-1 weights", {
  domains <- list(d1 = c("a"), d2 = c("b"), d3 = c("c"))
  weights <- list(d1 = 0.334, d2 = 0.333, d3 = 0.333)
  w <- resolve_weights(domains, weights)
  expect_equal(sum(w), 1, tolerance = 1e-10)
})
