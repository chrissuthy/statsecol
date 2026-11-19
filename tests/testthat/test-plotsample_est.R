test_that("point estimate, se and cv match the formulas", {
  n <- c(3, 0, 5, 2)
  p <- 0.25
  b <- plotsample_est(n, p, dbn = "binomial")
  expect_equal(b$Nhat, 40)
  expect_equal(b$se.Nhat, sqrt(40 * 0.75 / 0.25))
  expect_equal(b$cv.Nhat, b$se.Nhat / 40)
  po <- plotsample_est(n, p, dbn = "poisson")
  expect_equal(po$se.Nhat, sqrt(40 / 0.25))
})

test_that("interval methods give sensible intervals", {
  n <- c(3, 0, 5, 2, 4, 6, 1, 3)
  for (m in c("normal", "lognormal", "exact")) {
    for (d in c("binomial", "poisson")) {
      out <- plotsample_est(n, 0.2, method = m, dbn = d)
      expect_length(out$ci.Nhat, 2)
      expect_lt(out$ci.Nhat[1], out$Nhat)
      expect_gt(out$ci.Nhat[2], out$Nhat)
    }
  }
  set.seed(1)
  for (m in c("normal", "lognormal", "percentile")) {
    out <- plotsample_est(n, 0.2, method = m, dbn = "bootstrap", B = 199)
    expect_length(out$ci.Nhat, 2)
    expect_lt(out$ci.Nhat[1], out$ci.Nhat[2])
  }
})

test_that("confidence interval helpers are correct", {
  expect_equal(norm.ci(100, 20), 100 + c(-1, 1) * qnorm(0.975) * 20)
  ci <- lognorm.ci(100, 20)
  expect_equal(prod(ci), 100^2)
  expect_true(all(ci > 0))
})

test_that("invalid input gives an informative error", {
  expect_error(plotsample_est(10, 0.5, method = "percentile"), "Invalid method")
  expect_error(plotsample_est(c(1, 2), 0.5, method = "exact", dbn = "bootstrap"),
               "Invalid method")
  expect_error(plotsample_est(10, 0.5, dbn = "normal"), "Invalid dbn")
  expect_error(plotsample_est(10, 0.5, dbn = "bootstrap"), "individual plot counts")
  expect_error(plotsample_est(10, 0), "p must be")
  expect_error(plotsample_est(c(1, NA), 0.5), "n must be")
})

test_that("data sets match their documentation", {
  expect_equal(dim(wildebeest), c(30L, 8L))
  expect_named(wildebeest,
               c("year", "Nhat", "sehat", "cvhat", "lci", "uci", "rain", "Catch"))
  expect_equal(dim(voles), c(114L, 11L))
  expect_equal(dim(gtit), c(263L, 11L))
  expect_equal(dim(redsq), c(264L, 11L))
  expect_equal(dim(willow), c(237L, 16L))
  expect_equal(dim(falcons), c(40L, 5L))
  expect_equal(dim(burnet), c(87L, 6L))
  expect_equal(sum(plotsamp$Count), 150)
})
