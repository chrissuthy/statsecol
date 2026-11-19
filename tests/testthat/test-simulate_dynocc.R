test_that("the same ID always gives the same data", {
  capture.output(a <- simulate_dynocc(220012345))
  capture.output(b <- simulate_dynocc("220012345"))
  expect_identical(a, b)
  capture.output(d <- simulate_dynocc(220012346))
  expect_false(identical(a$y, d$y))
})

test_that("the data for a known ID have not changed", {
  capture.output(a <- simulate_dynocc(220012345))
  expect_equal(unname(a$design), c(90, 8, 3))
  expect_equal(unname(a$truth$parameters), c(0.15, 0.21, 0.11, 0.35))
  expect_equal(sum(a$y), 344)
})

test_that("the output has the documented structure", {
  out <- capture.output(a <- simulate_dynocc(220010011))
  expect_named(a, c("student_id", "y", "design", "truth"))
  expect_equal(unname(dim(a$y)), unname(a$design[c("n_sites", "n_visits", "n_seasons")]))
  expect_true(all(a$y %in% c(0L, 1L)))
  expect_length(a$truth$prop_occupied, a$design[["n_seasons"]])
  expect_true(any(grepl("Data generating", out)))
})

test_that("an invalid ID gives an informative error", {
  expect_error(simulate_dynocc("abc"), "matriculation number")
  expect_error(simulate_dynocc(c(1, 2)), "matriculation number")
})
