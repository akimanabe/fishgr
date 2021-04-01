context("fit growth")

test_that("function exists", {
  expect_is(fit_growth, "function")
})

test_that("function estimates parameters", {
  expect_is(fit_growth(foo, ps = c(300, 0.6, 0), model = "vb"), "data.frame")
})
