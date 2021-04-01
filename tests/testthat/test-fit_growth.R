context("fit growth")

test_that("function exists", {
  expect_is(fit_growth, "function")
})

test_that("function estimates parameters", {
  sampledata <- vb(seq(0, 5, 0.1), ps = c(300, 0.6, 0))

  expect_is(fit_growth(sampledata,
                       ps = c(200, 0.8, 0)),
            "data.frame")
})
