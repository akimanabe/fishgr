context("fit growth")

test_that("function exists", {
  expect_is(fit_vb, "function")
})

test_that("function estimates parameters", {
  foo <-
    tibble::tibble(Age = seq(0, 5, 0.1),
                   Length = vb(seq(0, 5, 0.1),
                               ps = c(300, 0.6, 0)))

  expect_is(fit_vb(foo,
                       ps = c(200, 0.8, 0)),
            "data.frame")
})
