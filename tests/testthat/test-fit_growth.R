context("fit vb")

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

test_that("function estimates parameters with t0 fixed as 0", {
  bar <-
    tibble::tibble(Age = seq(0, 5, 0.1),
                   Length = vb(seq(0, 5, 0.1),
                               ps = c(300, 0.6, -0.5)))
  expect_equal(
    fit_vb(bar, ps = c(300, 0.6), setorigin = TRUE) %>%
      dplyr::pull(value),
    c(278, 1.06), tolerance = 0.01
  )
})

context("fit ext_vb")

test_that("function exits", {
  expect_is(fit_ext_vb, "function")
})

test_that("function estimates parameters", {
  foo <- tibble::tibble(Age = seq(0, 5, 0.1),
                        Length = ext_vb(age = seq(0, 5, 0.1),
                                        ps = c(300, 0.6, 0.4, 3, 0)))

  expect_is(fit_ext_vb(foo,
                       ps = c(200, 0.6, 0.4, 2, 0)),
            "data.frame")
})

test_that("function estimates parameters with t0 fixed as 0", {
  bar <- tibble::tibble(Age = seq(0, 5, 0.1),
                        Length = ext_vb(age = seq(0, 5, 0.1),
                                        ps = c(300, 0.6, 0.4, 3, -0.3)))
  expect_equal(
    fit_ext_vb(bar, ps = c(300, 0.5, 0.5, 2), setorigin = TRUE) %>%
      dplyr::pull(value),
    c(291, 2.45, 0.648, 0.0977), tolerance = 0.01
  )
})
