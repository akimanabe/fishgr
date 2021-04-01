context("vb")

test_that("function exists", {
  expect_is(vb, "function")
})

test_that("function estimates Length at age", {
  length_1 <- vb(age = 1, ps = c(300, 0.6, 0))
  expect_equal(length_1, 135.3565, tolerance = 0.0001)
})