context("vb")

test_that("function exists", {
  expect_is(vb, "function")
})

test_that("function estimates Length at age", {
  length_1 <- vb(age = 1, ps = c(300, 0.6, 0))
  expect_equal(length_1, 135.3565, tolerance = 0.0001)
})

test_that("setorigin set t_0 as 0 for VB", {
  length_0 <- vb(age = 1, ps = c(300, 0.5), setorigin = TRUE)
  expect_equal(length_0, 118.0408, tolerance = 0.0001)
})

context("extended vb")

test_that("function exists", {
  expect_is(ext_vb, "function")
})

test_that("function estimates Length at age", {
  length_2 <- ext_vb(2, ps = c(300, 0.6, 0.4, 2, 0))
  expect_equal(length_2, 209.6417, tolerance = 0.0001)

  length_3 <- ext_vb(3, ps = c(300, 0.6, 0.4, 2, 0))
  expect_equal(length_3, 236.9592, tolerance = 0.0001)
})

test_that("setorigin set t_0 as 0 for extVB", {
  length_0_2 <- ext_vb(age = 2, ps = c(300, 0.5, 0.6, 3), setorigin = TRUE)
  expect_equal(length_0_2, 189.6362, tolerance = 0.0001)

  length_0_4 <- ext_vb(age = 4, ps = c(300, 0.5, 0.6, 3), setorigin = TRUE)
  expect_equal(length_0_4, 245.1949, tolerance = 0.0001)
})
