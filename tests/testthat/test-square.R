test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  expect_equal(-3 * -3, 9)
  expect_equal(c(1,2) * c(1,2), c(1,4))
})
