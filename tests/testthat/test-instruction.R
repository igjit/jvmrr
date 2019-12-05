test_that("instruction_of works", {
  expect_equal(instruction_of(18)$name, "ldc")
})

test_that("with_op works", {
  n <- 4
  expect_equal(with_op(iinc, 2, n), c(132, 2, 4))
})
