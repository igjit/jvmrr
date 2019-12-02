test_that("read_operation works", {
  code <- c(18, 1, 178, 0, 2)
  env <- rlang::env(pc = 1)

  op <- read_operation(code, env)
  expect_equal(op, operation(18, 1))
  expect_equal(env$pc, 3)

  op <- read_operation(code, env)
  expect_equal(op, operation(178, c(0, 2)))
  expect_equal(env$pc, 6)
})
