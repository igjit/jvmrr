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

test_that("iconst_<i> works", {
  env <- rlang::env(pc = 1, stack = stack(), frame = list())
  op <- read_operation(with_op(iconst_2), env)
  execute_operation(op, NULL, env)
  expect_equal(as.list(env$stack), list(2))
})

test_that("istore_<n> works", {
  env <- rlang::env(pc = 1, stack = as.stack(2), frame = list())
  op <- read_operation(with_op(istore_1), env)
  execute_operation(op, NULL, env)
  expect_equal(env$frame, list(2))
})

test_that("iload_<n> works", {
  env <- rlang::env(pc = 1, stack = stack(), frame = list(2))
  op <- read_operation(with_op(iload_1), env)
  execute_operation(op, NULL, env)
  expect_equal(as.list(env$stack), list(2))
})

test_that("isub works", {
  env <- rlang::env(pc = 1, stack = as.stack(list(4, 10)), frame = list())
  op <- read_operation(with_op(isub), env)
  execute_operation(op, NULL, env)
  expect_equal(as.list(env$stack), list(6))
})

test_that("iinc works", {
  env <- rlang::env(pc = 1, stack = stack(), frame = list(10, 20))
  op <- read_operation(with_op(iinc, 2, 4), env)
  execute_operation(op, NULL, env)
  expect_equal(env$frame, list(10, 24))
})

test_that("if_icmp<cond> works", {
  env <- rlang::env(pc = 1, stack = as.stack(list(3, 2)), frame = list())
  op <- read_operation(with_op(if_icmplt, 0, 10), env)
  execute_operation(op, NULL, env)
  expect_equal(env$pc, 11)

  env <- rlang::env(pc = 1, stack = as.stack(list(2, 3)), frame = list())
  op <- read_operation(with_op(if_icmplt, 0, 10), env)
  execute_operation(op, NULL, env)
  expect_equal(env$pc, 4)
})

test_that("if<cond> works", {
  env <- rlang::env(pc = 1, stack = as.stack(0), frame = list())
  op <- read_operation(with_op(ifeq, 0, 10), env)
  execute_operation(op, NULL, env)
  expect_equal(env$pc, 11)

  env <- rlang::env(pc = 1, stack = as.stack(1), frame = list())
  op <- read_operation(with_op(ifeq, 0, 10), env)
  execute_operation(op, NULL, env)
  expect_equal(env$pc, 4)
})

test_that("goto works", {
  env <- rlang::env(pc = 1, stack = stack(), frame = list())
  op <- read_operation(with_op(goto, 0, 10), env)
  execute_operation(op, NULL, env)
  expect_equal(env$pc, 11)
})

test_that("execute works", {
  file <- jvmrr_example("Hello.class")
  class <- read_class(file)
  expect_output(execute(class), "Hello, world.")

  file <- jvmrr_example("Arith.class")
  class <- read_class(file)
  expect_output(execute(class), "42")

  file <- jvmrr_example("FizzBuzz.class")
  class <- read_class(file)
  i <- 1:20
  fizzbuzz <- ifelse(i %% 15 == 0 , "FizzBuzz",
              ifelse(i %% 3 == 0, "Fizz",
              ifelse(i %% 5 == 0, "Buzz", i)))
  expect_output(execute(class), paste(fizzbuzz, collapse = "\n"))
})
