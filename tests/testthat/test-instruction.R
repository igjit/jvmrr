test_that("instruction_of works", {
  expect_equal(instruction_of(18)$name, "ldc")
})
