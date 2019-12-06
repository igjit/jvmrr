test_that("read_class works", {
  file <- jvmrr_example("Hello.class")
  cls <- read_class(file)
  expect_equal(cls$magic, as.raw(c(0xca, 0xfe, 0xba, 0xbe)))
  expect_equal(cls$this_class_name, "Hello")
  expect_equal(cls$super_class_name, "java/lang/Object")
})
