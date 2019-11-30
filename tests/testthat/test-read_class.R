test_that("read_class works", {
  file <- system.file("java/Hello.class", package = "jvmrr")
  cls <- read_class(file)
  expect_equal(cls$magic, as.raw(c(0xca, 0xfe, 0xba, 0xbe)))
})
