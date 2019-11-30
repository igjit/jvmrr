test_that("read_class works", {
  file <- system.file("java/Hello.class", package = "jvmrr")
  cls <- read_class(file)
  expect_equal(cls$magic, as.raw(c(0xca, 0xfe, 0xba, 0xbe)))
})

test_that("name_lookup works", {
  name_of <- name_lookup(c(a = 11, b = 12))
  expect_equal(name_of(12), "b")
})
