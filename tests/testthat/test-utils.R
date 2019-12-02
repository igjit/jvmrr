test_that("name_lookup works", {
  name_of <- name_lookup(c(a = 11, b = 12))
  expect_equal(name_of(12), "b")
})
