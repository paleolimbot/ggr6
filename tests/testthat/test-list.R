
test_that("mutable lists work as intended", {
  lst <- List$new()
  expect_equal(lst$size(), 0)
  expect_silent(lst$add(1))
  expect_equal(lst$size(), 1)
  expect_identical(lst$get(1), 1)
})
