
test_that("ScaleSimpleDataFrame behaves as expected", {
  scale <- ScaleSimpleDataFrame$new("aesthetic")
  expect_true(scale$is_empty())
  expect_identical(scale$limits(), tibble())
  expect_identical(scale$breaks(), tibble())
  expect_identical(scale$labels(), tibble())
  expect_error(scale$train(1:5), "Non-data frame supplied")

  scale$train(tibble(x = c(1, 1), y = c(2, 2)))
  expect_identical(scale$limits(), tibble(x = 1, y = 2))
  expect_identical(scale$breaks(), tibble(x = 1, y = 2))
})

test_that("data frame range can be trained", {
  range <- RangeDataFrame$new()
  expect_null(range$range)
  range$train(tibble(x = c(1, 1), y = c(2, 2)))
  expect_identical(range$range, tibble(x = 1, y = 2))
  range$train(tibble(x = 3, y = 2))
  expect_identical(range$range, tibble(x = c(1, 3), y = c(2, 2)))
  range$reset()
  expect_null(range$range)
})

test_that("data frame range can be trained with drop = FALSE", {
  range <- RangeDataFrame$new()
  expect_null(range$range)
  range$train(tibble(x = factor(c("a", "a"), levels = c("a", "b")), y = c(2, 2)), drop = FALSE)
  expect_identical(range$range, tibble(x = factor(c("a", "b")), y = c(2, NA)))
})
