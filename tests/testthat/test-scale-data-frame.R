
test_that("ScaleDataFrame behaves as expected", {
  scale <- ScaleDataFrame$new("aesthetic")
  expect_true(scale$is_empty())
  expect_identical(scale$limits(), tibble())
  expect_identical(scale$breaks(), tibble())
  expect_identical(scale$labels(), tibble())
  expect_error(scale$train(1:5), "Non-data frame supplied")

  scale$train(tibble(x = c(1, 1), y = c(2, 2)))
  expect_identical(scale$limits(), tibble(x = 1, y = 2))
  expect_identical(scale$breaks(), tibble(x = 1, y = 2))
})

test_that("within_limits() works for a data frame scale", {
  tbl1 <- tibble(x = c(1, 2), y = c(1, 2))
  tbl2 <- tibble(x = c(2, 3), y = c(2, 3))

  scale <- ScaleDataFrame$new()
  scale$set_limits(tbl1)
  expect_identical(scale$within_limits(tbl2), c(TRUE, FALSE))
})

test_that("within_limits() returns FALSE when the data frame scale is empty", {
  scale <- ScaleDataFrame$new()
  expect_identical(scale$within_limits(tibble(x = c(2, 3), y = c(2, 3))), c(FALSE, FALSE))
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
