
test_that("discrete position scales can be trained using discrete and continuous values", {
  scale <- ScaleSimpleDiscretePosition$new()
  expect_true(scale$is_empty())

  scale$train(1:5)
  expect_false(scale$is_empty())
  expect_equal(scale$limits_continuous(), c(1, 5))

  scale$reset()
  expect_true(scale$is_empty())

  scale$train(c("a", "b", "c"))
  expect_equal(scale$limits_continuous(), c(1, 3))

  scale$train(2:7)
  expect_equal(scale$limits_continuous(), c(1, 7))

  scale$set_limits_continuous(c(12, 82))
  expect_equal(scale$limits_continuous(), c(12, 82))
})

test_that("discrete position scales can map discrete and continuous values", {
  scale <- ScaleSimpleDiscretePosition$new()
  scale$train(c("a", "b", "c"))
  expect_identical(scale$map(c("a", "b", "c")), c(1L, 2L, 3L))
  expect_identical(scale$map(c(1L, 2L, 3L)), c(1L, 2L, 3L))
  expect_identical(scale$map(as.numeric(1:3)), as.numeric(1:3))
})

test_that("continuous position scales can be trained", {
  scale <- ScaleSimpleContinuousPosition$new()
  expect_true(scale$is_empty())

  scale$train(1:5)
  expect_false(scale$is_empty())
  expect_equal(scale$limits_continuous(), c(1, 5))

  scale$set_limits_continuous(c(12, 82))
  expect_equal(scale$limits_continuous(), c(12, 82))
})

test_that("continuous position scales can map values", {
  scale <- ScaleSimpleContinuousPosition$new()
  expect_identical(scale$map(c(1L, 2L, 3L)), c(1, 2, 3))
  expect_identical(scale$map(as.numeric(1:3)), as.numeric(1:3))
})
