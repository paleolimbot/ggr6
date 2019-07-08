
test_that("ScaleSimpleContinuous can be trained", {
  scale <- ScaleSimpleContinuous$new()
  expect_equal(scale$trained_range(), NULL)
  scale$train(1:10)
  expect_equal(scale$trained_range(), c(1, 10))
  expect_equal(scale$limits(), c(1, 10))
})

test_that("ScaleSimpleContinuous transforms values", {
  scale <- ScaleSimpleContinuous$new()$set_trans(scales::log10_trans())
  expect_equal(scale$transform(1:10), log10(1:10))
})
