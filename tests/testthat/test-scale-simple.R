
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

test_that("ScaleSimpleContinuous limits can be set", {
  scale <- ScaleSimpleContinuous$new()$set_limits(c(1, 10))
  expect_equal(scale$limits(), c(1, 10))
})

test_that("ScaleSimpleContinuous limits are returned in transformed space", {
  scale <- ScaleSimpleContinuous$
    new()$
    set_trans(scales::log10_trans())$
    set_limits(c(1, 10))
  expect_equal(scale$limits(), log10(c(1, 10)))
})

test_that("ScaleSimpleContinuous breaks are the trans breaks by default", {
  scale <- ScaleSimpleContinuous$
    new()$
    set_limits(c(1, 10))
  expect_equal(scale$breaks(), scales::extended_breaks()(c(1, 10)))

  scale$set_trans(scales::log10_trans())
  expect_equal(scale$breaks(), log10(scales::log_breaks(base = 10)(c(1, 10))))
})

test_that("ScaleSimpleContinuous doesn't change values by default", {
  scale <- ScaleSimpleContinuous$new()
  expect_equal(scale$map(c(NA, 1:10)), c(NA, 1:10))
})

test_that("ScaleSimpleContinuous can rescale values", {
  scale <- ScaleSimpleContinuous$
    new()$
    set_rescaler(scales::rescale)$
    set_limits(c(1, 10))

  expect_equal(scale$map(c(NA, 1:10)), scales::rescale(c(NA, 1:10), from = c(1, 10)))
})

test_that("ScaleSimpleContinuous can censor values", {
  scale <- ScaleSimpleContinuous$
    new()$
    set_oob(scales::censor)$
    set_limits(c(2, 9))

  expect_equal(scale$map(c(NA, 1:10)), c(NA, NA, 2:9, NA))
})

test_that("ScaleSimpleContinuous can set the NA value", {
  scale <- ScaleSimpleContinuous$new()$set_na_value(124)
  expect_equal(scale$map(c(NA, 1:10)), c(124, 1:10))
})

test_that("ScaleSimpleContinuous always has finite limits", {
  scale <- ScaleSimpleContinuous$new()
  expect_length(scale$limits(), 2)
  expect_true(all(is.finite(scale$limits())))
})
