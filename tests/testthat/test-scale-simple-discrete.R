
test_that("ScaleSimpleDiscrete can be trained", {
  scale <- ScaleSimpleDiscrete$new()
  expect_equal(scale$trained_range(), NULL)
  expect_true(scale$is_empty())
  scale$train(c("a", "b", "c"))
  expect_equal(scale$trained_range(), c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
})

test_that("ScaleSimpleDiscrete transforms values", {
  scale <- ScaleSimpleDiscrete$new()$set_trans(discrete_rev_trans())
  expect_equal(scale$transform(c("a", "b", "c")), c("c", "b", "a"))
})

test_that("ScaleSimpleDiscrete limits can be set", {
  scale <- ScaleSimpleDiscrete$new()$set_limits(c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
})

test_that("ScaleSimpleDiscrete limits are returned in transformed space", {
  scale <- ScaleSimpleDiscrete$
    new()$
    set_trans(discrete_rev_trans())$
    set_limits(c("a", "b"))
  expect_equal(scale$limits(), c("b", "a"))
})

test_that("ScaleSimpleDiscrete breaks are the trans breaks by default", {
  trans <- trans_discrete_new(
    "test",
    transform = force, inverse = force,
    breaks = rev,
    minor_breaks = function(breaks, limits, n) breaks
  )

  scale <- ScaleSimpleDiscrete$
    new()$
    set_trans(trans)$
    set_limits(c("a", "b", "c"))

  expect_equal(scale$breaks(), c("c", "b", "a"))
  expect_equal(scale$breaks_minor(), c("c", "b", "a"))
})

test_that("ScaleSimpleDiscrete labels are the trans labels by default", {
  trans <- trans_discrete_new(
    "test",
    transform = force, inverse = force,
    format = function(breaks) paste0("*", breaks, "*")
  )

  scale <- ScaleSimpleDiscrete$
    new()$
    set_trans(trans)$
    set_limits(c("a", "b", "c"))

  expect_identical(scale$labels(), c("*a*", "*b*", "*c*"))
})

test_that("ScaleSimpleDiscrete doesn't change values by default", {
  scale <- ScaleSimpleDiscrete$new()
  expect_equal(scale$map(c("a", "b", "c")), c("a", "b", "c"))
})

test_that("ScaleSimpleDiscrete can map values to character output", {
  scale <- ScaleSimpleDiscrete$
    new()$
    set_palette(scales::hue_pal())

  expect_equal(scale$map(c("a", "b", "c")), scales::hue_pal()(3))
})
