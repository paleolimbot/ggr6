
test_that("ScaleSimpleDiscrete can be trained", {
  scale <- ScaleSimpleDiscrete$new()
  expect_equal(scale$trained_range(), NULL)
  expect_true(scale$is_empty())
  scale$train(c("a", "b", "c"))
  expect_equal(scale$trained_range(), c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
})

test_that("ScaleSimpleDiscrete can be trained with drop = FALSE", {
  scale <- ScaleSimpleDiscrete$new()$set_drop(FALSE)
  expect_equal(scale$trained_range(), NULL)
  expect_true(scale$is_empty())
  scale$train(factor(character(0), levels = c("a", "b", "c")))
  expect_equal(scale$trained_range(), c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
})

test_that("ScaleSimpleDiscrete transforms values", {
  scale <- ScaleSimpleDiscrete$new()$set_trans(s3_trans("new_class"))
  expect_equal(unclass(scale$transform(c("a", "b", "c"))), c("a", "b", "c"))
  expect_is(scale$transform(c("a", "b", "c")), "new_class")
})

test_that("ScaleSimpleDiscrete limits can be set", {
  scale <- ScaleSimpleDiscrete$new()$set_limits(c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
})

test_that("ScaleSimpleDiscrete limits are returned in transformed space", {
  scale <- ScaleSimpleDiscrete$
    new()$
    set_trans(s3_trans(class_out = "out_class"))$
    set_limits(c("a", "b"))
  expect_equal(unclass(scale$limits()), c("a", "b"))
  expect_is(scale$limits(), "out_class")
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
    set_palette_factory(scales::hue_pal())$
    set_limits(c("a", "b", "c"))

  expect_equal(scale$map(c("a", "b", "c", NA)), c(scales::hue_pal()(3), NA))
})

test_that("ScaleSimpleDiscrete can censor values", {
  scale <- ScaleSimpleDiscrete$
    new()$
    set_oob(censor_discrete)$
    set_limits(c("a", "b"))

  expect_equal(scale$map(c("a", "b", "c", NA)), c("a", "b", NA, NA))
})

test_that("ScaleSimpleDiscrete can set the NA value", {
  scale <- ScaleSimpleDiscrete$new()$set_na_value("fishyfishyfishy")
  expect_equal(scale$map(c(NA, "a", "b")), c("fishyfishyfishy", "a", "b"))
})

test_that("ScaleSimpleDiscrete can have a custom range set", {
  NullRange <- R6Class(
    "NullRange", inherit = scales::DiscreteRange,
    public = list(
      train = function(x, drop = TRUE) {
        # do nothing
      }
    )
  )

  scale <- ScaleSimpleDiscrete$
    new()$
    set_range(NullRange$new())

  scale$train(c("a", "b", "c"))
  expect_identical(scale$trained_range(), NULL)
})

test_that("ScaleSimpleDiscrete can transform, train, and map tbls", {
  scale <- ScaleSimpleDiscrete$new(aesthetics = "x")$
    set_trans(s3_trans("new_class"))$
    set_palette_factory(scales::hue_pal())

  tbl <- tibble(x = c("a", "b", "c"), y = c(10, 20, 30))
  tbl_trans <- tibble(x = structure(c("a", "b", "c"), class = "new_class"), y = c(10, 20, 30))
  tbl_map <- tibble(x = scales::hue_pal()(3), y = c(10, 20, 30))

  expect_identical(scale$transform_tbl(tbl), tbl_trans)

  expect_identical(scale$trained_range(), NULL)
  scale$train_tbl(tbl_trans)
  expect_identical(scale$trained_range(), tbl$x)
  expect_identical(scale$map_tbl(tbl_trans), tbl_map)
})
