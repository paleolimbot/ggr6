
test_that("ScaleDiscrete has character(0) limits when empty", {
  scale <- ScaleDiscrete$new()
  expect_true(scale$is_empty())
  expect_identical(scale$limits(), character(0))
  expect_identical(scale$breaks(), character(0))
  expect_identical(scale$labels(), character(0))
})

test_that("ScaleDiscrete can be trained", {
  scale <- ScaleDiscrete$new()
  expect_equal(scale$trained_range(), NULL)
  expect_true(scale$is_empty())
  scale$train(c("a", "b", "c"))
  expect_equal(scale$trained_range(), c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
  scale$reset()
  expect_true(scale$is_empty())
})

test_that("ScaleDiscrete can be trained with drop = FALSE", {
  scale <- ScaleDiscrete$new()$set_drop(FALSE)
  expect_equal(scale$trained_range(), NULL)
  expect_true(scale$is_empty())
  scale$train(factor(character(0), levels = c("a", "b", "c")))
  expect_equal(scale$trained_range(), c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
})

test_that("ScaleDiscrete transforms values", {
  scale <- ScaleDiscrete$new()$set_trans(s3_trans("new_class"))
  expect_equal(unclass(scale$transform(c("a", "b", "c"))), c("a", "b", "c"))
  expect_is(scale$transform(c("a", "b", "c")), "new_class")
})

test_that("ScaleDiscrete limits can be set", {
  scale <- ScaleDiscrete$new()$set_limits(c("a", "b", "c"))
  expect_equal(scale$limits(), c("a", "b", "c"))
  expect_false(scale$is_empty())
})

test_that("ScaleDiscrete limits are returned in transformed space", {
  scale <- ScaleDiscrete$
    new()$
    set_trans(s3_trans(class_out = "out_class"))$
    set_limits(c("a", "b"))
  expect_equal(unclass(scale$limits()), c("a", "b"))
  expect_is(scale$limits(), "out_class")
})

test_that("ScaleDiscrete breaks are the trans breaks by default", {
  trans <- trans_discrete_new(
    "test",
    transform = force, inverse = force,
    breaks = rev,
    minor_breaks = function(breaks, limits, n) breaks
  )

  scale <- ScaleDiscrete$
    new()$
    set_trans(trans)$
    set_limits(c("a", "b", "c"))

  expect_equal(scale$breaks(), c("c", "b", "a"))
  expect_equal(scale$breaks_minor(), c("c", "b", "a"))
})

test_that("ScaleDiscrete labels are the trans labels by default", {
  trans <- trans_discrete_new(
    "test",
    transform = force, inverse = force,
    format = function(breaks) paste0("*", breaks, "*")
  )

  scale <- ScaleDiscrete$
    new()$
    set_trans(trans)$
    set_limits(c("a", "b", "c"))

  expect_identical(scale$labels(), c("*a*", "*b*", "*c*"))
})

test_that("ScaleDiscrete doesn't change values by default", {
  scale <- ScaleDiscrete$new()
  expect_equal(scale$map(c("a", "b", "c")), c("a", "b", "c"))
})

test_that("ScaleDiscrete can map values to character output", {
  scale <- ScaleDiscrete$
    new()$
    set_palette_factory(scales::hue_pal())$
    set_limits(c("a", "b", "c"))

  expect_equal(scale$map(c("a", "b", "c", NA)), c(scales::hue_pal()(3), NA))
})

test_that("ScaleDiscrete can censor values", {
  scale <- ScaleDiscrete$
    new()$
    set_oob(censor_discrete)$
    set_limits(c("a", "b"))

  expect_equal(scale$map(c("a", "b", "c", NA)), c("a", "b", NA, NA))
})

test_that("ScaleDiscrete can set the NA value", {
  scale <- ScaleDiscrete$new()$set_na_value("fishyfishyfishy")
  expect_equal(scale$map(c(NA, "a", "b")), c("fishyfishyfishy", "a", "b"))
})

test_that("ScaleDiscrete can have a custom range set", {
  NullRange <- R6Class(
    "NullRange", inherit = scales::DiscreteRange,
    public = list(
      train = function(x, drop = TRUE) {
        # do nothing
      }
    )
  )

  scale <- ScaleDiscrete$
    new()$
    set_range(NullRange$new())

  scale$train(c("a", "b", "c"))
  expect_identical(scale$trained_range(), NULL)
})

test_that("ScaleDiscrete can transform, train, and map tbls", {
  scale <- ScaleDiscrete$new(aesthetics = "x")$
    set_trans(s3_trans("new_class"))$
    set_palette_factory(scales::hue_pal())

  tbl <- tibble(x = c("a", "b", "c"), y = c(10, 20, 30))
  tbl_trans <- tibble(x = structure(c("a", "b", "c"), class = "new_class"), y = c(10, 20, 30))
  tbl_map <- tibble(x = scales::hue_pal()(3), y = c(10, 20, 30))

  expect_identical(scale$transform_tbl(tbl), tbl_trans)
  expect_identical(scale$untransform_tbl(tbl_trans), tbl)

  expect_identical(scale$trained_range(), NULL)
  scale$train_tbl(tbl_trans)
  expect_identical(scale$trained_range(), tbl$x)
  expect_identical(scale$map_tbl(tbl_trans), tbl_map)
})
