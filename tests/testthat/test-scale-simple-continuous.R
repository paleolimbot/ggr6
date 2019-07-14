
test_that("ScaleSimpleContinuous can be trained", {
  scale <- ScaleSimpleContinuous$new()
  expect_equal(scale$trained_range(), NULL)
  expect_true(scale$is_empty())
  scale$train(1:10)
  expect_equal(scale$trained_range(), c(1, 10))
  expect_equal(scale$limits(), c(1, 10))
  expect_false(scale$is_empty())
  scale$reset()
  expect_true(scale$is_empty())
})

test_that("ScaleSimpleContinuous transforms values", {
  scale <- ScaleSimpleContinuous$new()$set_trans(scales::log10_trans())
  expect_equal(scale$transform(1:10), log10(1:10))
})

test_that("ScaleSimpleContinuous limits can be set", {
  scale <- ScaleSimpleContinuous$new()$set_limits(c(1, 10))
  expect_equal(scale$limits(), c(1, 10))
  expect_false(scale$is_empty())
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
  expect_identical(
    scale$breaks_minor(),
    scales::regular_minor_breaks()(scale$breaks(), scale$limits(), 2)
  )

  scale$set_trans(scales::log10_trans())
  expect_equal(scale$breaks(), log10(scales::log_breaks(base = 10)(c(1, 10))))
  expect_identical(
    scale$breaks_minor(),
    log10(
      scales::regular_minor_breaks()(
        10^scale$breaks(),
        10^scale$limits(),
        2
      )
    )
  )
})

test_that("ScaleSimpleContinuous labels are the trans labels by default", {
  trans_identity2 <- scales::identity_trans()
  trans_identity2$format <- function(breaks) paste("label is", breaks)
  scale <- ScaleSimpleContinuous$new()$
    set_trans(trans_identity2)$
    set_breaks(c(1, 2, 3))

  expect_equal(scale$labels(), c("label is 1", "label is 2", "label is 3"))
})

test_that("ScaleSimpleContinuous breaks and labels can be set manually", {
  scale <- ScaleSimpleContinuous$new()$
    set_breaks(c(1, 2))$
    set_labels(c("a", "b"))$
    set_breaks_minor(1.5)

  expect_identical(scale$breaks(), c(1, 2))
  expect_identical(scale$labels(), c("a", "b"))
  expect_identical(scale$breaks_minor(), 1.5)
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

test_that("ScaleSimpleContinuous can map continuous values to character output", {
  colour_pal <- scales::gradient_n_pal(c("#000000", "#FFFFFF"))
  scale <- ScaleSimpleContinuous$
    new()$
    set_palette(colour_pal)$
    set_rescaler(scales::rescale)$
    set_limits(c(1, 10))$
    set_na_value("#121212")

  expect_equal(scale$map(c(1, 10, NA)), c("#000000", "#FFFFFF", "#121212"))
})

test_that("ScaleSimpleContinuous can have a custom range set", {
  NullRange <- R6Class(
    "NullRange", inherit = scales::ContinuousRange,
    public = list(
      train = function(x) {
        # do nothing
      }
    )
  )

  scale <- ScaleSimpleContinuous$
    new()$
    set_range(NullRange$new())

  scale$train(1:10)
  expect_identical(scale$trained_range(), NULL)
})

test_that("ScaleSimpleContinuous can transform, train, and map tbls", {
  colour_pal <- scales::gradient_n_pal(c("#000000", "#FFFFFF"))
  scale <- ScaleSimpleContinuous$new(aesthetics = "x")$
    set_trans(scales::log10_trans())$
    set_rescaler(scales::rescale)$
    set_palette(colour_pal)

  tbl <- tibble(x = c(1, 5), y = c(10, 20))
  tbl_trans <- tibble(x = log10(c(1, 5)), y = c(10, 20))
  tbl_map <- tibble(x = c("#000000", "#FFFFFF"), y = c(10, 20))

  expect_identical(scale$transform_tbl(tbl), tbl_trans)
  # there is a tiny rounding error that keeps these from being identical
  expect_identical(round(scale$untransform_tbl(tbl_trans)), tbl)

  expect_identical(scale$trained_range(), NULL)
  scale$train_tbl(tbl_trans)
  expect_identical(scale$trained_range(), log10(c(1, 5)))

  expect_identical(scale$map_tbl(tbl_trans), tbl_map)
})
