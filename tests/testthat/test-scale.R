
test_that("scale default name is the first aesthetic", {
  scale <- Scale$new()
  expect_identical(scale$name(), NA_character_)

  scale <- Scale$new(c("x", "xend"))
  expect_identical(scale$name(), "x")
})

test_that("scale list only accepts scales", {
  scale_list <- ScaleList$new()
  expect_error(scale_list$add(NULL), class = "bad_r6_type")
  expect_identical(scale_list$add(ScaleNull$new()), scale_list)
})

test_that("ScaleNull does not change input", {
  scale <- ScaleNull$new()
  df <- tibble(x = 1:5)
  expect_identical(scale$transform_tbl(df), df)
  expect_identical(scale$untransform_tbl(df), df)
  expect_identical(scale$map_tbl(df), df)
  expect_identical(scale$train_tbl(df), scale)

  expect_identical(scale$transform(1:5), 1:5)
  expect_identical(scale$untransform(1:5), 1:5)
  expect_identical(scale$map(1:5), 1:5)
  expect_identical(scale$train(1:5), scale)
})

test_that("ScaleNull has NULL breaks, minor breaks, labels, and infinite limits", {
  scale <- ScaleNull$new()
  expect_null(scale$breaks())
  expect_null(scale$breaks_minor())
  expect_null(scale$labels())
  expect_identical(scale$limits(), c(-Inf, Inf))
  expect_true(all(scale$within_limits(1:100)))
})

test_that("ScaleNull can be reset", {
  scale <- ScaleNull$new()
  expect_identical(scale$reset(), scale)
})

test_that("scale list methods work as expected", {

  scale_x <- ScaleContinuous$new("x")$set_trans(scales::log10_trans())
  scale_y <- ScaleContinuous$new("y")$set_trans(scales::log10_trans())
  scales <- ScaleList$new()$add(scale_x)$add(scale_y)

  expect_identical(scales$scale("x"), scale_x)
  expect_identical(scales$scale("y"), scale_y)
  expect_null(scales$filter_by_aesthetics("x")$scale("y"))
  expect_null(scales$filter_by_aesthetics("y")$scale("x"))
  expect_null(scales$discard_by_aesthetics("x")$scale("x"))
  expect_null(scales$discard_by_aesthetics("y")$scale("y"))

  guides <- scales$guides()
  expect_is(guides, "GuideList")
  expect_equal(guides$size(), scales$size())

  tbl <- tibble(x = 10, y = 10, z = 10)
  tbl_trans <- tibble(x = 1, y = 1, z = 10)

  expect_identical(scales$transform_tbl(tbl), tbl_trans)
  expect_identical(scales$untransform_tbl(tbl_trans), tbl)
  expect_identical(scales$map_tbl(tbl_trans), tbl_trans)

  expect_true(scale_x$is_empty())
  expect_true(scale_y$is_empty())

  scales$train_tbl(tbl_trans)

  expect_false(scale_x$is_empty())
  expect_false(scale_y$is_empty())
  expect_identical(scale_x$limits(), c(1, 1))
  expect_identical(scale_y$limits(), c(1, 1))

  scales$reset()

  expect_true(scale_x$is_empty())
  expect_true(scale_y$is_empty())

  expect_identical(scales$aesthetics(), c("x", "y"))
  scales$add_missing(tbl, RendererIdentity$new())
  expect_identical(scales$aesthetics(), c("x", "y", "z"))
})
