
test_that("scale list only accepts scales", {
  scale_list <- ScaleList$new()
  expect_error(scale_list$add(NULL), "Scale instance")
  expect_identical(scale_list$add(ScaleNull$new()), scale_list)
})

test_that("ScaleNull does not change input", {
  scale <- ScaleNull$new()
  df <- tibble(x = 1:5)
  expect_identical(scale$transform_tbl(df), df)
  expect_identical(scale$map_tbl(df), df)
  expect_identical(scale$train_tbl(df), scale)
})

test_that("ScaleNull has NULL breaks, minor breaks, labels, and infinite limits", {
  scale <- ScaleNull$new()
  expect_null(scale$breaks())
  expect_null(scale$breaks_minor())
  expect_null(scale$labels())
  expect_identical(scale$limits(), c(-Inf, Inf))
})
