
test_that("scale list only accepts scales", {
  scale_list <- ScaleList$new()
  expect_error(scale_list$add(NULL), "Scale instance")
  expect_identical(scale_list$add(ScaleIdentity$new()), scale_list)
})

test_that("ScaleIdentity does not change input", {
  scale <- ScaleIdentity$new()
  df <- tibble(x = 1:5)
  expect_identical(scale$transform_tbl(df), df)
  expect_identical(scale$map_tbl(df), df)
  expect_identical(scale$train_tbl(df), scale)
})
