
test_that("scale list only accepts scales", {
  scale_list <- ScaleList$new()
  expect_error(scale_list$add(NULL), "Scale instance")
  expect_identical(scale_list$add(ScaleIdentity$new()), scale_list)
})

test_that("ScaleIdentity does not change input", {
  scale <- ScaleIdentity$new()
  expect_identical(scale$transform(1:5), 1:5)
  expect_identical(scale$map(1:5), 1:5)
  expect_identical(scale$train(1:5), scale)
})
