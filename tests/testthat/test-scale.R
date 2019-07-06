
test_that("scale list only accepts scales", {
  scale_list <- ScaleList$new()
  expect_error(scale_list$add(NULL), "Scale instance")
  expect_identical(scale_list$add(ScaleIdentity$new()), scale_list)
})
