
test_that("layer list only accepts layers", {
  layer_list <- LayerList$new()
  expect_error(layer_list$add(NULL), "Layer instance")
  expect_identical(layer_list$add(Layer$new()), layer_list)
})
