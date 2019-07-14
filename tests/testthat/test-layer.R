
test_that("layer list only accepts layers", {
  layer_list <- LayerList$new()
  expect_error(layer_list$add(NULL), class = "bad_r6_type")
  expect_identical(layer_list$add(Layer$new()), layer_list)
})
