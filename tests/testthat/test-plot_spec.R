
test_that("PlotSpec objects can be initiated without arguments", {
  expect_true(is.R6(PlotSpec$new()))
})
