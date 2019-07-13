
test_that("PositionIdentity returns identical data", {
  df <- tibble(x = 1)
  expect_identical(df, PositionIdentity$new()$compute_panel(df))
})
