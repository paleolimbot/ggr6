
test_that("StatIdentity returns identical data", {
  df <- tibble(x = 1)
  expect_identical(df, StatIdentity$new()$compute_panel(df, Panel$new(), PlotRendererIdentity$new()))
})
