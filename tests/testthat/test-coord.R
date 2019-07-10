
test_that("CoordIdentity does not change position values", {
  df <- tibble(x = 1, y = 2)
  coord <- CoordIdentity$new(aesthetics = c("x", "y"))
  scales <- ScaleList$new()
  panel <- coord$setup_panel(scales)
  expect_identical(panel$transform(df), df)
})
