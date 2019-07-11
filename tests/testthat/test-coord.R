
test_that("CoordIdentity does not change position values", {
  df <- tibble(x = 1, y = 2, z = 3)
  coord <- CoordIdentity$new()
  scales <- ScaleList$new()

  expect_identical(coord$transform(df, scales), df)
})
