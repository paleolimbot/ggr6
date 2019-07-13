
test_that("quosure column mappings can map columns", {
  tbl <- tibble(fish = 1, whistle = "two", thinger = 3)
  mapping <- ColumnMappingQuosure$new(x = fish, y = whistle, z = stat(thinger))

  expect_identical(mapping$map_data(tbl), tibble(x = 1, y = "two"))
  expect_identical(mapping$map_data_stat(tbl), tibble(ish = 1, whistle = "two", z = 3))
  expect_identical(mapping$aesthetics(), c("x", "y", "z"))
})
