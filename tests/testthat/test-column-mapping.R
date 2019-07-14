
test_that("identity column mappings can map columns", {
  tbl <- tibble(fish = 1, whistle = "two", thinger = 3)
  mapping <- ColumnMappingIdentity$new(tbl)

  expect_identical(mapping$map_data(tbl), tbl)
  expect_identical(mapping$map_data_stat(tbl), tbl)
  expect_identical(mapping$aesthetics(), names(tbl))
})

test_that("quosure column mappings can map columns", {
  tbl <- tibble(fish = 1, whistle = "two", thinger = 3)
  mapping <- ColumnMappingQuosure$new(x = fish, y = whistle, z = stat(thinger))

  expect_identical(mapping$map_data(tbl), tibble(x = 1, y = "two"))
  expect_identical(mapping$map_data_stat(tbl), tibble(fish = 1, whistle = "two", thinger = 3, z = 3))
  expect_identical(mapping$aesthetics(), c("x", "y", "z"))
})

test_that("quosure column mapping throws an error when quosures aren't named", {
  expect_error(ColumnMappingQuosure$new(not_named), "must be named")
})
