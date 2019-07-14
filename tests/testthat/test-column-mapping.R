
test_that("identity column mappings can map columns", {
  tbl <- tibble(fish = 1, whistle = "two", thinger = 3)
  mapping <- ColumnMappingIdentity$new(tbl)

  expect_identical(mapping$map(tbl), tbl)
  expect_identical(mapping$map_new(tbl), tbl)
  expect_identical(mapping$map_update(tbl), tbl)
  expect_identical(mapping$aesthetics(), names(tbl))
})

test_that("quosure column mappings can map columns", {
  tbl <- tibble(fish = 1, whistle = "two", thinger = 3)
  tbl2 <- tibble(x = "the x", y = "the y")
  mapping <- ColumnMappingQuosure$new(x = fish, y = whistle, z = 10)

  expect_identical(mapping$map(tbl), tibble(x = 1, y = "two", z = 10))
  expect_identical(mapping$map_update(tbl), dplyr::bind_cols(tbl, tibble(x = 1, y = "two", z = 10)))
  expect_identical(mapping$map_new(tbl2), tibble(x = "the x", y = "the y", z = 10))
  expect_identical(mapping$aesthetics(), c("x", "y", "z"))
})

test_that("quosure column mapping throws an error when quosures aren't named", {
  expect_error(ColumnMappingQuosure$new(not_named), "must be named")
})
