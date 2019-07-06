
test_that("FacetNull creates one panel with all the data", {
  coord <- CoordIdentity$new()
  scales <- ScaleList$new()
  facet <- FacetNull$new()
  df <- tibble(x = 1, y = 2)

  facet$train(df)
  facet$init_panels(coord, scales)
  expect_length(facet$panel_data(df), 1)
  expect_identical(facet$panel_data(df)[[1]], df)
})

test_that("FacetNull cannot create panel data before panels are initiated", {
  facet <- FacetNull$new()
  df <- tibble(x = 1, y = 2)
  expect_error(facet$panel_data(df), "Attempting to call")
})
