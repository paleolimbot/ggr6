
test_that("FacetNull creates one panel with all the data", {
  coord <- CoordIdentity$new()
  scales <- ScaleList$new()
  facet <- FacetNull$new()
  df <- tibble(x = 1, y = 2)

  facet$train(df)
  facet$init_panels(coord, scales)
  expect_length(facet$panel_data_all(df), 1)
  expect_identical(facet$panel_data_all(df)[[1]], df)
})
