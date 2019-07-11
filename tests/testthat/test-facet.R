
test_that("FacetNull creates one panel with all the data", {
  coord <- CoordIdentity$new()
  scales <- ScaleList$new()
  facet <- FacetNull$new()
  df <- tibble(x = 1, y = 2)

  expect_length(facet$panels(coord, scales), 1)
  expect_identical(facet$panel_data(df, Panel$new()), df)
})
