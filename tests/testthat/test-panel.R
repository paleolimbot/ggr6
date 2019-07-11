
test_that("Panel object can transform data based on the coord", {
  CoordLogLogIdentity <- R6Class(
    "CoordLogLogIdentity", inherit = Coord,
    public = list(
      transform = function(data_mapped, scales) {
        cols <- intersect(colnames(data_mapped), self$aesthetics)
        dplyr::mutate_at(data_mapped, dplyr::vars(!!cols), log10)
      }
    )
  )

  tbl <- tibble(x = 10, y = 100, z = 3)
  scales <- ScaleList$new()
  panel <- Panel$new(CoordLogLogIdentity$new(), scales)
  expect_identical(panel$transform(tbl), tibble(x = 1, y = 2, z = 3))
})
