

RangeDataFrame <- R6Class(
  "RangeDataFrame", inherit = scales::DiscreteRange,

  public = list(
    train = function(x) {
      if (!inherits(x, "data.frame")) {
        abort("Non-data frame supplied to grouped scale")
      }

      self$range <- dplyr::distinct(
        dplyr::bind_rows(self$range, x)
      )
    }
  )
)
