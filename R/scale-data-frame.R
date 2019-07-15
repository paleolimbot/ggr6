
ScaleSimpleDataFrame <- R6Class(
  "ScaleSimpleDataFrame", inherit = ScaleSimpleDiscrete,

  public = list(
    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$set_range(RangeDataFrame$new())
      self$set_limits_empty(tibble())
    }
  )
)

RangeDataFrame <- R6Class(
  "RangeDataFrame", inherit = scales::DiscreteRange,

  public = list(
    train = function(x, drop = TRUE) {
      if (!inherits(x, "data.frame")) {
        abort("Non-data frame supplied to grouped scale.")
      }

      self$range <- dplyr::distinct(
        dplyr::bind_rows(
          self$range,
          dplyr::ungroup(
            dplyr::summarise(
              dplyr::group_by_all(x, .drop = drop)
            )
          )
        )
      )

      invisible(self)
    }
  )
)
