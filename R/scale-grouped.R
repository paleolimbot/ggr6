

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
