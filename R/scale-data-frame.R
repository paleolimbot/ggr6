
ScaleDataFrame <- R6Class(
  "ScaleDataFrame", inherit = ScaleDiscrete,

  public = list(
    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$set_range(RangeDataFrame$new())
      self$set_limits_empty(tibble())
    },

    within_limits = function(x) {
      limits <- self$limits()
      common_cols <- intersect(colnames(x), colnames(limits))
      if (length(common_cols) == 0) {
        return(rep(FALSE, length.out = nrow(x)))
      }

      limits$.in_limits <- TRUE
      in_limits <- dplyr::pull(
        dplyr::left_join(
          x,
          dplyr::mutate(limits, .in_limits = TRUE),
          by = common_cols
        ),
        .data$.in_limits
      )

      dplyr::coalesce(in_limits, FALSE)
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
