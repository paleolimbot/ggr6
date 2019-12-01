
#' Coordinate systems
#'
#' Coordinate systems pick which `aesthetics` are position `aesthetics`,
#' finalize the [Panel] at the end of the build process, and transform
#' coordinates. Regardless of their `aesthetics`, they should always
#' return from their `$transform()` method a data frame with `x` and `y`
#' columns. The default `CoordIdentity` does not change the [Panel] and
#' performs no changes in its `$transform()` method.
#'
#' @export
Coord <- R6Class(
  "Coord",
  public = list(
    aesthetics = NULL,

    #' @details
    #' Create a coordinate system representing one or more position
    #' `aesthetics`.
    initialize = function(aesthetics = c("x", "y")) {
      self$aesthetics <- aesthetics
    },

    # nocov start

    #' @details
    #' Transform a data frame containing columns representing one or more
    #' position `$aesthetics`. This method should always return a data frame
    #' with `x` and `y` columns and  leave non-position columns unchanged.
    transform = function(data_mapped, scales) {
      not_implemented()
    },

    #' @details
    #' Finalize the [Panel] object at the end of the build process. This is
    #' where scale expansion (if any) can occur.
    finalize_panel = function(panel) {
      not_implemented()
    }
    # nocov end
  )
)

#' @rdname Coord
#' @export
CoordIdentity <- R6Class(
  "CoordIdentity", inherit = Coord,
  public = list(
    transform = function(data_mapped, scales) {
      data_mapped
    },

    finalize_panel = function(panel) {
      panel
    }
  )
)
