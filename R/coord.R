
#' Coordinate systems
#'
#' Coordinate systems pick which `aesthetics` are position `aesthetics`,
#' finalize the [Panel] at the end of the build process, and transform
#' coordinates. Regardless of their `aesthetics`, they should always
#' return from their `$transform()` method a data frame with `x` and `y`
#' columns. The default `CoordIdentity` does not change the [Panel] and
#' performs no changes in its `$transform()` method.
#'
#' @eval r6doc("Coord")
#'
#' @eval r6inherits("CoordIdentity")
#'
#' @export
Coord <- R6Class(
  "Coord",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = c("x", "y")) {
      "
      Create a coordinate system representing one or more position
      `aesthetics`.
      "
      self$aesthetics <- aesthetics
    },

    # nocov start
    transform = function(data_mapped, scales) {
      "
      Transform a data frame containing columns representing one or more
      position `$aesthetics`. This method should always return a data frame
      with `x` and `y` columns and  leave non-position columns unchanged.
      "
      not_implemented()
    },

    finalize_panel = function(panel) {
      "
      Finalize the [Panel] object at the end of the build process. This is
      where scale expansion (if any) can occur.
      "
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
