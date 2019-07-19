
#' Adjust positions before rendering
#'
#' Positions are called once per panel per layer between
#' the [Stat] computation and the [Geom] computation. The
#' default `PositionIdentity` does no position adjustment.
#'
#' @eval r6doc("Position")
#'
#' @eval r6inherits("PositionIdentity")
#'
#' @export
Position <- R6Class(
  "Position",
  public = list(
    # nocov start
    compute_panel = function(transformed_data, panel) {
      "
      Perform position adjustment, returning a transformed copy of
      `transformed_data`.
      "
      not_implemented()
    }
    # nocov end
  )
)

#' @rdname Position
#' @export
PositionIdentity <- R6Class(
  "PositionIdentity", inherit = Position,
  public = list(
    compute_panel = function(transformed_data, panel) {
      transformed_data
    }
  )
)
