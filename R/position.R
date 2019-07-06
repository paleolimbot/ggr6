
#' Position adjustment
#'
#' @export
Position <- R6Class(
  "Position",
  public = list(
    layer = NULL,

    set_layer = function(layer) {
      self$layer <- layer
      invisible(self)
    },

    compute_layer = function(transformed_data) {
      abort("Not implemented.")
    }
  )
)

#' @rdname Position
#' @export
PositionIdentity <- R6Class(
  "PositionIdentity", inherit = Position,
  public = list(
    compute_layer = function(transformed_data) {
      transformed_data
    }
  )
)
