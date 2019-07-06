
Position <- R6Class(
  "Position",
  public = list(
    compute_layer = function(transformed_data) {
      abort("Not implemented.")
    }
  )
)

PositionIdentity <- R6Class(
  "PositionIdentity", inherit = Position,
  public = list(
    compute_layer = function(transformed_data) {
      transformed_data
    }
  )
)
