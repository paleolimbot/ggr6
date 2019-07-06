
Position <- R6Class(
  "Position",
  public = list(
    compute_layer = function(transformed_data) {
      not_implemented() # nocov
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
