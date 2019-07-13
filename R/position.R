
Position <- R6Class(
  "Position",
  public = list(
    compute_panel = function(transformed_data, panel) {
      not_implemented() # nocov
    }
  )
)

PositionIdentity <- R6Class(
  "PositionIdentity", inherit = Position,
  public = list(
    compute_panel = function(transformed_data, panel) {
      transformed_data
    }
  )
)
