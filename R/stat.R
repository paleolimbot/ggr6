
Stat <- R6Class(
  "Stat",
  public = list(
    compute_layer = function(mapped_data) {
      not_implemented() # nocov
    }
  )
)

StatIdentity <- R6Class(
  "StatIdentity", inherit = Stat,
  public = list(
    compute_layer = function(mapped_data) {
      mapped_data
    }
  )
)
