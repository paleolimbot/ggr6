
Geom <- R6Class(
  "Geom",
  public = list(
    compute_layer = function(data, panel) {
      not_implemented() # nocov
    }
  )
)

GeomBlank = R6Class(
  "GeomBlank", inherit = Geom,
  public = list(
    compute_layer = function(data, panel) {
      NULL
    }
  )
)
