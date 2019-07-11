
Coord <- R6Class(
  "Coord",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = c("x", "y", "geometry")) {
      self$aesthetics <- aesthetics
    },

    transform = function(data_mapped, scales) {
      not_implemented() # nocov
    }
  )
)

CoordIdentity <- R6Class(
  "CoordIdentity", inherit = Coord,
  public = list(
    transform = function(data_mapped, scales) {
      data_mapped
    }
  )
)
