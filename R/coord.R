
Coord <- R6Class(
  "Coord",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = c("x", "y", "geometry")) {
      self$aesthetics <- aesthetics
    },

    setup_panel = function(scales) {
      Panel$new(self, scales)
    },

    transform = function(coords, scales) {
      not_implemented() # nocov
    }
  )
)

CoordIdentity <- R6Class(
  "CoordIdentity", inherit = Coord,
  public = list(
    transform = function(coords, scales) {
      coords
    }
  )
)

Panel <- R6Class(
  "Panel",
  public = list(
    coord = NULL,
    scales = NULL,

    initialize = function(coord = CoordIdentity$new(), scales = ScaleList$new()) {
      self$coord <- coord
      self$scales <- scales
    },

    transform = function(data) {
      self$coord$transform(data, self$scales)
    }
  )
)
