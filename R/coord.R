
Coord <- R6Class(
  "Coord",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = c("x", "y")) {
      self$aesthetics <- aesthetics
    },

    setup_panel = function(scales) {
      Panel$new(self, scales)
    },

    coords = function(data, scales) {
      coords <- data[self$aesthetics]
      self$transform_coords(coords, scales)
    },

    transform_coords = function(coords, scales) {
      not_implemented() # nocov
    }
  )
)

CoordIdentity <- R6Class(
  "CoordIdentity", inherit = Coord,
  public = list(
    transform_coords = function(coords, scales) {
      coords
    }
  )
)

Panel <- R6Class(
  "Panel",
  public = list(
    coord = NULL,
    scales = NULL,

    initialize = function(coord, scales) {
      self$coord <- coord
      self$scales <- scales
    },

    coords = function(data) {
      self$coord$coords(data, self$scales)
    }
  )
)
