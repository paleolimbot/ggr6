
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
