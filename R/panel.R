
Panel <- R6Class(
  "Panel",
  public = list(
    coord = NULL,
    scales = NULL,

    initialize = function(coord = CoordIdentity$new(), scales = ScaleList$new()) {
      self$coord <- coord
      self$scales <- scales
    },

    position_scales = function() {
      self$scales$filter_by_aesthetics(self$coord$aesthetics)
    },

    non_position_scales = function() {
      self$scales$discard_by_aesthetics(self$coord$aesthetics)
    },

    transform = function(data) {
      self$coord$transform(data, self$scales)
    }
  )
)
