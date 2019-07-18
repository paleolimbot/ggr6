
Panel <- R6Class(
  "Panel",
  public = list(
    coord = NULL,
    scales = NULL,
    guides = NULL,

    initialize = function(coord = CoordIdentity$new(), scales = ScaleList$new()) {
      self$coord <- coord
      self$scales <- scales
      self$guides <- GuideList$new()
    },

    train_guides = function(layers, renderer) {
      guides <- self$scales$guides()
      guides$merge_all()
      guides$train_layers(layers, renderer)
      self$guides <- guides

      invisible(self)
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
