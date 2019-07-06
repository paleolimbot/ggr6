
Layer <- R6Class(
  "Layer",
  public = list(
    data = NULL,
    geom = NULL,
    stat = NULL,
    position = NULL,

    initialize = function(data = NULL, geom = GeomBlank$new(), stat = StatIdentity$new(),
                          position = PositionIdentity$new()) {
      self$data <- data
      self$geom <- geom
      self$stat <- stat
      self$position <- position
    }
  )
)

LayerList <- R6Class(
  "LayerList", inherit = List,
  public = list(
    set = function(index, item) {
      if (!is.R6(item) || !inherits(item, "Layer")) {
        abort("`item` must be a Layer instance.")
      }

      super$set(index, item)
    }
  )
)
