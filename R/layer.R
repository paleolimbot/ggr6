
Layer <- R6Class(
  "Layer",
  public = list(
    plot = NULL,
    data = NULL,
    geom = NULL,
    stat = NULL,
    position = NULL,

    initialize = function(data = NULL, geom, stat = StatIdentity$new(), position = PositionIdentity$new()) {
      self$data <- data
      self$geom <- geom
      self$stat <- stat
      self$position <- position

      self$geom$set_layer(self)
      self$stat$set_layer(self)
      self$position$set_layer(self)
    },

    set_plot = function(plot) {
      self$plot <- plot
      invisible(self)
    }
  )
)

LayerList <- R6Class(
  "LayerList", inherit = List,
  public = list(
    initialize = function(plot, lst = list())  {
      super$initialize(lst)
      self$plot <- plot
    },

    set = function(index, item) {
      if (!is.R6(item) || !inherits(item, "Layer")) {
        abort("`item` must be a Layer instance.")
      }

      item$set_plot(self$plot)
      super$set(index, item)
    }
  )
)
