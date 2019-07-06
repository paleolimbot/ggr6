
Plot <- R6Class(
  "Plot",
  public = list(
    layers = NULL,
    scales = NULL,

    initialize = function() {
      self$layers <- LayerList$new(self)
      self$scales <- ScaleList$new()
    }
  )
)
