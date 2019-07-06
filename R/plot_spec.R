
PlotSpec <- R6Class(
  "PlotSpec",
  public = list(
    layers = NULL,
    scales = NULL,
    coord = NULL,
    facet = NULL,

    initialize = function() {
      self$layers <- LayerList$new()
      self$scales <- ScaleList$new()
      self$coord <- CoordIdentity$new()
      self$facet <- FacetNull$new()
    }
  )
)
