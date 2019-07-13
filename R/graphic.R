
Graphic <- R6Class(
  "Graphic",

  public = list(

    layers = NULL,
    scales = NULL,
    coord = NULL,
    facet = NULL,

    plot_data = NULL,
    panels = NULL,

    initialize = function() {
      self$layers <- LayerList$new()
      self$scales <- ScaleList$new()
      self$coord <- CoordIdentity$new()
      self$facet <- FacetNull$new()
    }
  )
)
