
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

      self$set_coord(CoordIdentity$new())
      self$set_facet(FacetNull$new())
    },

    set_coord = function(coord) {
      assert_r6(coord, "Coord")
      self$coord <- coord
      invisible(self)
    },

    set_facet = function(facet) {
      assert_r6(facet, "Facet")
      self$facet <- facet
      invisible(self)
    }
  )
)
