
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

    add_layer = function(layer) {
      self$layers$add(layer)
      invisible(self)
    },

    add_scale = function(scale) {
      existing_scales <- self$scales$filter_by_aesthetics(scale$aesthetics)
      if (existing_scales$size() > 0) {
        abort(
          sprintf(
            "Scale for aesthetic %s already exists.",
            paste0("'", scale$aesthetics, "'", collapse = " / ")
          )
        )
      }

      self$scales$add(scale)
      invisible(self)
    },

    replace_scale = function(scale) {
      self$scales <- self$scales$discard_by_aesthetics(scale$aesthetics)
      self$add_scale(scale)
      invisible(self)
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
