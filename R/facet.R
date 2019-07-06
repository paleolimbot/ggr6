
Facet <- R6Class(
  "Facet",
  public = list(
    panels = NULL,

    train = function(data) {
      not_implemented() # nocov
    },

    init_panels = function(coord, scales) {
      not_implemented() # nocov
    },

    panel_data = function(data) {
      not_implemented() # nocov
    }
  )
)

FacetNull <- R6Class(
  "FacetNull", inherit = Facet,
  public = list(
    panels = NULL,

    train = function(data) {
      invisible(self)
    },

    init_panels = function(coord, scales) {
      self$panels <- list(coord$setup_panel(scales))
      invisible(self)
    },

    panel_data = function(data) {
      if (is.null(self$panels)) {
        invalid_state("Attempting to call FacetNull$panel_data() before FacetNull$init_panels()")
      }

      list(data)
    }
  )
)
