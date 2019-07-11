
Facet <- R6Class(
  "Facet",
  public = list(

    train = function(data) {
      not_implemented() # nocov
    },

    panels = function(coord, scales) {
      not_implemented() # nocov
    },

    panel_data = function(data, panel) {
      not_implemented() # nocov
    }
  )
)

FacetNull <- R6Class(
  "FacetNull", inherit = Facet,
  public = list(

    train = function(data) {
      invisible(self)
    },

    panels = function(coord, scales) {
      list(Panel$new(coord, scales))
    },

    panel_data = function(data, panel) {
      data
    }
  )
)
