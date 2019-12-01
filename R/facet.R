
#' Split data in to one or more panels
#'
#' Facets as implemented in ggr6 split layer data into
#' one or more pieces, and generate the list of [Panel]
#' objects that contain the list of [Scale]s, the list of
#' [Guide]s, and a reference to the [Coord]inate system.
#' Eventually they will have to also draw the panels (currently
#' this is the job of the [Renderer]). The default `FacetNull`
#' draws one panel with all of each [Layer]'s data.
#'
Facet <- R6Class(
  "Facet",
  public = list(

    train = function(data) {
      not_implemented() # nocov
    },

    reset = function() {
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

#' @rdname Facet
#' @export
FacetNull <- R6Class(
  "FacetNull", inherit = Facet,
  public = list(

    train = function(data) {
      invisible(self)
    },

    reset = function() {
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
