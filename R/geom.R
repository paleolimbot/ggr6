
#' Graphical objects
#'
#' @export
Geom <- R6Class(
  "Geom",
  public = list(
    layer = NULL,

    set_layer = function(layer) {
      self$layer <- layer
      invisible(self)
    },

    compute_layer = function(data, panel) {
      abort("Not implemented")
    }
  )
)

GeomBlank = R6Class(
  "GeomBlank", inherit = Geom,
  public = list(
    compute_layer = function(data, panel) {
      NULL
    }
  )
)
