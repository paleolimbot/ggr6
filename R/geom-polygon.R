
GeomPolygon <- R6Class(
  "GeomPolygon", inherit = Geom,

  public = list(
    aesthetics = function(renderer) {
      renderer$aesthetics("polygon")
    },

    default_aesthetic_values = function(renderer) {
      renderer$default_aesthetics("polygon")
    },

    render_panel = function(data, panel, renderer) {
      data <- panel$transform(data)
      exec(renderer$render_polygon, !!!data[self$aesthetics()])
    }
  )
)
