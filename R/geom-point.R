
GeomPoint <- R6Class(
  "GeomPoint", inherit = Geom,

  public = list(
    aesthetics = function(renderer) {
      renderer$aesthetics("points")
    },

    default_aesthetic_values = function(renderer) {
      renderer$default_aesthetics("points")
    },

    render_panel = function(data, panel, renderer) {
      data <- panel$transform(data)
      exec(renderer$render_points, !!!data[self$aesthetics()])
    }
  )
)
