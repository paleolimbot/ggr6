
GeomPath <- R6Class(
  "GeomPath", inherit = Geom,

  public = list(
    aesthetics = function(renderer) {
      renderer$aesthetics("path")
    },

    default_aesthetic_values = function(renderer) {
      renderer$default_aesthetics("path")
    },

    render_panel = function(data, panel, renderer) {
      data <- panel$transform(data)
      exec(renderer$render_path, !!!data[self$aesthetics()])
    }
  )
)
