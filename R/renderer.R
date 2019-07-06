
PlotRenderer <- R6Class(
  "PlotRenderer",
  public = list(
    render = function(plot_built) {
      abort("Not implemented")
    },

    render_points = function(coords, ...) {
      abort("Not implemented")
    },

    render_path = function(coords, group, ...) {
      abort("Not implemented")
    },

    render_polygon = function(coords, group, subgroup, ...) {
      abort("Not implemented")
    },

    render_text = function(coords, label, ...) {
      abort("Not implemented")
    },

    render_null = function() {
      abort("Not implemented")
    },

    render_stack = function(items) {
      abort("Not implemented")
    }
  )
)

PlotRendererIdentity <- R6Class(
  "PlotRendererIdentity",
  public = list(
    render = function(plot_built) {
      abort("Not implemented")
    },

    render_points = function(coords, ...) {
      structure(
        list(coords = coords, ...),
        class = "rendered_points"
      )
    },

    render_path = function(coords, group, ...) {
      structure(
        list(coords = coords, group = group, ...),
        class = "rendered_path"
      )
    },

    render_polygon = function(coords, group, subgroup, ...) {
      structure(
        list(coords = coords, group = group, subgroup = subgroup, ...),
        class = "rendered_polygon"
      )
    },

    render_text = function(coords, label, ...) {
      structure(
        list(coords = coords, label = label, ...),
        class = "rendered_text"
      )
    },

    render_null = function() {
      structure(list(), class = "rendered_null")
    },

    render_stack = function(items) {
      structure(
        items,
        class = "rendered_stack"
      )
    }
  )
)
