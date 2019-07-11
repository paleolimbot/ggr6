
PlotRenderer <- R6Class(
  "PlotRenderer",
  public = list(
    render = function(plot_built) {
      not_implemented() # nocov
    },

    render_points = function(x, y, ...) {
      not_implemented() # nocov
    },

    render_path = function(x, y, group, ...) {
      not_implemented() # nocov
    },

    render_polygon = function(x, y, group, subgroup, ...) {
      not_implemented() # nocov
    },

    render_sf = function(geometry, ...) {
      not_implemented() # nocov
    },

    render_text = function(x, y, label, ...) {
      not_implemented() # nocov
    },

    render_null = function() {
      not_implemented() # nocov
    },

    render_stack = function(...) {
      not_implemented() # nocov
    },

    default_scale = function(x, aesthetic) {
      not_implemented() # nocov
    },

    aesthetics = function(geom_type) {
      method <- self[[paste0("render_", geom_type)]]
      setdiff(names(formals(method)), "...")
    },

    default_aesthetics = function(geom_type) {
      method <- self[[paste0("render_", geom_type)]]
      args <- formals(method)
      as.list(args[!purrr::map_lgl(args, rlang::is_missing)])
    }
  )
)

PlotRendererIdentity <- R6Class(
  "PlotRendererIdentity",
  public = list(

    render_points = function(x, y, ...) {
      structure(
        list(x = x, y = y, ...),
        class = "rendered_points"
      )
    },

    render_path = function(x, y, group, ...) {
      structure(
        list(x = x, y = y, group = group, ...),
        class = "rendered_path"
      )
    },

    render_polygon = function(x, y, group, subgroup, ...) {
      structure(
        list(x = x, y = y, group = group, subgroup = subgroup, ...),
        class = "rendered_polygon"
      )
    },

    render_sf = function(geometry, ...) {
      structure(
        list(geometry = geometry, ...),
        class = "rendered_sf"
      )
    },

    render_text = function(x, y, label, ...) {
      structure(
        list(x = x, y = y, label = label, ...),
        class = "rendered_text"
      )
    },

    render_null = function() {
      structure(list(), class = "rendered_null")
    },

    render_stack = function(...) {
      structure(
        rlang::list2(...),
        class = "rendered_stack"
      )
    },

    default_scale = function(x, aesthetic) {
      ScaleNull$new(aesthetic)
    }
  )
)
