
#' Render and assemble a Graphic
#'
#' The role of the renderer isn't solidified in ggr6. Currently it's an
#' abstraction of various graphics backend that allow ggr6 to focus on the
#' grammar (rather than on rendering graphical objects). Currently it has
#' methods for rendering graphical primitives, methods for finding
#' the aesthetics that are supported by each primitive, methods for
#' constructing default scales for each aesthetic, and methods for
#' assembling panels.
#'
#' Realistically, each renderer needs its own collection
#' of [Geom], [Facet], [Coord], and [Scale] subclasses.
#' The role of ggr6 is to provide robust superclasses for these such
#' that the subclasses can focus on rendering. This package provides
#' [IdentityRenderer] for testing and [GraphicsRenderer], which uses the
#' base [graphics][graphics::graphics-package] package to render graphics.
#'
#' @export
Renderer <- R6Class(
  "Renderer",
  public = list(

    #' @param x,y Finite coordinates
    #' @param feature An ID that whose unique values can
    #'   be used to identify and/or separate features
    render_points = function(x, y, feature = seq_along(x), ...) {
      not_implemented() # nocov
    },

    #' @param x,y Finite coordinates
    #' @param feature An ID that whose unique values can
    #'   be used to identify and/or separate features
    #' @param part An ID used to separate parts in a multiline geometry
    render_path = function(x, y, feature, part = 1L, ...) {
      not_implemented() # nocov
    },

    #' @param x,y Finite coordinates
    #' @param feature An ID that whose unique values can
    #'   be used to identify and/or separate features
    #' @param piece An ID used to separate rings within polygons. The
    #'   first piece in the row order is interpreted as the outer ring.
    #' @param part An ID used to separate parts in a multipolygon geometry
    render_polygon = function(x, y, feature = 1L, piece = 1L, part = 1L, ...) {
      not_implemented() # nocov
    },

    #' @param x,y Finite coordinates
    #' @param feature An ID that whose unique values can
    #'   be used to identify and/or separate features
    render_text = function(x, y, label, feature = seq_along(x), ...) {
      not_implemented() # nocov
    },

    #' @details Renders nothing
    render_null = function() {
      not_implemented() # nocov
    },

    #' @param ... A series of `render_*()` calls
    render_stack = function(...) {
      not_implemented() # nocov
    },

    #' @details This method will (should) be moved to the [Coord] or [Panel]
    #'   object in the near future
    #' @param panel A [Panel] object.
    #' @param ... A series of `render_*()` calls
    render_panel = function(panel, ...) {
      not_implemented() # nocov
    },

    #' @details This method should be moved to the [Builder] or
    #'   [Facet] object in the near future
    #' @param graphic A [Graphic] (or [Builder]?) object
    #' @param ... A series of `render_*()` calls
    render_panels = function(graphic, ...) {
      not_implemented() # nocov
    },

    #' @details Because the mapped values of (for example) a colour
    #'   scale vary between scales
    #' @param x A vector of values in user data space
    #' @param aesthetic An aesthetic type (e.g., colour, x, y)
    default_scale = function(x, aesthetic) {
      not_implemented() # nocov
    },

    #' @details Assemble a character vector of aesthetic names that can be used
    #'   with a given `render_<geom_type>()` function.
    #' @param geom_type A string that follows the pattern `render_<geom_type>()`,
    #'   where `render_<geom_type>()` is a method of the [Renderer].
    aesthetics = function(geom_type) {
      method <- self[[paste0("render_", geom_type)]]
      setdiff(names(formals(method)), "...")
    },

    #' @details Get a list of default values (in aesthetic space) for a given
    #'   geometry type.
    #' @param geom_type A string that follows the pattern `render_<geom_type>()`,
    #'   where `render_<geom_type>()` is a method of the [Renderer].
    default_aesthetics = function(geom_type) {
      method <- self[[paste0("render_", geom_type)]]
      args <- formals(method)
      args <- as.list(args[!purrr::map_lgl(args, rlang::is_missing)])
      args <- lapply(args, eval)
      args
    }
  )
)

#' @rdname Renderer
#' @export
IdentityRenderer <- R6Class(
  "IdentityRenderer", inherit = Renderer,
  public = list(

    render_points = function(x, y, feature = 1L, ...) {
      structure(
        list(x = x, y = y, feature = feature, ...),
        class = "rendered_points"
      )
    },

    render_path = function(x, y, feature = 1L, part = 1L, ...) {
      structure(
        list(x = x, y = y, feature = feature, part = part, ...),
        class = "rendered_path"
      )
    },

    render_polygon = function(x, y, feature = 1L, piece = 1L, part = 1L, ...) {
      structure(
        list(x = x, y = y, feature = feature, piece = piece, part = part, ...),
        class = "rendered_polygon"
      )
    },

    render_text = function(x, y, label, feature = 1L, ...) {
      structure(
        list(x = x, y = y, label = label, feature = feature, ...),
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

    render_panel = function(panel, ...) {
      structure(
        rlang::list2(panel = panel, data = rlang::list2(...)),
        class = "rendered_panel"
      )
    },

    render_panels = function(graphic, ...) {
      structure(
        rlang::list2(graphic = graphic, panels = rlang::list2(...)),
        class = "rendered_panels"
      )
    },

    default_scale = function(x, aesthetic) {
      if (aesthetic %in% c("x", "y")) {
        if (is_discrete(x))
          ScaleDiscretePosition$new(aesthetic)
        else
          ScaleContinuousPosition$new(aesthetic)
      } else {
        ScaleNull$new(aesthetic)
      }
    }
  )
)
