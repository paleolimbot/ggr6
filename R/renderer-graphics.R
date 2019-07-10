
PlotRendererGraphics <- R6Class(
  "PlotRendererGraphics",
  public = list(

    render_points = function(x, y, ...) {
      graphics::points(x, y, ...)
    },

    render_path = function(x, y, group = 1, lty = 1, lwd = 1, col = "black", ...) {
      tbl <- tibble(x, y, group, lty, lwd, col)
      private$render_grouped(tbl, graphics::lines, quos(...))
    },

    render_polygon = function(x, y, group = 1, subgroup = 1, col = "black", fill = "grey50", lty = 1, ...) {
      tbl <- tibble(x, y, group, col = fill, border = col, lty)
      private$render_grouped(tbl, graphics::polygon, quos(...))
    },

    render_sf = function(geometry, ...) {
      plot(geometry, ..., add = TRUE)
    },

    render_text = function(x, y, label, cex = 1, col = "black", font = 1, ...) {
      graphics::text(x, y, label, cex = cex, col = col, font = font, ...)
    },

    render_null = function() {
      invisible()
    },

    render_stack = function(...) {
      purrr::walk(quos(...), rlang::eval_tidy)
    }
  ),

  private = list(
    render_grouped = function(tbl, fun, args, groups = dplyr::vars(.data$group)) {
      dplyr::group_walk(
        dplyr::group_by(tbl, !!!groups),
        function(tbl_group, grouping) {
          rlang::exec(fun, !!!tbl_group, !!!args)
        }
      )
      invisible()
    }
  )
)
