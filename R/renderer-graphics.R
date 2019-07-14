
RendererGraphics <- R6Class(
  "RendererGraphics", inherit = Renderer,
  public = list(

    render_points = function(x, y, pch = 1, cex = 1, col = "black", lwd = 1, fill = "black", ...) {
      graphics::points(x, y, pch = pch, cex = cex, col = col, lwd = lwd, bg = fill)
    },

    render_path = function(x, y, group = 1, lty = 1, lwd = 1, col = "black", ...) {
      tbl <- tibble(x, y, group, lty, lwd, col)
      private$render_grouped(tbl, graphics::lines)
    },

    render_polygon = function(x, y, group = 1, subgroup = 1, col = "black", fill = "grey50", lty = 1, ...) {
      tbl <- tibble(x, y, group, col = fill, border = col, lty)
      private$render_grouped(tbl, graphics::polygon, quos(...))
    },

    render_sf = function(geometry, ...) {
      plot(geometry, ..., add = TRUE)
    },

    render_text = function(x, y, label, cex = 1, col = "black", font = 1, ...) {
      graphics::text(x, y, label, cex = cex, col = col, font = font)
    },

    render_null = function() {
      invisible()
    },

    render_stack = function(...) {
      purrr::walk(quos(...), rlang::eval_tidy)
    },

    render_panel = function(panel, ...) {
      x <- panel$scales$scale("x")
      y <- panel$scales$scale("y")

      graphics::plot(
        # creates a blank dummy plot
        x = 1, y = 1,
        type = "n", axes = FALSE, xlab = NA, ylab = NA,

        # sets the limits
        xlim = x$limits(),
        ylim = y$limits()
      )

      graphics::axis(1, at = x$breaks(), labels = x$labels())
      graphics::axis(2, at = y$breaks(), labels = y$labels())

      self$render_stack(...)
    },

    render_panels = function(...) {
      dots <- quos(...)
      n_panels <- length(dots)
      withr::with_par(list(mfrow = grDevices::n2mfrow(n_panels)), {
        purrr::walk(quos(...), rlang::eval_tidy)
      })
    },

    default_scale = function(x, aesthetic) {
      ScaleNull$new(aesthetic)
    }
  ),

  private = list(
    render_grouped = function(tbl, fun, args = quos(), groups = dplyr::vars(.data$group)) {
      dplyr::group_walk(
        dplyr::group_by(tbl, !!!groups),
        function(tbl_group, grouping) {
          exec(fun, !!!tbl_group, !!!args)
        }
      )
      invisible()
    }
  )
)
