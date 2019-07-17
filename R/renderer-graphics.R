
RendererGraphics <- R6Class(
  "RendererGraphics", inherit = Renderer,
  public = list(

    render_points = function(x, y, pch = 16, cex = 1, col = "black", lwd = 1, bg = "black", ...) {
      graphics::points(x, y, pch = pch, cex = cex, col = col, lwd = lwd, bg = bg)
    },

    render_path = function(x, y, group = 1, lty = 1, lwd = 1, col = "black", ...) {
      tbl <- tibble(x, y, group, lty, lwd, col)
      private$render_grouped(tbl, graphics::lines)
    },

    render_polygon = function(x, y, group = 1, subgroup = 1, col = "black", fill = "grey50", lty = 1, ...) {
      tbl <- tibble(x, y, group, col = fill, border = col, lty)
      private$render_grouped(tbl, graphics::polygon, quos(...))
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
      guide_x <- x$guide$train(x)
      guide_y <- y$guide$train(y)

      graphics::plot(
        # creates a blank dummy plot
        x = 1, y = 1,
        type = "n", axes = FALSE, xlab = guide_x$title(), ylab = guide_y$title(),

        # sets the limits
        xlim = x$limits_continuous(),
        ylim = y$limits_continuous()
      )

      graphics::axis(1, at = guide_x$key$x, labels = guide_x$key$.labels)
      graphics::axis(2, at = guide_y$key$y, labels = guide_y$key$.labels)

      self$render_stack(...)
    },

    render_panels = function(graphic, ...) {
      dots <- quos(...)
      n_panels <- length(dots)
      withr::with_par(list(mfrow = grDevices::n2mfrow(n_panels)), {
        purrr::walk(dots, rlang::eval_tidy)
      })
    },

    default_scale = function(x, aesthetic) {
      if (aesthetic %in% c("x", "y")) {
        if (is_discrete(x))
          ScaleDiscretePosition$new(aesthetic)
        else
          ScaleContinuousPosition$new(aesthetic)

      } else if (aesthetic %in% c("col", "fill")) {
        if (is_discrete(x)) {
          ScaleDiscrete$new(aesthetic)$
            set_palette_factory(scales::hue_pal())$
            set_na_value("grey50")$
            set_guide(GuideGraphicsLegend$new())
        } else {
          ScaleContinuous$new(aesthetic)$
            set_rescaler(scales::rescale)$
            set_palette(scales::seq_gradient_pal())$
            set_na_value("grey50")$
            set_guide(GuideGraphicsLegend$new())
        }

      } else if (aesthetic == "pch") {
        if (is_discrete(x)) {
          ScaleDiscrete$new(aesthetic)$
            set_palette_factory(scales::shape_pal())$
            set_guide(GuideGraphicsLegend$new())
        } else {
          abort("Cannot map a continuous value to 'pch'")
        }

      } else {
        ScaleNull$new(aesthetic)
      }
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

GuideGraphicsLegend <- R6Class(
  "GuideGraphicsLegend", inherit = Guide,

  public = list(
    legend_args = NULL,
    layer_defaults = NULL,

    initialize = function(...) {
      super$initialize()

      legend_args <- list(...)

      if (length(legend_args) > 0 && !rlang::is_named(legend_args)) {
        abort("All legend guide arguments must be named")
      }

      self$legend_args <- legend_args
      self$layer_defaults <- NULL
    },

    train_layers = function(layers, renderer) {
      defaults <- list()
      for (layer in layers$lst) {
        if (!any(self$aesthetics() %in% layer$mapping$aesthetics())) {
          next
        }

        layer_defaults <- layer$geom$default_aesthetic_values(renderer)
        if (inherits(layer$geom, "GeomPoint")) {
          names(layer_defaults) <- gsub("(cex|lwd|bg)", "pt.\\1", names(layer_defaults))
        }

        defaults <- c(layer_defaults, defaults)
      }

      self$layer_defaults <- defaults[unique(names(defaults))]
      invisible(self)
    },

    render = function(layers, panel, renderer) {
      if (is.null(self$position())) {
        return()
      }

      legend_args <- self$legend_args
      key <- self$key
      key$legend <- key$.labels

      arg_names <- setdiff(
        names(formals(graphics::legend)),
        c("title", "x", "y")
      )

      key_arg_names <- setdiff(intersect(names(key), arg_names), names(legend_args))
      legend_arg_names <- intersect(names(legend_args), arg_names)

      exec(
        graphics::legend,
        self$position() %|W|% "bottomright",
        title = self$title() %|W|% NULL,
        !!!key[key_arg_names],
        !!!legend_args[legend_arg_names]
      )
    }
  )
)
