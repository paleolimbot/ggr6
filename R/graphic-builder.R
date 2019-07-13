
GraphicBuilder <- R6Class(
  "GraphicBuilder", inherit = Graphic,

  public = list(
    plot_data = NULL,
    panels = NULL,
    renderer = NULL,

    initialize = function(graphic, renderer = PlotRendererIdentity$new()) {
      self$layers <- graphic$layers
      self$scales <- graphic$scales
      self$coord <- graphic$coord
      self$facet <- graphic$facet
      self$renderer <- renderer
    },

    # high-level functions ---------------------------

    build_init = function() {
      self$train_facet()
      self$create_panels()
      self$plot_data <- self$create_plot_data()

      invisible(self)
    },

    build = function() {

      plot_data <- self$prepare_data_and_scales(self$plot_data)
      plot_data <- self$compute_statistics(plot_data)
      plot_data <- self$compute_positions(plot_data)
      plot_data <- self$map_non_position_scales(plot_data)
      plot_data <- self$finish_data(plot_data)

      self$plot_data <- plot_data

      invisible(self)
    },

    render = function() {
      not_implemented()
    },

    # self$build() ------------------------------------

    prepare_data_and_scales = function(plot_data) {
      plot_data <- self$split_layer_data(plot_data)
      plot_data <- self$map_data_columns(plot_data)
      self$add_missing_scales(plot_data, self$renderer)

      plot_data
    },

    compute_statistics = function(plot_data) {
      plot_data <- self$scale_transform(plot_data)
      self$scale_train_position(plot_data)
      plot_data <- self$scale_map_position(plot_data)

      plot_data <- self$stat_compute(plot_data)
      plot_data <- self$map_stat_columns(plot_data)

      plot_data
    },

    compute_positions = function(plot_data) {
      self$add_position_scales(self$renderer)
      plot_data <- self$position_compute(plot_data)
      self$scale_retrain_position(plot_data)
      self$finalize_panels()
      plot_data <- self$scale_remap_position(plot_data)

      plot_data
    },

    map_non_position_scales = function(plot_data) {
      self$scale_train_non_position(plot_data)
      plot_data <- self$scale_map_non_position(plot_data)

      plot_data
    },

    finish_data = function(plot_data) {
      plot_data <- self$add_geom_defaults(plot_data, self$renderer)

      plot_data
    },

    # self$build_init() -----------------------------------

    train_facet = function() {
      self$facet$reset()

      for (layer in self$layers$lst) {
        self$facet$train(layer$data_src)
      }
      invisible(self)
    },

    create_panels = function() {
      self$panels <- self$facet$panels(self$coord, self$scales)
    },

    create_plot_data = function() {
      panel_indices <- seq_along(self$panels)
      layer_indices <- seq_along(self$layers$lst)

      plot_data <- vector(
        mode = "list",
        length = length(panel_indices) * length(layer_indices)
      )

      dim(plot_data) <- c(length(panel_indices), length(layer_indices))

      plot_data
    },

    # self$prepare_data() -----------------------------------

    split_layer_data = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        self$facet$panel_data(layer$data_src, panel_index)
      })
    },

    map_data_columns = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        layer$data(data)
      })
    },

    add_missing_scales = function(plot_data, renderer) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$scales$add_missing(data, renderer)
      })

      invisible(self)
    },

    # self$compute_statistics() -----------------------------------

    scale_transform = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$scales$transform_tbl(data)
      })
    },

    scale_train_position = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$position_scales()$train_tbl(data)
      })
    },

    scale_map_position = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$position_scales()$map_tbl(data)
      })
    },

    stat_compute = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        layer$stat$compute_panel(data, panel)
      })
    },

    map_stat_columns = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        layer$data_stat(data)
      })
    },

    # self$compute_positions() -------------------------

    add_position_scales = function(renderer) {
      for (panel in self$panels) {
        panel$scales$add_missing(tibble(x = numeric(0), y = numeric(0)), renderer)
      }

      invisible(self)
    },

    position_compute = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        layer$position$compute_panel(data, panel)
      })
    },

    scale_retrain_position = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$position_scales()$reset()$train_tbl(data)
      })
    },

    finalize_panels = function() {
      purrr::walk(self$panels, function(panel) self$coord$finalize_panel(panel))
      invisible(self)
    },

    scale_remap_position = function(plot_data) {
      self$scale_map_position(plot_data)
    },

    # self$map_non_position_scales()

    scale_train_non_position = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$non_position_scales()$train_tbl(data)
      })
    },

    scale_map_non_position = function(plot_data) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$non_position_scales()$map_tbl(data)
      })
    },

    # self$finish_data() -----------------------

    add_geom_defaults = function(plot_data, renderer) {
      self$modify_plot_data(plot_data, function(panel, layer, data) {
        layer$geom$add_default_aesthetic_values(data, panel, renderer)
      })
    },

    # utility methods ------------------------------

    modify_plot_data = function(plot_data, fun) {
      plot_data_copy <- plot_data
      purrr::iwalk(self$panels, function(panel, panel_index) {
        purrr::iwalk(self$layers$lst, function(layer, layer_index) {
          data <- plot_data_copy[panel_index, layer_index][[1]]
          result <- fun(panel, layer, data)
          plot_data_copy[panel_index, layer_index] <- list(result)
          plot_data_copy <<- plot_data_copy
        })
      })

      plot_data_copy
    }
  )
)
