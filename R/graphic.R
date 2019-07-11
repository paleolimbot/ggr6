
Graphic <- R6Class(
  "Graphic",

  public = list(

    layers = NULL,
    scales = NULL,
    coord = NULL,
    facet = NULL,

    plot_data = NULL,
    panels = NULL,

    initialize = function() {
      self$layers <- LayerList$new()
      self$scales <- ScaleList$new()
      self$coord <- CoordIdentity$new()
      self$facet <- FacetNull$new()
    },

    train_facet = function() {
      for (layer in self$layers) {
        self$facet$train(layer$data_src)
      }
      invisible(self)
    },

    create_panels = function() {
      self$facet$panels(self$coord, self$scales)
    },

    create_plot_data = function() {
      panel_indices <- seq_along(self$panels)
      layer_indices <- seq_along(self$layers)

      panel_data <- vector(
        mode = "list",
        length = length(panel_indices) * length(layer_indices)
      )

      dim(panel_data) <- c(length(panel_indices), length(layer_indices))

      panel_data
    },

    split_layer_data = function(plot_data) {
      private$modify_plot_data(plot_data, function(panel, layer, data) {
        self$facet$panel_data(layer$data_src, panel_index)
      })
    },

    map_data_columns = function(plot_data) {
      private$modify_plot_data(plot_data, function(panel, layer, data) {
        layer$data(data)
      })
    },

    add_missing_scales = function(plot_data) {
      private$modify_plot_data(plot_data, function(panel, layer, data) {
        self$panel$scales$add_missing(data)
      })
    },

    scale_transform = function(plot_data) {
      private$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$scales$transform_tbl(data)
      })
    },

    scale_train = function(plot_data) {
      private$modify_plot_data(plot_data, function(panel, layer, data) {
        panel$scales$train_tbl(data)
      })
    },

    build = function(renderer = PlotRendererIdentity$new()) {

      self$train_facet()
      self$panels <- self$create_panels()
      plot_data <- self$create_plot_data()
      plot_data <- self$split_layer_data(plot_data)
      plot_data <- self$map_data_columns(plot_data)
      plot_data <- self$add_missing_scales(plot_data)
      plot_data <- self$scale_transform(plot_data)
      plot_data <- self$scale_train(plot_data)

      # there's a bit of reseting and retraining that happen here that
      # isn't captured in the current code

      for (panel_index in panel_indices) {
        scales <- panel_scales[[panel_index]]

        for (layer_index in layer_indices) {
          layer <- self$layers$get(i)
          data_trans <- data[panel_index, layer_index]
          data_stat_computed <- layer$stat$compute_panel(data_trans, scales, renderer)
          data_stat <- layer$data_stat(data_stat_computed)
          scales$train_tbl(data_stat)
          data[panel_index, layer_index] <- list(data_stat)
        }
      }


      # this is it for building, rendering is next

      renderer$render_panels(
        !!!purrr::map(panel_indices, function(panel_index) {
          panel <- self$facet$init_panel(self$coord, panel_scales[[panel_index]])
          renderer$render_panel(
            !!!purrr::map(layer_indices, function(layer_index) {
              layer <- self$layers$get(layer_index)
              data_stat <- data[panel_index, layer_index]
              layer$geom$render_panel(data_stat, panel, renderer)
            })
          )
        })
      )
    }
  ),

  private = list(
    modify_plot_data = function(plot_data, fun) {
      plot_data_copy <- plot_data
      purrr::iwalk(self$panels, function(panel, panel_index) {
        purrr::iwalk(self$layers, function(layer, layer_index) {
          data <- plot_data_copy[panel_index, layer_index][[1]]
          result <- fun(panel, layer, data)
          plot_data_copy[panel_index, layer_index] <- list(result)
        })
      })

      plot_data_copy
    }
  )
)


