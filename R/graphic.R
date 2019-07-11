
Graphic <- R6Class(
  "Graphic",
  public = list(
    layers = NULL,
    scales = NULL,
    coord = NULL,
    facet = NULL,

    initialize = function() {
      self$layers <- LayerList$new()
      self$scales <- ScaleList$new()
      self$coord <- CoordIdentity$new()
      self$facet <- FacetNull$new()
    },

    build = function(renderer = PlotRendererIdentity$new()) {

      for (layer in self$layers) {
        self$facet$train(layer$data_src)
      }

      panel_indices <- self$facet$panel_indices()
      layer_indices <- seq_along(self$layers)

      panel_scales <- vector(mode = "list", length = length(panel_indices))
      data <- vector(mode = "list", length = length(panel_indices) * length(layer_indices))
      dim(data_trans) <- c(length(panel_indices), length(layer_indices))

      for (panel_index in panel_indices) {
        scales <- self$facet$panel_scales(self$scales, panel_index)
        panel_scales[[panel_index]] <- scales

        for (layer_index in layer_indices) {
          layer <- self$layers$get(i)


          data_src <- self$facet$panel_data(layer$data_src, panel_index)
          data <- layer$data(data_src)
          scales$add_missing(data)

          data_trans <- scales$transform_tbl(data)
          scales$train_tbl(data_trans)
          data[panel_index, layer_index] <- list(data_trans)
        }
      }

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
  )
)
