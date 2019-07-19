
#' A Graphics Layer
#'
#' Specifies a Layer within a [Graphic]. Each [Layer] in the
#' Grammar of Graphics framework contains a [Stat], a [Geom], and
#' a [Position]. The `data_src` and [ColumnMapping] are used to create
#' the data for each layer; the `stat_mapping` is a [ColumnMapping] that
#' is applied after the [Stat] is computed.
#'
#' @export
Layer <- R6Class(
  "Layer",
  public = list(
    data_src = NULL,
    mapping = NULL,
    stat_mapping = NULL,
    geom = NULL,
    stat = NULL,
    position = NULL,

    initialize = function(data_src = NULL,
                          mapping = ColumnMappingIdentity$new(data_src),
                          stat_mapping = ColumnMappingIdentity$new(),
                          geom = GeomBlank$new(),
                          stat = StatIdentity$new(),
                          position = PositionIdentity$new()) {
      self$data_src <- data_src
      self$mapping <- mapping
      self$stat_mapping <- stat_mapping
      self$geom <- geom
      self$stat <- stat
      self$position <- position
    },

    data = function(data_src) {
      self$mapping$map(data_src)
    },

    data_stat = function(data_stat) {
      self$stat_mapping$map_update(data_stat)
    }
  )
)

#' A list of Layers
#'
#' A mutable list of [Layer] objects.
#'
#' @export
LayerList <- R6Class(
  "LayerList", inherit = List,
  public = list(
    set = function(index, item) {
      assert_r6(item, "Layer")
      super$set(index, item)
    }
  )
)
