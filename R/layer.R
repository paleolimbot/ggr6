
Layer <- R6Class(
  "Layer",
  public = list(
    data_src = NULL,
    mapping = NULL,
    geom = NULL,
    stat = NULL,
    position = NULL,

    initialize = function(data_src = NULL, mapping = ColumnMappingIdentity$new(data_src),
                          geom = GeomBlank$new(), stat = StatIdentity$new(),
                          position = PositionIdentity$new()) {
      self$data_src <- data_src
      self$mapping <- mapping
      self$geom <- geom
      self$stat <- stat
      self$position <- position
    },

    data = function(data_src) {
      self$mapping$map_data(data_src)
    },

    data_stat = function(data_stat) {
      self$mapping$map_data_stat(data_stat)
    }
  )
)

LayerList <- R6Class(
  "LayerList", inherit = List,
  public = list(
    set = function(index, item) {
      assert_r6(item, "Layer")
      super$set(index, item)
    }
  )
)
