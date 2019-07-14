
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
      self$stat_mapping$map_new(data_stat)
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
