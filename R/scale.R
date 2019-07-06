
Scale <- R6Class(
  "Scale",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = character(0)) {
      self$aesthetics <- aesthetics
    },

    transform_tbl = function(data) {
      abort("Not implemented") # nocov
    },

    train_tbl = function(data_trans) {
      abort("Not implemented") # nocov
    },

    map_tbl = function(data_trans) {
      abort("Not implemented") # nocov
    }
  )
)

ScaleIdentity <- R6Class(
  "ScaleIdentity", inherit = Scale,
  public = list(
    transform_tbl = function(data) {
      data
    },

    train_tbl = function(data_trans) {
      invisible(self)
    },

    map_tbl = function(data_trans) {
      data_trans
    }
  )
)

ScaleList <- R6Class(
  "ScaleList", inherit = List,
  public = list(
    set = function(index, item) {
      if (!is.R6(item) || !inherits(item, "Scale")) {
        abort("`item` must be a Scale instance.")
      }

      super$set(index, item)
    }
  )
)
