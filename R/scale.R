
Scale <- R6Class(
  "Scale",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = character(0)) {
      self$aesthetics <- aesthetics
    },

    transform_tbl = function(data) {
      not_implemented() # nocov
    },

    train_tbl = function(data_trans) {
      not_implemented() # nocov
    },

    map_tbl = function(data_trans) {
      not_implemented() # nocov
    },

    breaks = function() {
      not_implemented() # nocov
    },

    breaks_minor = function() {
      not_implemented() # nocov
    },

    labels = function() {
      not_implemented() # nocov
    },

    limits = function() {
      not_implemented() # nocov
    }
  )
)

ScaleNull <- R6Class(
  "ScaleNull", inherit = Scale,
  public = list(
    transform_tbl = function(data) {
      data
    },

    train_tbl = function(data_trans) {
      invisible(self)
    },

    map_tbl = function(data_trans) {
      data_trans
    },

    breaks = function() {
      NULL
    },

    breaks_minor = function() {
      NULL
    },

    labels = function() {
      NULL
    },

    limits = function() {
      c(-Inf, Inf)
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
