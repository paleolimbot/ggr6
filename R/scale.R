
Scale <- R6Class(
  "Scale",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = character(0)) {
      self$aesthetics <- aesthetics
    },

    transform = function(x) {
      not_implemented() # nocov
    },

    untransform = function(x) {
      not_implemented() # nocov
    },

    train = function(x) {
      not_implemented() # nocov
    },

    map = function(x) {
      not_implemented() # nocov
    },

    transform_tbl = function(data) {
      not_implemented() # nocov
    },

    untransform_tbl = function(data_trans) {
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

    transform = function(x) {
      x
    },

    untransform = function(x) {
      x
    },

    train = function(x) {
      invisible(self)
    },

    map = function(x) {
      x
    },

    transform_tbl = function(data) {
      data
    },

    untransform_tbl = function(data_trans) {
      data_trans
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
