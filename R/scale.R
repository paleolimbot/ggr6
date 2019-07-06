
Scale <- R6Class(
  "Scale",
  public = list(
    aesthetics = NULL,

    initialize = function(aesthetics = character(0)) {
      self$aesthetics <- aesthetics
    },

    transform = function(x) {
      abort("Not implemented")
    },

    train = function(x) {
      abort("Not implemented")
    },

    map = function(x) {
      abort("Not implemented")
    }
  )
)

ScaleIdentity <- R6Class(
  "ScaleIdentity", inherit = Scale,
  public = list(
    transform = function(x) {
      x
    },

    train = function(x) {
      x
      invisible(self)
    },

    map = function(x) {
      x
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
