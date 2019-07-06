
#' Transformations
#'
#' @export
Stat <- R6Class(
  "Stat",
  public = list(
    layer = NULL,

    set_layer = function(layer) {
      self$layer <- layer
      invisible(self)
    },

    compute_layer = function(mapped_data) {
      abort("Not implemented")
    }
  )
)

#' @rdname Stat
#' @export
StatIdentity <- R6Class(
  "StatIdentity", inherit = Stat,
  public = list(
    compute_layer = function(mapped_data) {
      mapped_data
    }
  )
)
