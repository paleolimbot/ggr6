
StatGeomBase <- R6Class(
  "StatGeomBase",

  public = list(
    constant_aesthetic_values = NULL,

    initialize = function(constant_aesthetic_values = list()) {
      self$set_constant_aesthetic_values(constant_aesthetic_values)
    },

    set_constant_aesthetic_values = function(constant_aesthetic_values) {
      self$constant_aesthetic_values <- constant_aesthetic_values
      invisible(self)
    }
  )
)
