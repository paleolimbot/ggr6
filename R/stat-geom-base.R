
#' Base class for statistics and geometries
#'
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
    },

    setup_data = function(data, panel, renderer) {
      data <- self$apply_default_aesthetic_values(data, renderer)
      self$apply_constant_aesthetic_values(data)
    },

    aesthetics = function(renderer) {
      character(0) # nocov
    },

    default_aesthetic_values = function(renderer) {
      list() # nocov
    },

    apply_default_aesthetic_values = function(data, renderer) {
      default_values <- self$default_aesthetic_values(renderer)
      new_aesthetics <- setdiff(names(default_values), colnames(data))
      dplyr::mutate(data, !!!default_values[new_aesthetics])
    },

    apply_constant_aesthetic_values = function(data) {
      dplyr::mutate(data, !!!self$constant_aesthetic_values)
    }
  )
)
