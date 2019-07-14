
ColumnMapping <- R6Class(
  "ColumnMapping",
  public = list(

    aesthetics = function() {
      not_implemented() # nocov
    },

    map = function(data) {
      not_implemented() # nocov
    },

    map_new = function(data) {
      not_implemented() # nocov
    },

    map_update = function(data) {
      not_implemented() # nocov
    }
  )
)

ColumnMappingIdentity <- R6Class(
  "ColumnMappingIdentity", inherit = ColumnMapping,

  public = list(
    names = NULL,

    initialize = function(data = tibble()) {
      self$names <- colnames(data)
    },

    aesthetics = function() {
      self$names
    },

    map = function(data) {
      data
    },

    map_new = function(data) {
      data
    },

    map_update = function(data) {
      data
    }
  )
)

ColumnMappingQuosure <- R6Class(
  "ColumnMappingQuosure", inherit = ColumnMapping,

  public = list(
    exprs = NULL,

    initialize = function(...) {
      self$exprs <- quos(...)
      if (!rlang::is_named(self$exprs) || any(names(self$exprs) == "")) {
        abort("All column mappings must be named.")
      }
    },

    aesthetics = function() {
      names(self$exprs)
    },

    map = function(data) {
      dplyr::transmute(data, !!!self$exprs)
    },

    map_new = function(data) {
      new_aesthetics <- setdiff(self$aesthetics(), colnames(data))
      dplyr::mutate(data, !!!self$exprs[new_aesthetics])
    },

    map_update = function(data) {
      dplyr::mutate(data, !!!self$exprs)
    }
  )
)
