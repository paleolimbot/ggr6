
ColumnMapping <- R6Class(
  "ColumnMapping",
  public = list(

    aesthetics = function() {
      not_implemented() # nocov
    },

    map_data = function(data) {
      not_implemented() # nocov
    },

    map_data_stat = function(data) {
      not_implemented() # nocov
    }
  )
)

ColumnMappingIdentity <- R6Class(
  "ColumnMappingIdentity",
  public = list(
    names = NULL,

    initialize = function(data) {
      self$names <- colnames(data)
    },

    aesthetics = function() {
      self$names
    },

    map_data = function(data) {
      data
    },

    map_data_stat = function(data) {
      data
    }
  )
)

ColumnMappingQuosure <- R6Class(
  "ColumnMappingQuosure",
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

    map_data = function(data) {
      is_stat_mapping <- purrr::map_lgl(self$exprs, contains_call_to, "stat")
      exprs <- self$exprs[!is_stat_mapping]
      dplyr::transmute(data, !!!exprs)
    },

    map_data_stat = function(data) {
      is_stat_mapping <- purrr::map_lgl(self$exprs, contains_call_to, "stat")
      exprs <- self$exprs[is_stat_mapping]
      dplyr::mutate(data, !!!exprs)
    }
  )
)

stat <- function(x) {
  x
}
