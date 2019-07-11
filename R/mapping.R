
ColumnMapping <- R6Class(
  "ColumnMapping",
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
      dplyr::transmute(data, !!!exprs)
    }
  )
)

stat <- function(x) {
  x
}
