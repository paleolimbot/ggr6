
#' Transform data
#'
#' @export
Stat <- R6Class(
  "Stat", inherit = StatGeomBase,

  public = list(
    default_mapping = NULL,

    initialize = function(constant_aesthetic_values = list()) {
      super$initialize(constant_aesthetic_values)
      self$set_default_mapping(ColumnMappingIdentity$new())
    },

    set_default_mapping = function(default_mapping) {
      assert_r6(default_mapping, "ColumnMapping")
      self$default_mapping <- default_mapping
      invisible(self)
    },

    compute_panel = function(data_trans, panel, renderer) {
      dplyr::ungroup(
        dplyr::group_modify(
          dplyr::group_by(data_trans, .data$group),
          function(group_df, grouping) self$compute_group(group_df, panel, renderer)
        )
      )
    },

    compute_group = function(data_trans, panel, renderer) {
      not_implemented() # nocov
    }
  )
)

#' @rdname Stat
#' @export
StatIdentity <- R6Class(
  "StatIdentity", inherit = Stat,
  public = list(
    compute_panel = function(data_trans, panel, renderer) {
      data_trans
    }
  )
)
