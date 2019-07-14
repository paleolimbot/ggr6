
Geom <- R6Class(
  "Geom", inherit = StatGeomBase,

  public = list(

    render_panel = function(data, panel, renderer) {
      renderer$render_stack(
        !!!dplyr::group_map(
          dplyr::group_by(data, .data$group),
          function(group_df, grouping) self$render_group(group_df, panel, renderer)
        )
      )
    },

    render_group = function(data, panel, renderer) {
      not_implemented() # nocov
    }
  )
)

GeomBlank = R6Class(
  "GeomBlank", inherit = Geom,
  public = list(
    render_panel = function(data, panel, renderer) {
      renderer$render_null()
    }
  )
)
