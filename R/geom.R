
Geom <- R6Class(
  "Geom",
  public = list(

    aesthetics = function() {
      character(0) # nocov
    },

    default_aesthetics = function() {
      list() # nocov
    },

    render_panel = function(data, panel, renderer) {
      renderer$render_stack(
        !!!purrr::map(
          dplyr::group_split(
            dplyr::group_by(
              data,
              .data$group
            )
          ),
          self$render_group,
          panel,
          renderer
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
