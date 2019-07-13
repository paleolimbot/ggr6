
Geom <- R6Class(
  "Geom",
  public = list(

    aesthetics = function(renderer) {
      character(0) # nocov
    },

    default_aesthetic_values = function(renderer) {
      list() # nocov
    },

    add_default_aesthetic_values = function(data, panel, renderer) {
      missing_aesthetics <- setdiff(self$aesthetics(renderer), colnames(data))
      missing_aesthetic_values <- self$default_aesthetic_values(renderer)[missing_aesthetics]
      dplyr::mutate(data, !!!missing_aesthetic_values)
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
