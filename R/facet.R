
Facet <- R6Class(
  "Facet",
  public = list(
    panels = NULL,

    train = function(data) {
      not_implemented() # nocov
    },

    panel_indices = function() {
      not_implemented() # nocov
    },

    panel_data = function(data, index) {
      not_implemented() # nocov
    },

    panel_data_all = function(data) {
      purrr::map(
        self$panel_indices(),
        function(i) self$panel_data(data, i)
      )
    },

    panel_scales = function(scales, index) {
      not_implemented() # nocov
    },

    init_panels = function(coord, scales) {
      not_implemented() # nocov
    }
  )
)

FacetNull <- R6Class(
  "FacetNull", inherit = Facet,
  public = list(
    panels = NULL,

    train = function(data) {
      invisible(self)
    },

    panel_indices = function() {
      1L
    },

    panel_data = function(data, index) {
      data
    },

    panel_scales = function(scales, index) {
      scales
    },

    init_panels = function(coord, scales) {
      self$panels <- list(coord$setup_panel(scales))
      invisible(self)
    }
  )
)
