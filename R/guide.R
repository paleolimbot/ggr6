
Guide <- R6Class(
  "Guide",
  public = list(
    title = NULL,
    key = NULL,
    position = NULL,

    initialize = function() {
      self$set_position(waiver())
      self$set_title(waiver())
    },

    train = function(scale) {
      not_implemented() # nocov
    },

    render = function(layers, renderer) {
      not_implemented() # nocov
    },

    make_key = function(scale, censor = TRUE) {
      aesthetics <- scale$aesthetics
      breaks <- scale$breaks()
      labels <- scale$labels()
      values <- scale$map(breaks)
      in_limits <- scale$within_limits(breaks)

      key <- tibble(
        .breaks = breaks,
        .labels = breaks,
        !!!purrr::map(rlang::set_names(aesthetics), function(aesthetic) values)
      )

      if (censor) {
        key[scale$within_limits(breaks), ]
      } else {
        key
      }
    },

    set_position = function(position) {
      self$position <- position
      invisible(self)
    },

    set_title = function(title) {
      self$title <- title
      invisible(self)
    }
  )
)

GuideNull <- R6Class(
  "GuideNull", inherit = Guide,

  public = list(
    train = function(scale, layers) {
      self$set_title(self$title %|W|% scale$name())
      self$key <- self$make_key(scale)[0, ]
      invisible(self)
    }
  )
)

GuideLegend <- R6Class(
  "GuideLegend", inherit = Guide,
  public = list(
    train = function(scale) {
      self$set_title(self$title %|W|% scale$name())
      self$key <- self$make_key(scale)
      invisible(self)
    }
  )
)

GuideAxis <- R6Class(
  "GuideAxis", inherit = GuideLegend,
  public = list(
    transform = function(panel) {
      not_implemented()
    }
  )
)
