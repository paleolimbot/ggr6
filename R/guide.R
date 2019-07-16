
Guide <- R6Class(
  "Guide",
  public = list(
    title = NULL,
    key = NULL,
    position = NULL,

    initialize = function() {
      self$set_position("none")
      self$set_title(waiver())
    },

    train = function(scale, layers) {
      breaks <- scale$breaks()
      labels <- scale$labels()
      in_limits <- scale$within_limits(breaks)

      self$key <- tibble(
        .breaks = breaks[in_limits],
        .labels = breaks[in_limits],
        !!scale$aesthetics[1] := scale$map(.data$.breaks)
      )

      invisible(self)
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
