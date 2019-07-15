
Guide <- R6Class(
  "Guide",
  public = list(
    title = NULL,
    key = NULL,
    position = NULL,

    initialize = function() {

    },

    train = function(scale, layers) {
      breaks <- scale$breaks()
      labels <- scale$labels()

      self$key <- tibble(
        .breaks = scale$breaks(),
        .labels = scale$labels()
      )
    }
  )
)
