
ScaleDiscrete <- R6Class(
  "ScaleDiscrete", inherit = ScaleSimple,

  public = list(
    palette_factory = NULL,
    drop = NULL,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$set_range(scales::DiscreteRange$new())
      self$set_trans(discrete_identity_trans())
      self$set_drop(TRUE)
      self$set_limits_empty(character(0))
    },

    train = function(x) {
      self$range$train(x, drop = self$drop)
      invisible(self)
    },

    palette = function(x) {
      # as implemented in the scales package, there really is
      # no identity palette for discrete scales
      if (is.null(self$palette_factory)) {
        return(x)
      }

      limits <- self$limits()
      discrete_palette <- self$palette_factory(length(limits))
      matched <- match(x, limits)
      discrete_palette[matched]
    },

    map = function(x) {
      limits <- self$limits()
      censored <- self$oob(x, range = limits)
      mapped <- self$palette(censored)
      na_mapped <- vctrs::vec_cast(self$na_value, mapped)
      ifelse(!is.na(mapped), mapped, na_mapped)
    },

    within_limits = function(x) {
      x %in% self$limits()
    },

    set_palette_factory = function(palette_factory) {
      self$palette_factory <- palette_factory
      invisible(self)
    },

    set_drop = function(drop) {
      self$drop <- drop
      invisible(self)
    }
  )
)
