
ScaleContinuousPosition <- R6Class(
  "ScaleContinuousPosition", inherit = ScaleContinuous,

  public = list(
    limits_continuous_in = NULL,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$limits_continuous_in <- waiver()
    },

    map = function(x) {
      super$map(as.numeric(x))
    },

    limits_continuous = function() {
      self$limits()
    },

    set_limits_continuous = function(limits_continuous) {
      self$set_limits(limits_continuous)
      invisible(self)
    }
  )
)

ScaleDiscretePosition <- R6Class(
  "ScaleDiscretePosition", inherit = ScaleDiscrete,

  public = list(
    range_continuous = NULL,
    limits_continuous_in = NULL,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$set_range_continuous(scales::ContinuousRange$new())
      self$limits_continuous_in <- waiver()
    },

    limits_continuous = function() {
      limits_default <- range(c(self$map(self$limits()), self$range_continuous$range))
      function_or_value(
        self$limits_continuous_in %|W|% identity,
        limits_default
      )
    },

    train = function(x) {
      if (is_discrete(x)) {
        super$train(x)
      } else {
        self$range_continuous$train(x)
      }

      invisible(self)
    },

    reset = function() {
      # discrete position ranges are never reset, because when the scales
      # are reset (after computing the facet), there is no way to recover
      # the original values
      self$range_continuous$reset()
      invisible(self)
    },

    is_empty = function() {
      super$is_empty() && is.null(self$range_continuous$range)
    },

    map = function(x) {
      if (is_discrete(x)) {
        match(x, self$limits())
      } else if (is.integer(x)) {
        x
      } else {
        as.numeric(x)
      }
    },

    set_range_continuous = function(range_continuous) {
      assert_r6(range_continuous, "Range")
      self$range_continuous <- range_continuous
      invisible(self)
    },

    set_limits_continuous = function(limits_continuous) {
      self$limits_continuous_in <- limits_continuous
      invisible(self)
    }
  )
)
