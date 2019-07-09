
ScaleSimpleContinuous <- R6Class(
  "ScaleSimpleContinuous", inherit = ScaleSimple,

  public = list(
    rescaler = NULL,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$set_range(scales::ContinuousRange$new())
      self$set_palette(scales::identity_pal())
      self$set_oob(oob_keep)
      self$set_rescaler(rescale_none)
      self$set_na_value(NA)
      self$set_limits_empty(c(1, 1))
      self$set_trans(scales::identity_trans())
    },

    train = function(x) {
      self$range$train(x)
      invisible(self)
    },

    map = function(x) {
      limits <- self$limits()
      censored <- self$oob(x, range = limits)
      rescaled <- self$rescaler(censored, from = limits)
      mapped <- self$palette(rescaled)
      na_mapped <- vctrs::vec_cast(self$na_value, mapped)
      ifelse(!is.na(mapped), mapped, na_mapped)
    },

    trained_range = function() {
      self$range$range
    },

    set_trans = function(trans) {
      super$set_trans(trans)
      self$set_limits_empty(scales::squish(self$limits_empty, self$trans$domain))
      invisible(self)
    }
  )
)

