
ScaleSimpleContinuous <- R6Class(
  "ScaleSimple", inherit = ScaleSimple,

  public = list(
    range = NULL,
    palette = NULL,
    oob = NULL,
    rescaler = NULL,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$range <- scales::ContinuousRange$new()
      self$palette <- scales::identity_pal()
      self$oob <- oob_keep
      self$rescaler <- rescale_none
      self$na_value <- NA
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

    set_range = function(range) {
      self$range <- range
      invisible(self)
    },

    set_oob = function(oob) {
      self$oob <- oob
      invisible(self)
    },

    set_palette = function(palette) {
      self$palette <- palette
      invisible(self)
    },

    set_rescaler = function(rescaler) {
      self$rescaler <- rescaler
      invisible(self)
    },

    set_trans = function(trans) {
      super$set_trans(trans)
      self$set_limits_empty(scales::squish(self$limits_empty, self$trans$domain))
      invisible(self)
    }
  )
)

