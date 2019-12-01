
#' Continuous Scale
#'
#' @export
ScaleContinuous <- R6Class(
  "ScaleContinuous", inherit = ScaleSimple,

  public = list(
    rescaler = NULL,
    palette = NULL,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
      self$set_range(scales::ContinuousRange$new())

      self$set_palette(scales::identity_pal())
      self$set_rescaler(rescale_none)
      self$set_trans(scales::identity_trans())
    },

    map = function(x) {
      limits <- self$limits()
      censored <- self$oob(x, range = limits)
      rescaled <- self$rescaler(censored, from = limits)
      mapped <- self$palette(rescaled)
      na_mapped <- vctrs::vec_cast(self$na_value, mapped)
      ifelse(!is.na(mapped), mapped, na_mapped)
    },

    within_limits = function(x) {
      limits <- self$limits()
      x >= limits[1] & x <= limits[2]
    },

    set_trans = function(trans) {
      super$set_trans(trans)
      self$set_limits_empty(scales::squish(self$limits_empty, self$trans$domain))
      invisible(self)
    },

    set_palette = function(palette) {
      self$palette <- palette
      invisible(self)
    }
  )
)
