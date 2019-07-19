
#' Default implementation of Scale objects
#'
#' Most useful scales will inherit from [ScaleContinuous] or
#' [ScaleDiscrete], both of which use the [scales][scales::scales]
#' package to create meaningful mappings between user data and
#' aesthetics with useful breaks and labels.
#'
#' @eval r6inherits("ScaleSimple")
#'
#' @eval r6doc("ScaleSimple")
#'
#' @export
ScaleSimple <- R6Class(
  "ScaleSimple", inherit = Scale,
  public = list(
    breaks_in = NULL,
    breaks_minor_in = NULL,
    labels_in = NULL,
    limits_in = NULL,
    trans = NULL,
    na_value = NULL,
    limits_empty = NULL,

    range = NULL,
    oob = NULL,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)

      self$breaks_in <- waiver()
      self$breaks_minor_in <- waiver()
      self$labels_in <- waiver()
      self$limits_in <- waiver()

      self$set_oob(oob_keep)
      self$set_na_value(NA)
      self$set_limits_empty(c(1, 1))
      self$set_guide(Guide$new())
    },

    is_empty = function() {
      "
      Returns `TRUE` if the scale has `$limits()` (or can calculate them).
      "
      is_waive(self$limits_in) && (length(self$trained_range()) == 0)
    },

    transform = function(x) {
      self$trans$transform(x)
    },

    untransform = function(x) {
      self$trans$inverse(x)
    },

    transform_tbl = function(data) {
      private$tbl_modify(data, self$transform)
    },

    untransform_tbl = function(data) {
      private$tbl_modify(data, self$untransform)
    },

    train = function(x) {
      self$range$train(x)
      invisible(self)
    },

    train_tbl = function(data_trans) {
      cols <- intersect(colnames(data_trans), self$aesthetics)
      tbl <- dplyr::select(data_trans, !!cols)
      purrr::walk(tbl, self$train)

      invisible(self)
    },

    reset = function() {
      self$range$reset()
    },

    map_tbl = function(data_trans) {
      private$tbl_modify(data_trans, self$map)
    },

    breaks = function() {
      "
      Calculates and returns breaks in transformed data
      space. This can be a function of the `$limits()` and by default
      the `$breaks()` method of the transform is used.
      For continuous scales this is usually [labeling::extended()], and for
      discrete scales the default is to return the limits unchanged.
      "
      values <- function_or_value(
        self$breaks_in %|W|% self$trans$breaks,
        self$untransform(self$limits())
      )
      self$transform(values)
    },

    breaks_minor = function() {
      "
      Calculates and returns minor breaks in transformed data space. This can
      be a function of the `$breaks()`, `$limits()`, and number of intervals
      between breaks (usually 2). Minor breaks usually don't make sense for
      discrete scales.
      "
      values <- function_or_value(
        self$breaks_minor_in %|W|% self$trans$minor_breaks,
        # minor breaks function in scales::trans objects are
        # parameterized as breaks, limits, n
        self$untransform(self$breaks()),
        self$untransform(self$limits()),
        2
      )
      self$transform(values)
    },

    labels = function() {
      "
      Calculates and returns the labels associated with the `$breaks()`. This
      can be a function of the `$breaks()`, and by default the `$format()` method
      of the transform is used.
      "
      function_or_value(
        self$labels_in %|W|% self$trans$format,
        self$untransform(self$breaks())
      )
    },

    limits = function() {
      "
      Calculates and returns the limits. This can be a funnction of the
      `$trained_range()`, and defaults to returning the `$trained_range()`.
      Continuous limits are always a numeric vector of length 2; discrete limits
      are usually a character vector containing possible values.
      "
      if (self$is_empty()) {
        return(self$transform(self$limits_empty))
      }

      values <- function_or_value(
        self$limits_in %|W|% identity,
        self$untransform(self$trained_range())
      )
      self$transform(values)
    },

    trained_range = function() {
      "
      The range of the values in transformed data space that were observed
      for this scale's aesthetics. Contiuous ranges are always `NULL` (when no
      values were observed) or a numeric vector of length 2; discrete ranges
      are usually a character vector of values that were observed in the data.
      "
      self$range$range
    },

    set_breaks = function(breaks) {
      "
      Set the breaks for this scale. Can be a vector of breaks in user data space
      or a function of the `$limits()`.
      "
      self$breaks_in <- breaks
      invisible(self)
    },

    set_breaks_minor = function(breaks_minor) {
      "
      Set the minor breaks for this scale. Can be a vector of breaks in user data space
      or a function of the `$breaks()`, `$limits()`, and the number 2.
      "
      self$breaks_minor_in <- breaks_minor
      invisible(self)
    },

    set_labels = function(labels) {
      "
      Set the labels for this scale. Can be a vector of labels or a function of
      the `$breaks()`.
      "
      self$labels_in <- labels
      invisible(self)
    },

    set_limits = function(limits) {
      "
      Set the limits for this scale. Can be a function of the `$trained_range()` or
      a vector (length 2 numeric for continuous scales or a character vector for
      discrete scales).
      "
      self$limits_in <- limits
      invisible(self)
    },

    set_oob = function(oob) {
      "
      Set the out-of-bounds function for this scale. The out-of-bounds function
      is a function of a vector and the `$limits()`; useful oob functions include
      [oob_keep()] (which does nothing; the default),
      [scales::censor()] (which sets values outside the limits to `NA`) and
      [scales::squish()] (which sets values outside the limits to the outermost values),
      and [censor_discrete()] (which works like [scales::censor()] but works for
      discrete scales).
      "
      self$oob <- oob
      invisible(self)
    },

    set_range = function(range) {
      "
      Set the range class for this scale. This must be a subclass of
      [scales::Range].
      "
      assert_r6(range, "Range")
      self$range <- range
      invisible(self)
    },

    set_rescaler = function(rescaler) {
      "
      Set the rescaler function for this scale. Rescaled values (between 0 and 1)
      are often useful as inputs to continuous palletes. This function should take
      a keyword argument `from`, which is the scale `$limits()`. The most useful
      values for `rescaler` are [rescale_none()] (the default) and [scales::rescale()].
      "
      self$rescaler <- rescaler
      invisible(self)
    },

    set_trans = function(trans) {
      "
      Set the transform for this scale. See [scales::trans_new()] for a list
      of transforms for continuous scales. For discrete scales, the default
      value of [discrete_identity_trans()] is probably the only value that makes
      sense.
      "
      self$trans <- trans
      invisible(self)
    },

    set_na_value = function(na_value) {
      "
      Set the output (mapped data space) value that will represent `NA` values
      for this scale.
      "
      self$na_value <- na_value
      invisible(self)
    },

    set_limits_empty = function(limits_empty) {
      "
      Set the limits to return (in transformed data space) when `$is_empty()`
      returns `TRUE`.
      "
      self$limits_empty <- limits_empty
      invisible(self)
    }
  ),

  private = list(
    tbl_modify = function(tbl, fun) {
      cols <- intersect(colnames(tbl), self$aesthetics)
      dplyr::mutate_at(tbl, dplyr::vars(!!cols), fun)
    }
  )
)
