
#' Default implementation of Scale objects
#'
#' Most useful scales will inherit from [ScaleContinuous] or
#' [ScaleDiscrete], both of which use the [scales][scales::scales]
#' package to create meaningful mappings between user data and
#' aesthetics with useful breaks and labels.
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

    #' @details
    #' Returns `TRUE` if the scale has `$limits()` (or can calculate them).
    is_empty = function() {
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

    #' @details
    #' Calculates and returns minor breaks in transformed data space. This can
    #' be a function of the `$breaks()`, `$limits()`, and number of intervals
    #' between breaks (usually 2). Minor breaks usually don't make sense for
    #' discrete scales.
    breaks_minor = function() {
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

    #' @details
    #' Calculates and returns the labels associated with the `$breaks()`. This
    #' can be a function of the `$breaks()`, and by default the `$format()` method
    #' of the transform is used.
    labels = function() {
      function_or_value(
        self$labels_in %|W|% self$trans$format,
        self$untransform(self$breaks())
      )
    },

    #' @details
    #' Calculates and returns the limits. This can be a funnction of the
    #' `$trained_range()`, and defaults to returning the `$trained_range()`.
    #' Continuous limits are always a numeric vector of length 2; discrete limits
    #' are usually a character vector containing possible values.
    limits = function() {
      if (self$is_empty()) {
        return(self$transform(self$limits_empty))
      }

      values <- function_or_value(
        self$limits_in %|W|% identity,
        self$untransform(self$trained_range())
      )
      self$transform(values)
    },

    #' @details
    #' The range of the values in transformed data space that were observed
    #' for this scale's aesthetics. Contiuous ranges are always `NULL` (when no
    #' values were observed) or a numeric vector of length 2; discrete ranges
    #' are usually a character vector of values that were observed in the data.
    trained_range = function() {
      self$range$range
    },

    #' @details
    #' Set the breaks for this scale. Can be a vector of breaks in user data space
    #' or a function of the `$limits()`.
    set_breaks = function(breaks) {
      self$breaks_in <- breaks
      invisible(self)
    },

    #' @details
    #' Set the minor breaks for this scale. Can be a vector of breaks in user data space
    #' or a function of the `$breaks()`, `$limits()`, and the number 2.
    set_breaks_minor = function(breaks_minor) {
      self$breaks_minor_in <- breaks_minor
      invisible(self)
    },

    #' @details
    #' Set the labels for this scale. Can be a vector of labels or a function of
    #' the `$breaks()`.
    set_labels = function(labels) {
      self$labels_in <- labels
      invisible(self)
    },

    #' @details
    #' Set the limits for this scale. Can be a function of the `$trained_range()` or
    #' a vector (length 2 numeric for continuous scales or a character vector for
    #' discrete scales).
    set_limits = function(limits) {
      self$limits_in <- limits
      invisible(self)
    },

    #'  @details
    #'  Set the out-of-bounds function for this scale. The out-of-bounds function
    #' is a function of a vector and the `$limits()`; useful oob functions include
    #' [oob_keep()] (which does nothing; the default),
    #' [scales::censor()] (which sets values outside the limits to `NA`) and
    #' [scales::squish()] (which sets values outside the limits to the outermost values),
    #' and [censor_discrete()] (which works like [scales::censor()] but works for
    #' discrete scales).
    set_oob = function(oob) {
      self$oob <- oob
      invisible(self)
    },

    #' @details
    #' Set the range class for this scale. This must be a subclass of
    #' [scales::Range].
    set_range = function(range) {
      assert_r6(range, "Range")
      self$range <- range
      invisible(self)
    },

    #' @details
    #' Set the rescaler function for this scale. Rescaled values (between 0 and 1)
    #' are often useful as inputs to continuous palletes. This function should take
    #' a keyword argument `from`, which is the scale `$limits()`. The most useful
    #' values for `rescaler` are [rescale_none()] (the default) and [scales::rescale()].
    set_rescaler = function(rescaler) {
      self$rescaler <- rescaler
      invisible(self)
    },

    #' @details
    #' Set the transform for this scale. See [scales::trans_new()] for a list
    #' of transforms for continuous scales. For discrete scales, the default
    #' value of [discrete_identity_trans()] is probably the only value that makes
    #' sense.
    set_trans = function(trans) {
      self$trans <- trans
      invisible(self)
    },

    #' @details
    #' Set the output (mapped data space) value that will represent `NA` values
    #' for this scale.
    set_na_value = function(na_value) {
      self$na_value <- na_value
      invisible(self)
    },

    #' @details
    #' Set the limits to return (in transformed data space) when `$is_empty()`
    #' returns `TRUE`.
    set_limits_empty = function(limits_empty) {
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
