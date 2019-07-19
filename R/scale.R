
#' Convert between data spaces
#'
#' Scales convert between the user data space, the data space used
#' to compute the [`Stat`]s (transformed data space), and the data space
#' used by the [`Geom`]s (mapped data space) to draw graphical objects.
#' Scales also compute breaks and labels used in the [`Guide`]s. Most useful
#' Scale classes inherit from [`ScaleSimple`], which is designed to leverage
#' the [scales][scales::scales] package. Each scale represents zero
#' or more `$aesthetics` (character vector), and contains a `$guide`
#' ([`Guide`] object) that will be used to communicate the relationship
#' between user data values and mapped data values. `ScaleNull` is a
#' scale that has infinite limits and never transforms or maps values,
#' which is useful for testing and for values that should be passed
#' to the [`Stat`] and [`Geom`] without being transformed or mapped.
#'
#' @eval r6doc("Scale")
#'
#' @eval r6inherits("ScaleNull")
#'
#' @export
Scale <- R6Class(
  "Scale",
  public = list(
    aesthetics = NULL,
    guide = NULL,

    initialize = function(aesthetics = character(0)) {
      "
      Create a Scale object representing zero or more `aesthetics`.
      "
      self$aesthetics <- aesthetics
      self$set_guide(GuideNull$new())
    },

    name = function(x) {
      "
      Calculate the scale name, which should be a character vector of
      length 1. May return [waiver()] for when
      the scale name should be calculated from the [`ColumnMapping`]. This
      method may be removed in favour of this value living in the
      [`Guide`].
      "
      self$aesthetics[1]
    },

    # don't calculate test coverage for abstract methods
    # nocov start
    transform = function(x) {
      "
      Transform the user-data vector `x`. Usually called by
      `$transform_tbl()`.
      "
      not_implemented()
    },

    untransform = function(x) {
      "
      Untransform the transformed-data vector `x`.
      "
      not_implemented()
    },

    train = function(x) {
      "
      Train the scale with the transformed-data vector `x`.
      The range of values observed by the scale are used to
      calculate its `$limits()` (if these have not been
      specified manually). Usually called by `$train_tbl()`.
      "
      not_implemented()
    },

    reset = function() {
      "
      Forget all values that were 'remembered' by `$train()`.
      "
      not_implemented()
    },

    map = function(x) {
      "
      Convert the transformed-date vector `x` into mapped data
      space. This may be a numeric value (for position scales) or
      some other vector type (e.g., for colour scales). Usually
      called by `$map_tbl()`.
      "
      not_implemented()
    },

    transform_tbl = function(data) {
      "
      Call `$transform()` on the columns of `data` whose names
      are included in `$aesthetics`.
      "
      not_implemented()
    },

    untransform_tbl = function(data_trans) {
      "
      Call `$untransform()` on the columns of `data` whose names
      are included in `$aesthetics`.
      "
      not_implemented()
    },

    train_tbl = function(data_trans) {
      "
      Call `$train()` on the columns of `data` whose names
      are included in `$aesthetics`.
      "
      not_implemented()
    },

    map_tbl = function(data_trans) {
      "
      Call `$map()` on the columns of `data` whose names
      are included in `$aesthetics`.
      "
      not_implemented()
    },

    breaks = function() {
      "
      Calculate the breaks, which are usually nicely-numbered
      values around the scale `$limits()`.
      "
      not_implemented()
    },

    breaks_minor = function() {
      "
      Calculate the minor breaks, which are usually nicely-numbered
      values between the scale `$breaks()` and
      around the scale `$limits()`.
      "
      not_implemented()
    },

    labels = function() {
      "
      Calculate the labels associated with the `$breaks()`.
      "
      not_implemented()
    },

    limits = function() {
      "
      Calculate the scale `$limits()`, which is usually the range
      of values observed but could also be set.
      "
      not_implemented()
    },

    within_limits = function(x) {
      "
      Returns a logical vector describing whether or not values in
      the transformed-data vector `x` are within the scale `$limits()`.
      "
      not_implemented()
    },

    # nocov end

    set_guide = function(guide) {
      "
      Set the [Guide] that will be used to communicate the relationship
      between user data values and mapped data values.
      "
      assert_r6(guide, "Guide")
      self$guide <- guide
      invisible(self)
    }
  )
)

#' @rdname Scale
#' @export
ScaleNull <- R6Class(
  "ScaleNull", inherit = Scale,
  public = list(

    transform = function(x) {
      x
    },

    untransform = function(x) {
      x
    },

    train = function(x) {
      invisible(self)
    },

    reset = function() {
      invisible(self)
    },

    map = function(x) {
      x
    },

    transform_tbl = function(data) {
      data
    },

    untransform_tbl = function(data_trans) {
      data_trans
    },

    train_tbl = function(data_trans) {
      invisible(self)
    },

    map_tbl = function(data_trans) {
      data_trans
    },

    breaks = function() {
      NULL
    },

    breaks_minor = function() {
      NULL
    },

    labels = function() {
      NULL
    },

    limits = function() {
      c(-Inf, Inf)
    },

    within_limits = function(x) {
      rep(TRUE, length.out = length(x))
    }
  )
)

ScaleList <- R6Class(
  "ScaleList", inherit = List,
  public = list(
    set = function(index, item) {
      assert_r6(item, "Scale")
      super$set(index, item)
    },

    aesthetics = function() {
      unique(unlist(purrr::map(self$lst, function(scale) scale$aesthetics)))
    },

    scale = function(aesthetic, default = abort(sprintf("No scale for aesthetic `%s`", aesthetic))) {
      for (scale in self$lst) {
        if (aesthetic %in% scale$aesthetics) {
          return(scale)
        }
      }

      default
    },

    guides = function() {
      guides <- GuideList$new()
      for (scale in self$lst) {
        guides$add(scale$guide$clone()$train(scale))
      }

      guides
    },

    filter_by_aesthetics = function(aesthetics) {
      new <- ScaleList$new()
      for (scale in self$lst) {
        if (any(aesthetics %in% scale$aesthetics)) {
          new$add(scale)
        }
      }
      new
    },

    discard_by_aesthetics = function(aesthetics) {
      new <- ScaleList$new()
      for (scale in self$lst) {
        if (!any(aesthetics %in% scale$aesthetics)) {
          new$add(scale)
        }
      }
      new
    },

    transform_tbl = function(data) {
      for (i in seq_len(self$size())) {
        data <- self$get(i)$transform_tbl(data)
      }

      data
    },

    untransform_tbl = function(data) {
      for (i in seq_len(self$size())) {
        data <- self$get(i)$untransform_tbl(data)
      }

      data
    },

    train_tbl = function(data_trans) {
      for (i in seq_len(self$size())) {
        self$get(i)$train_tbl(data_trans)
      }

      invisible(self)
    },

    reset = function() {
      for (i in seq_len(self$size())) {
        self$get(i)$reset()
      }

      invisible(self)
    },

    map_tbl = function(data_trans) {
      for (i in seq_len(self$size())) {
        data_trans <- self$get(i)$map_tbl(data_trans)
      }

      data_trans
    },

    add_missing = function(data, renderer) {
      assert_r6(renderer, "Renderer")

      new_aesthetics <- setdiff(names(data), self$aesthetics())
      for (aesthetic in new_aesthetics) {
        self$add(renderer$default_scale(data[[aesthetic]], aesthetic))
      }
    }
  )
)
