
#' Convert between data spaces
#'
#' Scales convert between the user data space, the data space used
#' to compute the [Stat]s (transformed data space), and the data space
#' used by the [Geom]s (mapped data space) to draw graphical objects.
#' Scales also compute breaks and labels used in the [Guide]s. Most useful
#' Scale classes inherit from [ScaleSimple], which is designed to leverage
#' the [scales][scales::scales] package. Each scale represents zero
#' or more `$aesthetics` (character vector), and contains a `$guide`
#' ([Guide] object) that will be used to communicate the relationship
#' between user data values and mapped data values. `ScaleNull` is a
#' scale that has infinite limits and never transforms or maps values,
#' which is useful for testing and for values that should be passed
#' to the [Stat] and [Geom] without being transformed or mapped.
#'
#' @export
Scale <- R6Class(
  "Scale",
  public = list(
    aesthetics = NULL,
    guide = NULL,

    #' @details
    #' Create a Scale object representing zero or more `aesthetics`.
    initialize = function(aesthetics = character(0)) {
      self$aesthetics <- aesthetics
      self$set_guide(GuideNull$new())
    },

    #' @details
    #' Calculate the scale name, which should be a character vector of
    #' length 1. May return [waiver()] for when
    #' the scale name should be calculated from the [`ColumnMapping`]. This
    #' method may be removed in favour of this value living in the
    #' [Guide].
    name = function(x) {
      self$aesthetics[1]
    },

    # don't calculate test coverage for abstract methods
    # nocov start
    #' @details
    #' Transform the user-data vector `x`. Usually called by
    #' `$transform_tbl()`.
    transform = function(x) {
      not_implemented()
    },

    #' @details Untransform the transformed-data vector `x`.
    untransform = function(x) {
      not_implemented()
    },

    #' @details
    #' Train the scale with the transformed-data vector `x`.
    #' The range of values observed by the scale are used to
    #' calculate its `$limits()` (if these have not been
    #' specified manually). Usually called by `$train_tbl()`.
    train = function(x) {
      not_implemented()
    },

    #' @details Forget all values that were 'remembered' by `$train()`.
    reset = function() {
      not_implemented()
    },

    #' @details
    #' Convert the transformed-date vector `x` into mapped data
    #' space. This may be a numeric value (for position scales) or
    #' some other vector type (e.g., for colour scales). Usually
    #' called by `$map_tbl()`.
    map = function(x) {
      not_implemented()
    },

    #' @details
    #' Call `$transform()` on the columns of `data` whose names
    #' are included in `$aesthetics`.
    transform_tbl = function(data) {
      not_implemented()
    },

    #' @details
    #' Call `$untransform()` on the columns of `data` whose names
    #' are included in `$aesthetics`.
    untransform_tbl = function(data_trans) {
      not_implemented()
    },

    #' @details
    #' Call `$train()` on the columns of `data` whose names
    #' are included in `$aesthetics`.
    train_tbl = function(data_trans) {
      not_implemented()
    },

    #' @details
    #' Call `$map()` on the columns of `data` whose names
    #' are included in `$aesthetics`.
    map_tbl = function(data_trans) {
      not_implemented()
    },

    #' @details
    #' Calculate the breaks, which are usually nicely-numbered
    #' values around the scale `$limits()`.
    breaks = function() {
      not_implemented()
    },

    #' @details
    #' Calculate the minor breaks, which are usually nicely-numbered
    #' values between the scale `$breaks()` and
    #' around the scale `$limits()`.
    breaks_minor = function() {
      not_implemented()
    },

    #' @details Calculate the labels associated with the `$breaks()`.
    labels = function() {
      not_implemented()
    },

    #' @details Calculate the scale `$limits()`, which is usually the range
    #' of values observed but could also be set.
    limits = function() {
      not_implemented()
    },

    #' @details Returns a logical vector describing whether or not values in
    #' the transformed-data vector `x` are within the scale `$limits()`.
    within_limits = function(x) {
      not_implemented()
    },

    # nocov end

    #' @details
    #' Set the [Guide] that will be used to communicate the relationship
    #' between user data values and mapped data values.
    set_guide = function(guide) {
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

#' Mutable List of Scale objects
#'
#' ScaleList objects contain a list of [Scale]s, and have
#' methods that can perform the same operation on all the
#' scales they contain. They currently also generate the
#' [GuideList] of trained [Guide]s based on the [Scale]s
#' they contain.
#'
#' @export
ScaleList <- R6Class(
  "ScaleList", inherit = List,
  public = list(
    set = function(index, item) {
      assert_r6(item, "Scale")
      super$set(index, item)
    },

    #' @details
    #' A character vector of aesthetics represented by the [Scale]s
    #' within the list.
    aesthetics = function() {
      unique(unlist(purrr::map(self$lst, function(scale) scale$aesthetics)))
    },

    #' @details
    #' Extract a scale by `aesthetic` (returning `default` if there is none.)
    scale = function(aesthetic, default = NULL) {
      for (scale in self$lst) {
        if (aesthetic %in% scale$aesthetics) {
          return(scale)
        }
      }

      default
    },

    #' @details
    #' Returns a [GuideList] of [Guide]s extracted and trained from the
    #' [Scale]s within the object.
    guides = function() {
      guides <- GuideList$new()
      for (scale in self$lst) {
        guides$add(scale$guide$clone()$train(scale))
      }

      guides
    },

    #' @details
    #' Return a new `ScaleList` containing any scale representing
    #' one or more `aesthetics` specified.
    filter_by_aesthetics = function(aesthetics) {
      new <- ScaleList$new()
      for (scale in self$lst) {
        if (any(aesthetics %in% scale$aesthetics)) {
          new$add(scale)
        }
      }
      new
    },

    #' @details
    #' Return a new `ScaleList` containing any scale that does not
    #' represent the `aesthetics` specified.
    discard_by_aesthetics = function(aesthetics) {
      new <- ScaleList$new()
      for (scale in self$lst) {
        if (!any(aesthetics %in% scale$aesthetics)) {
          new$add(scale)
        }
      }
      new
    },

    #' @details
    #' Calls the `$transform_tbl()` method for each [Scale] iteratively,
    #' returning the result.
    transform_tbl = function(data) {
      for (i in seq_len(self$size())) {
        data <- self$get(i)$transform_tbl(data)
      }

      data
    },

    #' @details
    #' Calls the `$untransform_tbl()` method for each [Scale] iteratively,
    #' returning the result.
    untransform_tbl = function(data) {
      for (i in seq_len(self$size())) {
        data <- self$get(i)$untransform_tbl(data)
      }

      data
    },

    #' @details Calls the `$transform_tbl()` method for each [Scale].
    train_tbl = function(data_trans) {
      for (i in seq_len(self$size())) {
        self$get(i)$train_tbl(data_trans)
      }

      invisible(self)
    },

    #' Calls the `$reset()` method for each [Scale] iteratively,
    #' returning the result.
    reset = function() {
      for (i in seq_len(self$size())) {
        self$get(i)$reset()
      }

      invisible(self)
    },

    #' @details
    #' Calls the `$map_tbl()` method for each [Scale] iteratively,
    #' returning the result.
    map_tbl = function(data_trans) {
      for (i in seq_len(self$size())) {
        data_trans <- self$get(i)$map_tbl(data_trans)
      }

      data_trans
    },

    #' @details
    #' Add missing scales to this object based on data (whose names
    #' are aesthetics) and the [Renderer].
    add_missing = function(data, renderer) {
      assert_r6(renderer, "Renderer")

      new_aesthetics <- setdiff(names(data), self$aesthetics())
      for (aesthetic in new_aesthetics) {
        self$add(renderer$default_scale(data[[aesthetic]], aesthetic))
      }
    }
  )
)
