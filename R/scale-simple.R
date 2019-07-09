
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

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)

      self$breaks_in <- waiver()
      self$breaks_minor_in <- waiver()
      self$labels_in <- waiver()
      self$limits_in <- waiver()
    },

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

    train_tbl = function(data_trans) {
      cols <- intersect(colnames(data_trans), self$aesthetics)
      tbl <- dplyr::collect(
        dplyr::select(data_trans, !!cols)
      )
      purrr::walk(tbl, self$train)

      invisible(self)
    },

    map_tbl = function(data_trans) {
      private$tbl_modify(data_trans, self$map)
    },

    breaks = function() {
      values <- function_or_value(
        self$breaks_in %|W|% self$trans$breaks,
        self$untransform(self$limits())
      )
      self$transform(values)
    },

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

    labels = function() {
      function_or_value(
        self$labels_in %|W|% self$trans$labels,
        self$untransform(self$breaks())
      )
    },

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

    trained_range = function() {
      not_implemented() # nocov
    },

    set_breaks = function(breaks) {
      self$breaks_in <- breaks
      invisible(self)
    },

    set_breaks_minor = function(breaks_minor) {
      self$breaks_minor_in <- breaks_minor
      invisible(self)
    },

    set_labels = function(labels) {
      self$labels_in <- labels
      invisible(self)
    },

    set_limits = function(limits) {
      self$limits_in <- limits
      invisible(self)
    },

    set_trans = function(trans) {
      self$trans <- trans
      invisible(self)
    },

    set_na_value = function(na_value) {
      self$na_value <- na_value
      invisible(self)
    },

    set_limits_empty = function(limits_empty) {
      self$limits_empty <- limits_empty
      invisible(self)
    }
  ),

  private = list(
    tbl_modify = function(tbl, fun) {
      cols <- intersect(colnames(tbl), self$aesthetics)
      dplyr::mutate_at(
        dplyr::collect(tbl),
        dplyr::vars(!!cols),
        fun
      )
    }
  )
)
