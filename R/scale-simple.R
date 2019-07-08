
ScaleSimple <- R6Class(
  "ScaleSimple", inherit = Scale,
  public = list(
    breaks_in = NULL,
    breaks_minor_in = NULL,
    labels_in = NULL,
    limits_in = identity,

    initialize = function(aesthetics = character(0)) {
      super$initialize(aesthetics)
    },

    transform_tbl = function(data) {
      private$tbl_modify(data, self$transform)
    },

    train_tbl = function(data_trans) {
      private$tbl_modify(data, self$transform)
      invisible(self)
    },

    map_tbl = function(data_trans) {
      private$tbl_modify(data, self$map)
    },

    breaks = function() {
      values <- function_or_value(self$breaks_in, self$untransform(self$limits()))
      self$transform(values)
    },

    breaks_minor = function() {
      values <- function_or_value(self$breaks_minor_in, self$untransform(self$limits()))
      self$transform(values)
    },

    labels = function() {
      function_or_value(self$labels_in, self$untransform(self$breaks()))
    },

    limits = function() {
      values <- function_or_value(self$limits_in, self$untransform(self$trained_range()))
      self$transform(values)
    },

    trained_range = function() {
      not_implemented()
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
    }
  ),

  private = list(
    tbl_modify = function(tbl, fun) {
      cols <- intersect(colnames(data), self$aesthetics)
      dplyr::mutate_at(
        dplyr::collect(data),
        dplyr::vars(!!cols),
        fun
      )
    }
  )
)

ScaleSimpleContinuous <- R6Class(
  "ScaleSimple", inherit = ScaleSimple,

  public = list(
    range = NULL,
    trans = NULL,
    palette = NULL,
    oob = NULL,
    rescaler = NULL,
    na_value = NULL,

    initialize = function(aesthetics = character(0), na_value = NA) {
      super$initialize(aesthetics)
      self$range <- scales::ContinuousRange$new()
      self$palette <- scales::identity_pal()
      self$oob <- function(x, limits) x
      self$rescaler <- scales::rescale
      self$na_value <- na_value

      self$set_trans(scales::identity_trans())
    },

    transform = function(x) {
      self$trans$transform(x)
    },

    untransform = function(x) {
      self$trans$inverse(x)
    },

    train = function(x) {
      self$range$train(x)
      invisible(self)
    },

    map = function(x) {
      limits <- self$limits()

      x <- self$rescale(self$oob(x, range = limits), limits)

      uniq <- unique(x)
      pal <- self$palette(uniq)
      scaled <- pal[match(x, uniq)]

      ifelse(!is.na(scaled), scaled, self$na_value)
    },

    trained_range = function() {
      self$range$range
    },

    set_trans = function(trans) {
      self$trans <- trans
      self$breaks_in <- self$breaks_in %||% self$trans$breaks
      self$labels_in <- self$labels_in %||% self$trans$labels
      invisible(self)
    }
  )
)
