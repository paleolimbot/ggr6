
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
      private$tbl_modify(data, self$transform)
      invisible(self)
    },

    map_tbl = function(data_trans) {
      private$tbl_modify(data, self$map)
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
        self$untransform(self$breaks()),
        self$untransform(self$limits())
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
      self$pallete <- palette
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
