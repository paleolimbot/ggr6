
Scale <- R6Class(
  "Scale",
  public = list(
    aesthetics = NULL,
    guide = NULL,

    initialize = function(aesthetics = character(0)) {
      self$aesthetics <- aesthetics
      self$set_guide(GuideNull$new())
    },

    name = function(x) {
      self$aesthetics[1]
    },

    transform = function(x) {
      not_implemented() # nocov
    },

    untransform = function(x) {
      not_implemented() # nocov
    },

    train = function(x) {
      not_implemented() # nocov
    },

    reset = function(x) {
      not_implemented() # nocov
    },

    map = function(x) {
      not_implemented() # nocov
    },

    transform_tbl = function(data) {
      not_implemented() # nocov
    },

    untransform_tbl = function(data_trans) {
      not_implemented() # nocov
    },

    train_tbl = function(data_trans) {
      not_implemented() # nocov
    },

    map_tbl = function(data_trans) {
      not_implemented() # nocov
    },

    breaks = function() {
      not_implemented() # nocov
    },

    breaks_minor = function() {
      not_implemented() # nocov
    },

    labels = function() {
      not_implemented() # nocov
    },

    limits = function() {
      not_implemented() # nocov
    },

    within_limits = function(x) {
      not_implemented() # nocov
    },

    set_guide = function(guide) {
      self$guide <- guide
      invisible(self)
    }
  )
)

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

    scale = function(aesthetic) {
      for (scale in self$lst) {
        if (aesthetic %in% scale$aesthetics) {
          return(scale)
        }
      }

      abort(sprintf("No scale for aesthetic `%s`", aesthetic))
    },

    guides = function() {
      guides <- GuideList$new()
      for (scale in self$lst) {
        guides$add(scale$guide$train(scale))
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
