
Guide <- R6Class(
  "Guide",

  public = list(
    title_in = NULL,
    key = NULL,
    position_in = NULL,

    initialize = function() {
      self$set_key(tibble(.breaks = character(0), .labels = character(0)))
      self$set_title(waiver())
      self$set_position(waiver())
    },

    title = function() {
      self$title_in
    },

    position = function() {
      self$position_in
    },

    aesthetics = function() {
      self$aesthetics_from_key(self$key)
    },

    train = function(scale) {
      self$set_title(self$title() %|W|% scale$name())
      self$key <- self$make_key(scale)
      invisible(self)
    },

    merge = function(guide) {
      key <- self$key
      other_key <- guide$key

      same_breaks <- isTRUE(all.equal(key$.breaks, other_key$.breaks))
      same_labels <- isTRUE(all.equal(key$.labels, other_key$.labels))
      same_title <- identical(self$title(), guide$title())
      same_class <- identical(class(self), class(guide))
      same_pos <- identical(self$position(), guide$position())

      if (same_breaks && same_labels && same_class && same_title && same_class && same_pos) {
        new_cols <- setdiff(colnames(other_key), colnames(key))
        new_key <- dplyr::bind_cols(key, other_key[new_cols])
        self$set_key(new_key)
        TRUE
      } else {
        FALSE
      }
    },

    render = function(layers, panel, renderer) {
      not_implemented() # nocov
    },

    set_title = function(title) {
      self$title_in <- title
      invisible(self)
    },

    set_key = function(key) {
      if (!tibble::is_tibble(key)) {
        abort("A guide `key` must be a tibble")
      }

      if (!all(c(".breaks", ".labels") %in% colnames(key))) {
        abort("A Guide `key` must contain `.breaks` and `.labels` columns")
      }

      self$key <- key
      invisible(self)
    },

    set_position = function(position) {
      self$position_in <- position
      invisible(self)
    },

    make_key = function(scale, censor = TRUE) {
      aesthetics <- scale$aesthetics
      breaks <- scale$breaks()
      labels <- scale$labels()
      values <- scale$map(breaks)
      in_limits <- scale$within_limits(breaks)

      key <- tibble(
        .breaks = breaks,
        .labels = labels,
        !!!purrr::map(rlang::set_names(aesthetics), function(aesthetic) values)
      )

      if (censor) {
        key[scale$within_limits(breaks), ]
      } else {
        key
      }
    },

    aesthetics_from_key = function(key) {
      colnames(key)[!grepl("^\\.", colnames(key))]
    }
  )
)

GuideNull <- R6Class(
  "GuideNull", inherit = Guide,

  public = list(
    train = function(scale, layers) {
      # having a zero-length column of the correct type helps avoid
      # the 'Unknown or uninitialised column' tibble waning
      self$set_title(self$title() %|W|% scale$name())
      self$key <- self$make_key(scale)[0, ]
      invisible(self)
    },

    render = function(layers, panel, renderer) {
      renderer$render_null()
    }
  )
)
