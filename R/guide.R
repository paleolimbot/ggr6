
#' Graphical represntation of Scales
#'
#' Guide objects are a graphical representation of [Scale]s, in that
#' they communicate the relationship between user data values and
#' mapped data values. Each [Scale] must have a Guide, although this
#' can be (and often is) `GuideNull`, which draws nothing). Guides which
#' have the same name, class, breaks, and labels are merged together,
#' such that in the final plot there may be more than one scale represented
#' by one guide. All guides except `GuideNull` will probably have to be
#' subclassed for each [Renderer].
#'
#' @export
Guide <- R6Class(
  "Guide",

  public = list(
    title_in = NULL,
    key = NULL,
    position_in = NULL,

    #' @details Create a Guide object.
    initialize = function() {
      self$set_key(tibble(.breaks = character(0), .labels = character(0)))
      self$set_title(waiver())
      self$set_position(waiver())
    },

    #' @details Return the title that should be displayed by this guide.
    title = function() {
      self$title_in
    },

    #' @details
    #' Return the object describing the position of this guide (specific
    #' to the [Renderer]). A value of `NULL` means it should not be displayed;
    #' a value of `waiver()` means the position is unspecified.
    position = function() {
      self$position_in
    },

    #' @details The aesthetics represented by this `Guide`.
    aesthetics = function() {
      self$aesthetics_from_key(self$key)
    },

    #' @details
    #' Add information from a [Scale] to this guide. This creates the
    #' `$key` field, which is a [tibble::tibble()] with a column for each
    #' aesthetic and a `.breaks` and `.labels` column.
    train = function(scale) {
      self$set_title(self$title() %|W|% scale$name())
      self$key <- self$make_key(scale)
      invisible(self)
    },

    #' @details
    #' Add information from a [LayerList] to this object. This is used
    #' to assemble the default aesthetic values and geometry primatives
    #' that will be displayed by the guide.
    train_layers = function(layers, renderer) {
      invisible(self)
    },

    #' @details
    #' Merges information from another guide into this guide. The default implementation
    #' is to merge only if the Guides have the same class, breaks, labels, title, and
    #' position. This would occur if a user maps the same column to two aesthetics.
    #' Returns `TRUE` if a merge occured, `FALSE` otherwise. This is usually called by
    #' [GuideList]'s `$merge_all()` method.
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
        self$set_position(self$position() %|W|% guide$position())
        TRUE
      } else {
        FALSE
      }
    },

    # nocov start

    #' @details
    #' Renders the guide.
    render = function(panel, renderer) {
      not_implemented()
    },
    # nocov end

    set_title = function(title) {
      self$title_in <- title
      invisible(self)
    },

    #' @details
    #' Sets the `$key` for this Guide, which is a [tibble::tibble()] with
    #' a column for each aesthetic and a `.breaks` and `.labels` column.
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

    #' @details
    #' Sets the `$position()` for this guide. Use `NULL` to hide,
    #' `waiver()` to let the [Renderer] choose, or some other
    #' [Renderer]-specific value.
    set_position = function(position) {
      self$position_in <- position
      invisible(self)
    },

    #' @details
    #' Creates the `$key` based on a [Scale], optionally censoring the breaks
    #' to ensure they are within the [Scale]'s `$limits()`.
    make_key = function(scale, censor = TRUE) {
      aesthetics <- scale$aesthetics
      breaks <- scale$breaks() %||% character(0)
      labels <- scale$labels() %||% character(0)
      values <- scale$map(breaks) %||% character(0)
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

#' @rdname Guide
#' @export
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

    train_layers = function(layers, renderer) {
      invisible(self)
    },

    render = function(panel, renderer) {
      renderer$render_null()
    }
  )
)

#' List of Guides
#'
#' @export
GuideList <- R6Class(
  "GuideList", inherit = List,

  public = list(
    set = function(index, item) {
      assert_r6(item, "Guide")
      super$set(index, item)
    },

    #' @details
    #' Extract a guide by `aesthetic` (returning `default` if there is none.)
    guide = function(aesthetic, default = NULL) {
      for (guide in self$lst) {
        if (aesthetic %in% guide$aesthetics()) {
          return(guide)
        }
      }

      default
    },

    #' @details
    #' Calls the `$train_layers()` method of each [Guide].
    train_layers = function(layers, renderer) {
      for (guide in self$lst) {
        guide$train_layers(layers, renderer)
      }

      invisible(self)
    },

    #' @details
    #' Merges the [Guide] objects by calling their `$merge()` methods
    #' iteratively. Merging is done in-place.
    merge_all = function() {
      lst <- self$lst

      for (i in seq_len(length(lst) - 1)) {
        guide <- lst[[i]]
        if (is.null(guide)) {
          next
        }

        for (j in seq(i + 1, length(lst))) {
          guide2 <- lst[[j]]
          if (is.null(guide2)) {
            next
          }

          if (guide$merge(guide2)) {
            # "pop" guide2 from the list
            lst[j] <- list(NULL)
          }
        }
      }

      self$lst <- purrr::compact(lst)
      invisible(self)
    }
  )
)
