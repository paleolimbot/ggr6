
#' Cascading Themes
#'
#' Strong graphics packages are built on good defaults that are
#' highly customizable. The [CascadingTheme] class makes it
#' possible to define a list of defaults that specify inheritance,
#' allowing customizable defaults with less repeated code.
#'
#' @export
CascadingTheme <- R6Class(
  "CascadingTheme",
  public = list(
    data = list(),
    nodes = list(),
    tree = list(),
    nodes_validators = list(),
    values_validators = list(),

    initialize = function(data = list()) {
      self$set_data(!!!data)
    },

    compile = function() {
      value_list <- purrr::map(rlang::set_names(self$keys()), self$value)
      new_cascading_element(value_list, subclass = "compiled_theme")
    },

    keys = function() {
      names(self$nodes) %||% character(0)
    },

    inheritance = function(key) {
      assert_chr_scalar(key)
      branch <- calc_inheritance(key, self$tree)
      bad_references <- branch[!(branch %in% self$keys())]
      if (length(bad_references) > 0) {
        bad_refs_label <- paste0("'", bad_references, "'", collapse = ", ")
        abort(
          glue::glue(
            "Can't calculate inheritance for key '{key}': no such node ({bad_refs_label})"
          )
        )
      }

      branch
    },

    value_validators = function(key, default = abort(glue::glue("No validators for value: '{key}'"))) {
      assert_chr_scalar(key)
      self$values_validators[[key]] %||% default
    },

    node_validators = function(key, default = abort(glue::glue("No validators for node: '{key}'"))) {
      self$nodes_validators[[key]] %||% default
    },

    node = function(key, default = abort(glue::glue("No such node: '{key}'"))) {
      assert_chr_scalar(key)
      if (key %in% self$keys()) {
        self$nodes[[key]]
      } else {
        default
      }
    },

    value = function(key, default = abort(glue::glue("No such value: '{key}'"))) {
      assert_chr_scalar(key)
      if (key %in% self$keys()) {
        value <- self$value_base(key, default)
        if (rlang::is_box(value)) {
          rlang::unbox(value)
        } else {
          value
        }
      } else {
        default
      }
    },

    value_base = function(key, default = abort(glue::glue("No such node: '{key}'"))) {
      assert_chr_scalar(key)
      if (key %in% self$keys()) {
        branch <- self$inheritance(key)
        value <- tryCatch(
          purrr::reduce(self$nodes[branch], cascade, .init = rlang::zap()),
          error = function(e) {
            abort(
              glue::glue("Error calculating value for key '{key}':\n{e}"),
              class = "cascading_error",
              parent = e
            )
          }
        )

        tryCatch(
          apply_validators(self$value_validators(key, list()), value),
          error = function(e) {
            abort(
              glue::glue("Value failed validation for key '{key}':\n{e}"),
              class = "value_validation_error",
              parent = e
            )
          }
        )
      } else {
        default
      }
    },

    set_node = function(key, value) {
      assert_chr_scalar(key)
      if (rlang::is_zap(value)) {
        self$nodes[[key]] <- NULL
      } else {
        value <- tryCatch(
          apply_validators(self$node_validators(key, list()), value),
          error = function(e) {
            abort(
              glue::glue("Node failed validation for key '{key}':\n{e}"),
              class = "node_validation_error",
              parent = e
            )
          }
        )
        self$nodes[key] <- list(value)
      }

      invisible(self)
    },

    set_value = function(key, value) {
      assert_chr_scalar(key)
      if (key %in% self$keys()) {
        current_value <- self$value(key)
      } else {
        current_value <- missing_value()
      }

      self$set_node(key, cascade(current_value, value, self))
    },

    set_nodes = function(...) {
      items <- rlang::list2(...)
      if (!rlang::is_dictionaryish(items)) {
        abort("CascadingTheme nodes must have unique keys")
      }
      purrr::walk2(names(items), items, self$set_node)
      invisible(self)
    },

    set_values = function(...) {
      items <- rlang::list2(...)
      if (!rlang::is_dictionaryish(items)) {
        abort("CascadingTheme values must have unique keys")
      }
      purrr::walk2(names(items), items, self$set_value)
      invisible(self)
    },

    set_single_value_validators = function(key, ...) {
      assert_chr_scalar(key)
      validators <- rlang::list2(...)
      if (length(validators) == 0) {
        self$values_validators[[key]] <- NULL
      } else {
        self$values_validators[[key]] <- purrr::map(validators, rlang::as_function)
      }

      invisible(self)
    },

    set_single_node_validators = function(key, ...) {
      assert_chr_scalar(key)
      validators <- rlang::list2(...)
      if (length(validators) == 0) {
        self$nodes_validators[[key]] <- NULL
      } else {
        self$nodes_validators[[key]] <- purrr::map(validators, rlang::as_function)
      }

      invisible(self)
    },

    set_tree = function(...) {
      items <- rlang::list2(...)
      if (!rlang::is_dictionaryish(items)) {
        abort("Tree nodes in a CascadingTheme must have unique keys")
      }

      # store zapped keys (length zero)
      zapped_keys <- names(items)[purrr::map_int(items, length) == 0]
      items <- items[setdiff(names(items), zapped_keys)]

      bad_types <- names(items)[!purrr::map_lgl(items, rlang::is_character, n = 1)]
      if (length(bad_types) > 0) {
        bad_types_label <- paste0("'", bad_types, "'", collapse = ", ")
        abort(
          glue::glue("All tree nodes must be character vectors of length 1 ({bad_types_label})")
        )
      }

      # copy tree for testing
      tree_copy <- self$tree
      tree_copy[names(items)] <- items

      # make sure all inheritance can be calculated
      purrr::walk(names(tree_copy), calc_inheritance, tree_copy)

      # make sure all references are defined
      references <- union(names(tree_copy), unlist(tree_copy))
      bad_references <- references[!(references %in% self$keys())]
      if (length(bad_references) > 0) {
        bad_refs_label <- paste0("'", bad_references, "'", collapse = ", ")
        abort(
          glue::glue(
            "Bad references in tree: no such node ({bad_refs_label})"
          )
        )
      }

      # reassign tree and remove zapped elements
      self$tree <- tree_copy[setdiff(names(tree_copy), zapped_keys)]
      invisible(self)
    },

    set_data = function(...) {
      items <- rlang::list2(...)
      if (!rlang::is_dictionaryish(items)) {
        abort("CascadingTheme data must have unique keys")
      }
      self$data[names(items)] <- items
      invisible(self)
    },

    set_node_validators = function(...) {
      items <- rlang::list2(...)
      if (!rlang::is_dictionaryish(items)) {
        abort("CascadingTheme node validators must have unique keys")
      }

      for (key in names(items)) {
        item <- items[[key]]
        if (!is.list(item)) {
          item <- list(item)
        }
        self$set_single_node_validators(key, !!!item)
      }

      invisible(self)
    },

    set_value_validators = function(...) {
      items <- rlang::list2(...)
      if (!rlang::is_dictionaryish(items)) {
        abort("CascadingTheme value validators must have unique keys")
      }

      for (key in names(items)) {
        item <- items[[key]]
        if (!is.list(item)) {
          item <- list(item)
        }
        self$set_single_value_validators(key, !!!item)
      }

      invisible(self)
    }
  )
)

#' Create a cascading element
#'
#' A cascading element is a [list()] with unique names whose elements
#' can be inherited by a child [cascading_element()]. The details of
#' cascading are handled by the [cascade()] and [reverse_cascade()]
#' generics.
#'
#' @param ... Key/value pairs. Passed to [rlang::list2()], so
#'   tidy evaluation is supported.
#' @param .subclass The class to use for this cascading element
#'
#' @return A list with class `cascading_element` and any subclasses
#'   specified by `.subclass`.
#' @export
#'
cascading_element <- function(..., .subclass = NULL) {
  x <- new_cascading_element(rlang::list2(...), subclass = .subclass)
  validate_cascading_element(x)
  x
}

#' S3 details for cascading elements
#'
#' @param x (possibly) a [cascading_element()]
#' @param subclass The class to use for this cascading element
#'
#' @export
#'
#' @examples
#' is_cascading_element(cascading_element())
#' new_cascading_element(list())
#' validate_cascading_element(cascading_element())
#'
is_cascading_element <- function(x) {
  inherits(x, "cascading_element")
}

#' @rdname is_cascading_element
#' @export
new_cascading_element <- function(x, subclass = NULL) {
  if (!is.list(x)) {
    abort("A cascading_element() must be a list()")
  }

  structure(x, class = union(subclass, "cascading_element"))
}

#' @rdname is_cascading_element
#' @export
validate_cascading_element <- function(x) {
  if (!is.list(x)) {
    abort("A cascading_element() must be a list()")
  }

  if (!rlang::is_dictionaryish(x)) {
    abort("A cascading_element() must have unique names")
  }

  invisible(x)
}

#' Combine cascading elements
#'
#' Combines two values. For most values, `child` is returned in place
#' of `parent`. Exeptions are formulas (converted to quosures
#' and evaluated with the object `parent` and objects from the
#' [CascadingTheme]'s `data` field available), functions (evaluated
#' on the `parent` if there is one), and [cascading_element()]s (values
#' are merged recursively). Use [rlang::as_box()] to cascade items
#' without applying any class-based rules, and [rlang::zap()] to
#' remove a key from the parent [cascading_element()].
#'
#' @param parent The value to be inherited from
#' @param child The value to inherit
#' @param cascading_theme The [CascadingTheme] object from which [cascade()]
#'   is called.
#'
#' @export
#'
#' @examples
#' # in most cases, child is returned in place parent
#' cascade("parent value", "child value")
#'
#' # formulas/quosures can use `parent` (tidy evaluation is supported)
#' # and data supplied by the theme
#' cascade(
#'   "parent value",
#'   ~paste(parent, "child value", sep = separator),
#'   CascadingTheme$new(data = list(separator = "=>"))
#' )
#'
#' # items of cascading elements are merged recursively using the
#' # same rules
#' cascade(
#'   cascading_element(key1 = "parent value 1", key2 = "parent_value 2"),
#'   cascading_element(
#'     key1 = "child value 1",
#'     key2 = ~paste(parent, "child value2", sep = separator)
#'   ),
#'   CascadingTheme$new(data = list(separator = "=>"))
#' )
#'
cascade <- function(parent, child = missing_value(), cascading_theme = NULL) {
  UseMethod("cascade")
}

#' @rdname cascade
#' @export
cascade.default <- function(parent, child = missing_value(), cascading_theme = NULL) {
  if (is_missing_value(child)) {
    parent
  } else {
    if (missing(parent)) {
      parent <- missing_value()
    }
    reverse_cascade(child, parent, cascading_theme)
  }
}

#' @rdname cascade
#' @export
cascade.function <- function(parent, child = missing_value(), cascading_theme = NULL) {
  parent <- parent()
  cascade(parent, child, cascading_theme)
}

#' @rdname cascade
#' @export
cascade.quosure <- function(parent, child = missing_value(), cascading_theme = NULL) {
  parent <- rlang::eval_tidy(
    parent,
    cascading_theme$data
  )

  cascade(parent, child, cascading_theme)
}

#' @rdname cascade
#' @export
cascade.formula <- function(parent, child = missing_value(), cascading_theme = NULL) {
  cascade(rlang::as_quosure(parent), child, cascading_theme)
}

#' @rdname cascade
#' @export
reverse_cascade <- function(child, parent = missing_value(), cascading_theme = NULL) {
  UseMethod("reverse_cascade")
}

#' @rdname cascade
#' @export
reverse_cascade.default <- function(child, parent = missing_value(), cascading_theme = NULL) {
  child
}

#' @rdname cascade
#' @export
reverse_cascade.function <- function(child, parent = missing_value(), cascading_theme = NULL) {
  if (is_missing_value(parent)) {
    child()
  } else {
    child(parent)
  }
}

#' @rdname cascade
#' @export
reverse_cascade.quosure <- function(child, parent = missing_value(), cascading_theme = NULL) {
  rlang::eval_tidy(
    child,
    rlang::list2(parent = parent, !!!cascading_theme$data)
  )
}

#' @rdname cascade
#' @export
reverse_cascade.formula <- function(child, parent = missing_value(), cascading_theme = NULL) {
  reverse_cascade(rlang::as_quosure(child), parent, cascading_theme)
}

#' @rdname cascade
#' @export
reverse_cascade.cascading_element <- function(child, parent = missing_value(), cascading_theme = NULL) {
  if (is_cascading_element(parent)) {
    parent_only_keys <- setdiff(names(parent), names(child))
    child_only_keys <- setdiff(names(child), names(parent))
    child[parent_only_keys] <- list(missing_value())
    parent[child_only_keys] <- list(missing_value())

    final_keys <- union(names(child), names(parent))
    result <- purrr::map2(
      parent[final_keys], child[final_keys],
      cascade,
      cascading_theme = cascading_theme
    )

    # remove 'zapped' elements, return new element with child class
    zapped_keys <- names(result)[purrr::map_lgl(result, rlang::is_zap)]
    new_cascading_element(
      result[setdiff(names(result), zapped_keys)],
      subclass = class(child)
    )
  } else {
    child
  }
}

#' Sentinel for missing values
#'
#' @param x A (possibly) missing value
#'
#' @export
missing_value <- function() {
  structure(list(), class = "missing_value")
}

#' @rdname missing_value
#' @export
is_missing_value <- function(x) {
  inherits(x, "missing_value")
}

calc_inheritance <- function(key, tree, branch = character(0)) {
  new_branch <- c(key, branch)

  if (key %in% branch) {
    original_key <- branch[length(branch)]
    abort(
      glue::glue(
        "Error calculating branch for key '{original_key}': circular reference to '{key}'"
      ),
      class = "circular_reference"
    )
  } else if(key %in% names(tree)) {
    calc_inheritance(tree[[key]], tree, new_branch)
  } else {
    new_branch
  }
}

apply_validators <- function(validators, value) {
  for (validator in validators) {
    value <- validator(value)
  }

  value
}
