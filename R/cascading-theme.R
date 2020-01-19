
CascadingTheme <- R6Class(
  "CascadingTheme",
  public = list(
    data = list(),
    inheritance = list(),
    values = list(),
    validators = list(),

    initialize = function(data = list(), inheritance = list(), values = list(), validators = list()) {
      self$set_data(data)
      self$set_inheritance(inheritance)
      self$set_values(values)
    },

    keys = function() {
      names(values) %||% character(0)
    },

    item = function(key) {
      branch <- self$calc_inheritance(key)
      purrr::reduce(self$items[branch], cascade)
    },

    calc_inheritance = function(key) {
      branch <- calc_inheritance(key, self$inheritance)
      bad_references <- branch[!(branch %in% names(values))]
      if (length(bad_references) > 0) {
        bad_refs_label <- paste0("'", bad_references, "'", collapse = ", ")
        abort(
          glue::glue(
            "Error calculating inheritance for key '{key}': bad references ({bad_refs_label})"
          )
        )
      }
    },

    replace_value = function(key, value) {
      items[[key]] <- value
      invisible(self)
    },

    compile = function(keys = self$keys()) {
      purrr::map(rlang::set_names(keys), self$item)
    },

    set_data = function(data) {
      self$data <- data
      invisible(self)
    },

    set_inheritance = function(inheritance) {
      self$inheritance <- inheritance
      invisible(self)
    },

    set_values = function(values) {
      self$values <- values
      invisible(self)
    },

    set_validators = function(validators) {
      self$validators <- validators
      invisible(self)
    }
  )
)

cascading_element <- function(..., .subclass = NULL) {
  new_cascading_element(rlang::list2(...), subclass = .subclass)
}

is_cascading_element <- function(x) {
  inherits(x, "cascading_element")
}

new_cascading_element <- function(x, subclass = NULL) {
  if (!is.list(x)) {
    abort("A cascading_element() must be a list()")
  }

  structure(x, class = union(subclass, "cascading_element"))
}

validate_cascading_element <- function(x) {
  if (!is.list(x)) {
    abort("A cascading_element() must be a list()")
  }
  invisible(x)
}

cascade <- function(parent, child, cascading_theme = NULL) {
  UseMethod("cascade")
}

reverse_cascade <- function(child, parent, cascading_theme = NULL) {
  UseMethod("reverse_cascade")
}

cascade.default <- function(parent, child, cascading_theme = NULL) {
  if (rlang::is_missing(child)) {
    parent
  } else {
    reverse_cascade(child, parent, cascading_theme)
  }
}

reverse_cascade.default <- function(child, parent, cascading_theme = NULL) {
  child
}

reverse_cascade.function <- function(child, parent, cascading_theme = NULL) {
  child(parent)
}

reverse_cascade.quosure <- function(child, parent, cascading_theme = NULL) {
  if (!rlang::is_missing(parent)) {
    parent_vars <- list(.x = parent, . = parent)
  } else {
    parent_vars <- list()
  }

  rlang::eval_tidy(
    child,
    rlang::list2(!!!parent_vars, !!!cascading_theme$data)
  )
}

reverse_cascade.formula <- function(child, parent, cascading_theme = NULL) {
  reverse_cascade(rlang::as_quosure(child), parent, cascading_theme)
}

reverse_cascade.cascading_element <- function(child, parent, cascading_theme = NULL) {
  if (is_cascading_element(parent)) {
    common_keys <- intersect(names(parent), names(child))
    common_items <- purrr::map2(
      parent[common_keys],
      child[common_keys],
      cascade,
      cascading_theme = cascading_theme
    )

    parent_items <- purrr::map(
      parent[setdiff(names(parent), names(child))],
      cascade,
      cascading_theme = cascading_theme
    )

    child_items <- purrr::map(
      child[setdiff(names(child), names(parent))],
      function(child) cascade(child = child, cascading_theme = cascading_theme)
    )

    new_cascading_element(
      c(parent_items, common_items, child_items),
      subclass = class(child)
    )
  } else {
    child
  }
}

calc_inheritance <- function(key, inheritance, branch = character(0)) {
  new_branch <- c(key, branch)

  if (key %in% branch) {
    original_key <- branch[length(branch)]
    abort(
      glue::glue(
        "Error calculating inheritance for key '{original_key}': circular reference to '{key}'"
      ),
      class = "circular_reference"
    )
  } else if(key %in% names(inheritance)) {
    calc_inheritance(inheritance[[key]], inheritance, new_branch)
  } else {
    new_branch
  }
}
