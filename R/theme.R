
#' Get a value from the current theme
#'
#' Themes allow customization of default values that can be defined in a single
#' place. Using a [CascadingTheme] allows minimal repetition in defining these
#' values. Normally, a [Graphic] will be rendered by the [Builder] using
#' [with_theme()], such that a set of defaults is available. Any function that
#' gets called by the [Builder] or [Renderer] can use [theme()] to make a default
#' value configurable.
#'
#' @param key A key
#' @param default A value to return if key is not defined in the current theme
#' @param cascading_theme A [CascadingTheme] (or something else with a `$value()`
#'   method)
#' @param expr An expression to evaluate with `cascading_theme` temporarily
#'   set as the default theme
#'
#' @export
#'
#' @examples
#' # if there is no theme set, the default value is returned
#' theme("first", "a sensible default value")
#'
#' # with a theme set
#' test_theme <- CascadingTheme$new()$set_values("first" = 1)
#' with_theme(test_theme, 2 * theme("first"))
#'
theme <- function(
  key, default = abort(glue::glue("No value for `theme('{key}')`"), "no_theme_key")
) {
  theme_env$current_theme$value(key, default)
}

#' @rdname theme
#' @export
with_theme <- function(cascading_theme, expr) {
  old_theme <- theme_env$current_theme
  theme_env$current_theme <- cascading_theme
  on.exit(assign("current_theme", old_theme, envir = theme_env))
  force(expr)
}

theme_env <- new.env(parent = emptyenv())
theme_env$current_theme <- list(value = function(key, default) default)
