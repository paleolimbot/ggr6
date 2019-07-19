
not_implemented <- function(message = "Not implemented") {
  abort(message, "not_implemented", trace = rlang::trace_back(bottom = 1))
}

invalid_state <- function(message = "Invalid state") {
  abort(message, "invalid_state", trace = rlang::trace_back(bottom = 1))
}

assert_r6 <- function(x, class) {
  x_label <- rlang::as_label(enquo(x))

  if (!is.R6(x) || !inherits(x, class)) {
    actual_class <- paste(class(x), collapse = " / ")
    message <- sprintf(
      "`%s` does not inherit from R6 class `%s`\n(inherits from %s)",
      x_label, class, actual_class
    )

    abort(message, "bad_r6_type", trace = rlang::trace_back(bottom = 1))
  }

  invisible(x)
}

function_or_value <- function(fun_or_value, ...) {
  if (is.function(fun_or_value)) {
    fun_or_value(...)
  } else {
    fun_or_value
  }
}

#' Default value
#'
#' A `waiver()` is used to specify a default value when `NULL` has meaning.
#'
#' @export
waiver <- function() structure(list(), class = "waiver")

is_waive <- function(x) inherits(x, "waiver")

`%|W|%` <- function(a, b) {
  if (!is_waive(a)) a else b
}

trans_discrete_new <- function(..., breaks = identity,
                               minor_breaks = function(breaks, limits, n) NULL,
                               format = identity,
                               domain = c(-Inf, Inf)) {
  scales::trans_new(
    ...,
    breaks = breaks,
    format = format,
    minor_breaks = minor_breaks,
    domain = domain
  )
}

discrete_identity_trans <- function() {
  trans_discrete_new(
    "discrete_identity",
    transform = force,
    inverse = force
  )
}

s3_trans <- function(class_out, class_in = NULL) {
  force(class_out)
  force(class_in)

  trans_discrete_new(
    "s3_trans",
    transform = function(x) {
      class(x) <- class_out
      x
    },
    inverse = function(x) {
      class(x) <- class_in
      x
    }
  )
}

rescale_none <- function(x, ...) {
  x
}

oob_keep <- function(x, ...) {
  x
}

censor_discrete <- function(x, range) {
  na_value <- vctrs::vec_cast(NA, x)
  ifelse(x %in% range, x, na_value)
}

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}
