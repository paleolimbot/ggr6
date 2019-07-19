
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

#' Discrete Transform
#'
#' Discrete transforms have different defaults than those used by
#' [scales::trans_new()]. `discrete_identity_trans()` is probably
#' the only useful discrete transform.
#'
#' @param ...,breaks,minor_breaks,format,domain See [scales::trans_new()].
#'
#' @export
#'
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

#' @rdname trans_discrete_new
#' @export
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

#' Don't rescale
#'
#' This rescaler just returns its input, as opposed to
#' [scales::rescale()], [scales::rescale_max()], and [scales::rescale_mid()],
#' all of which return values between 0 and 1.
#'
#' @param x A vector of values.
#' @param ... Used to accept any other parameters.
#'
#' @return `x` unchanged
#' @export
#'
#' @examples
#' scales::rescale(1:5)
#' rescale_none(1:5)
#'
rescale_none <- function(x, ...) {
  x
}

#' Keep out-of-bounds values
#'
#' This out-of-bounds function returns its input unchanged,
#' and is arguably the opposite of [scales::discard()]. Other useful
#' out-of-bounds functions include [scales::censor()], [scales::squish()],
#' [scales::squish_infinite()], and [scales::discard()].
#'
#' @inheritParams rescale_none
#'
#' @return `x` unchanged.
#' @export
#'
#' @examples
#' oob_keep(1:5)
#'
oob_keep <- function(x, ...) {
  x
}

#' Censor discrete values
#'
#' The out-of-bounds functions in the [scales][scales::scales]
#' package only work with continuous ranges. This oob function
#' works on discrete values.
#'
#' @param x A vector of values.
#' @param range A character vector of values that should be kept.
#'
#' @return `x` with values not in `range` as `NA`.
#' @export
#'
#' @examples
#' censor_discrete(c("a", "b", "c"), range = c("a", "b"))
#'
censor_discrete <- function(x, range) {
  na_value <- vctrs::vec_cast(NA, x)
  ifelse(x %in% range, x, na_value)
}

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}
