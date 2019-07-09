
not_implemented <- function(message = "Not implemented") {
  abort(message, trace = rlang::trace_back(bottom = 1), .subclass = "not_implemented")
}

invalid_state <- function(message = "Invalid state") {
  abort(message, trace = rlang::trace_back(bottom = 1), .subclass = "invalid_state")
}

function_or_value <- function(fun_or_value, ...) {
  if (is.function(fun_or_value)) {
    fun_or_value(...)
  } else {
    fun_or_value
  }
}

waiver <- function() structure(list(), class = "waiver")

is_waive <- function(x) inherits(x, "waiver")

`%|W|%` <- function(a, b) {
  if (!is_waive(a)) a else b
}

trans_discrete_new <- function(..., breaks = identity,
                               minor_breaks = function(breaks, limits, n) NULL,
                               domain = c(-Inf, Inf)) {
  scales::trans_new(
    ...,
    breaks = breaks,
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

discrete_rev_trans <- function() {
  trans_discrete_new(
    "discrete_rev",
    transform = rev,
    inverse = rev
  )
}

rescale_none <- function(x, ...) {
  x
}

oob_keep <- function(x, ...) {
  x
}
