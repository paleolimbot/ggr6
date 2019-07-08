
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
