
r6doc <- function(class_name, super = TRUE) {
  r6doc_class(get(class_name, envir = environment()), super = super)
}

r6doc_class <- function(Class, super = TRUE) {
  if (!R6::is.R6Class(Class)) {
    abort("Can't create R6 documentation from something that isn't an R6 Class")
  }

  methods_df <- r6methods(Class, super = super)

  classname <- purrr::map_chr(methods_df$class, "classname")
  name <- methods_df$method_name
  usage <- purrr::map2(name, methods_df$class, r6usage)
  docstring <- purrr::map2(name, methods_df$class, function(name, Cls) {
    r6docstring(Cls$public_methods[[name]])
  })

  sections <- glue::glue(
    normalize_whitespace(
      "
      \\code{{{usage}}}

      {docstring}
      "
    )
  )

  paste0(sections, collapse = "\n\n")
}

r6methods <- function(Class, super = TRUE) {
  methods <- tibble(
    class = list(Class),
    method_name = names(Class$public_methods)
  )

  Super <- Class$get_inherit()

  if (super && !is.null(Super)) {
    super_methods <- r6methods(Super)
    super_methods <- super_methods[!(super_methods$method_name %in% methods$method_name), ]
    dplyr::bind_rows(methods, super_methods)
  } else {
    methods
  }
}

r6docstring <- function(fun) {
  bdy <- body(fun)
  if (rlang::is_call(bdy, "{") && is.character(bdy[[2]]) && length(bdy) > 2) {
    normalize_whitespace(bdy[[2]])
  } else {
    ''
  }
}

r6usage <- function(method_name, Class) {
  if (!R6::is.R6Class(Class)) {
    abort("Can't create R6 method usage from something that isn't an R6 Class")
  }

  fun <- Class$public_methods[[method_name]]
  if (is.null(fun)) {
    abort(sprintf("No such method: %s$%s", Class$classname, method_name))
  }

  call <- rlang::call2(
    "$", rlang::sym(Class$classname),
    rlang::call2(method_name, !!!formals(fun))
  )

  paste0(format(call), collapse = "\n")
}

normalize_whitespace <- function(docstring) {
  docstring <- paste0(docstring, collapse = "\n")
  lines <- strsplit(docstring, "\n")[[1]]
  if (length(lines) >= 2) {
    leading_whitespace <- gsub("^( +).*", "\\1", lines)
    n_leading <- min(nchar(leading_whitespace)[-1])
    lines <- gsub(paste0("^\\s{", n_leading, "}"), "", lines)
  }

  lines <- gsub("\\s+$", "", lines)

  lines <- lines[!((lines == "") & (seq_along(lines) %in% c(1, length(lines))))]
  paste0(lines, collapse = "\n")
}
